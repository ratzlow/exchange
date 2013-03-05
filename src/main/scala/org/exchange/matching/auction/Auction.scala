package org.exchange.matching.auction

import org.exchange.model.{OrderType, Order, Orderbook}

import org.exchange.matching.engine.{MatchEngine, MatchResult, MatchEngineImpl}

/**
 * At a auction, participants place orders to buy or sell units at certain buying or selling prices. Orders collected
 * during a auction are matched to form a contract.
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-16
 */
class Auction(orderbook: Orderbook) {

  /**
   * true ... if the orderbook contains limit orders which can be used to derive a price.
   */
  private val isOrderbookWithLimitOrders = hasLimitOrders(orderbook)

  //
  // public API
  //

  /**
   * Execute an auction for given orderbook. This will determine an auction price and several quantities.
   *
   * @param referencePrice default is none
   * @return collecting parameter reflecting the result of the auction.
   */
  def conduct( referencePrice: Option[BigDecimal] = None ) : AuctionResult = {

    val possibleLimits: Set[BigDecimal] =
      ((orderbook.sellOrders.map( _.price ) ++ orderbook.buyOrders.map(_.price))).toSet

    // function literal that matches the order book for a given limit
    val matchByLimit = (limit: BigDecimal) => {
      val matchEngine = MatchEngine(Some(limit))
      val balanced: MatchResult = matchEngine.balance(orderbook)
      new AuctionMatch(limit, balanced.executions, balanced.orderbook)
    }

    // match on all limits greater zero -> side effect: populate limit:match map
    val auctions: Set[AuctionMatch] = possibleLimits.filter(_ > 0).map( matchByLimit(_) )

    create( referencePrice, auctions.toList )
  }

  //-----------------------------------------------------------------------------------
  // derive the parameters of the auction for given book an optional reference price
  //-----------------------------------------------------------------------------------

  private def create( referencePrice: Option[BigDecimal],
                      auctionMatches: List[AuctionMatch]) : AuctionResult = {

    // find single limit with highest executable quantity
    val bestAuctions: Set[AuctionMatch] = highestExecutionVolumes(referencePrice, auctionMatches)

    // reflect the decistion tree as outlined on Xetra Auctions chap 12
    val possibleLimits: Int = bestAuctions.size

    // no limit could be found -> only market orders
    if (possibleLimits == 0 ) {
      createByOrderTypeCheck( referencePrice, bestAuctions )

    } else if (possibleLimits == 1) {
      create(referencePrice, Some(bestAuctions.head))

    } else if (possibleLimits > 1) {
      createBySurplusComparison(referencePrice, bestAuctions)

    } else throw new AuctionException("Found invalid number of best auctions!")
  }


  private def createByOrderTypeCheck(referencePrice: Option[BigDecimal], bestAuctions: Set[AuctionMatch]) : AuctionResult = {

    referencePrice match {

      // we have an orderbook without crossing orders thus no executions
      case None if (isOrderbookWithLimitOrders) => {
        assert(bestAuctions.forall(_.executableVolume == 0),
          "Found case with executions in an auction so price could be derived!")

        val maxVisibleBidLimit = orderbook.buyOrders.filter(isLimitOrder(_)).maxBy(_.price).price
        val minVisibleAskLimit = orderbook.sellOrders.filter(isLimitOrder(_)).minBy(_.price).price
        new AuctionResult(orderbook, bidSurplus = 0, askSurplus = 0,
          lowestVisibleAskLimit = Some(minVisibleAskLimit), highestVisibleBidLimit = Some(maxVisibleBidLimit),
          executableQuantity = 0
        )
      }
      case Some(x) if (!isOrderbookWithLimitOrders) => {
        // TODO (FRa) : (FRa) : refactor! to conduct()
        val matchEngine = MatchEngine()
        val balanced: MatchResult = matchEngine.balance(orderbook)
        val auctionMatch = new AuctionMatch(referencePrice.get, balanced.executions, balanced.orderbook)
        create(referencePrice, Some(auctionMatch))
      }
      case _ => throw new AuctionException("Unhandled combination isOrderbookWithLimitOrders = " +
                  isOrderbookWithLimitOrders + " referencePrice = " + referencePrice)
    }
  }


  private def createByInLimitRangeCheck(referencePrice: BigDecimal,
                                maxBidSurplusAuction: AuctionMatch,
                                minAskSurplusAuction: AuctionMatch): AuctionResult = {

    // check if price is a possible limit -> use this limit as new auction price
    if (maxBidSurplusAuction.limit == referencePrice) {
      create( Some(referencePrice), Some(maxBidSurplusAuction) )

    } else if (minAskSurplusAuction.limit == referencePrice) {
      create( Some(referencePrice), Some(minAskSurplusAuction) )

    } else {

      val maxLimitOffset = (maxBidSurplusAuction.limit - referencePrice).abs
      val minLimitOffset = (minAskSurplusAuction.limit - referencePrice).abs

      if (maxLimitOffset < minLimitOffset) create(Some(referencePrice), Some(maxBidSurplusAuction))
      else if (maxLimitOffset > minLimitOffset) create(Some(referencePrice), Some(minAskSurplusAuction))
      // if refPrice is exactly between max and min limit -> take highest possible limit
      else create(Some(referencePrice), Some(maxBidSurplusAuction))
    }
  }


  private def createByRefPriceComparison(referencePrice: BigDecimal,
                                 maxBidSurplusAuction: AuctionMatch,
                                 minAskSurplusAuction: AuctionMatch): AuctionResult = {
    require( referencePrice > 0, "Reference price must be > 0: " + referencePrice)

    val minPossibleLimit: BigDecimal = minAskSurplusAuction.limit
    val maxPossibleLimit: BigDecimal = maxBidSurplusAuction.limit

    if ( referencePrice < maxPossibleLimit && referencePrice > minPossibleLimit ) {
      createByInLimitRangeCheck( referencePrice, maxBidSurplusAuction, minAskSurplusAuction)
    } else if ( referencePrice >= maxPossibleLimit ) {
      create( Some(referencePrice), Some(maxBidSurplusAuction) )
    } else if ( referencePrice <= minPossibleLimit ) {
      create( Some(referencePrice), Some(minAskSurplusAuction))
    } else throw new AuctionException("Found no surplus on either side! Invalid bounds of highest and lowest possible limit.")
  }


  private def createBySurplusComparison(referencePrice: Option[BigDecimal], auctionMatches: Set[AuctionMatch]) : AuctionResult = {
    require( auctionMatches.nonEmpty, "Cannot derive price if no valid auction is given!" )
    require( auctionMatches.forall( auctionMatches.head.executableVolume == _.executableVolume),
            "Volumes must be equal but might have different limit prices" )

    // for same executable volume find:
    // a) auction with highest limit on bid side
    val maxBidSurplusAuction = auctionMatches.groupBy(_.bidSurplus).maxBy(_._1)._2.maxBy(_.limit)
    // a) auction with lowest limit on ask side
    val minAskSurplusAuction = auctionMatches.groupBy(_.askSurplus).maxBy(_._1)._2.minBy(_.limit)

    if ( maxBidSurplusAuction.bidSurplus > 0 && minAskSurplusAuction.askSurplus == 0 ) {
      create(referencePrice, Some(maxBidSurplusAuction) )

    } else if ( maxBidSurplusAuction.bidSurplus == 0 && minAskSurplusAuction.askSurplus > 0 ) {
      create(referencePrice, Some(minAskSurplusAuction) )

    // surplus is not a distinct indicator
    } else if ( (maxBidSurplusAuction.bidSurplus > 0 && minAskSurplusAuction.askSurplus > 0) ||
                (maxBidSurplusAuction.bidSurplus == 0 && minAskSurplusAuction.askSurplus == 0) ) {
      createByRefPriceComparison( referencePrice.get, maxBidSurplusAuction, minAskSurplusAuction )

    } else throw new AuctionException("Found no surplus on either side! Should have been covered by 'possibleLimit == 1' case ")
  }


  private def create(referencePrice: Option[BigDecimal], bestAuction: Option[AuctionMatch]) = {
    bestAuction match {
      // if no match was provided the orderbook must have been unmodified so we can take input
      case None => new AuctionResult( orderbook, referencePrice, None, 0, 0, 0)
      // orderbook resulted in matches so take orderbook we unexecutable orders
      case Some(m) => new AuctionResult(m.orderbook, referencePrice, Some(m.limit),
                                        m.bidSurplus, m.askSurplus, m.executableVolume)
    }
  }

  /**
   * @return true ... the orderbook contains at least one order that has a limit price set that could be used to
   *         determine an auction price
   */
  private def hasLimitOrders(orderbook: Orderbook) : Boolean = {
    orderbook.buyOrders.exists( isLimitOrder(_)) ||
    orderbook.sellOrders.exists( isLimitOrder(_))
  }

  private def isLimitOrder(order: Order) : Boolean =
    order.orderType == OrderType.LIMIT || order.orderType == OrderType.STOP_LIMIT


  /**
   * @return auction with highest executable volume
   */
  def highestExecutionVolumes(referencePrice: Option[BigDecimal], auctions: List[AuctionMatch]) : Set[AuctionMatch] = {

    if ( auctions.isEmpty ) Set.empty
    else {

      val auctionsByExecutableVolue: Map[Int, List[AuctionMatch]] = referencePrice match {
        case Some(x: BigDecimal) => auctions.groupBy(_.executableVolume)
        case None => auctions.filter(_.executableVolume > 0).groupBy(_.executableVolume)
      }

      if ( auctionsByExecutableVolue.isEmpty) Set.empty
      else auctionsByExecutableVolue.maxBy(_._1)._2.toSet
    }
  }
}

object Auction {
  def apply(orderbook: Orderbook) = new Auction(orderbook)
}
