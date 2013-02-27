package org.exchange.matching.auction

import org.exchange.model.Orderbook

import org.exchange.matching.engine.{MatchResult, MatchEngineImpl}

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-16
 */
class Auction(orderbook: Orderbook) {

  // TODO (FRa) : (FRa) : perf imp: execute loop in parallel
  def conduct( referencePrice: Option[BigDecimal] = None ) : AuctionResult = {

    val possibleLimits: Set[BigDecimal] =
      ((orderbook.sellOrders.map( _.price ) ++ orderbook.buyOrders.map(_.price))).toSet

    // function literal that matches the order book for a given limit
    val matchByLimit = (limit: BigDecimal) => {
      val matchEngine = new MatchEngineImpl(Some(limit))
      val balanced: MatchResult = matchEngine.balance(orderbook)
      new AuctionMatch(limit, balanced.executions, balanced.orderbook)
    }

    // match on all limits greater zero -> side effect: populate limit:match map
    val auctions: Set[AuctionMatch] = possibleLimits.filter(_ > 0).map( matchByLimit(_) )

    create( referencePrice, auctions.toList )
  }

  //
  //------------------------------
  //

  private def create( referencePrice: Option[BigDecimal],
                      auctionMatches: List[AuctionMatch]) : AuctionResult = {

    // find single limit with highest executable quantity
    val auctionByLimit: AuctionByLimit = new AuctionByLimit(auctionMatches)
    val bestAuctions: Set[AuctionMatch] = auctionByLimit.highestExecutionVolumes

    // reflect the decistion tree as outlined on Xetra Auctions chap 12
    val possibleLimits: Int = bestAuctions.size

    // no limit could be found -> only market orders
    if (possibleLimits == 0 && !auctionMatches.isEmpty) {
      if ( referencePrice == None) throw new AuctionException("If no limit could be found a refPrice must be available!")
      else create(referencePrice, Some(auctionMatches.head))

    } else if (possibleLimits == 1) {
      create(referencePrice, Some(auctionByLimit.highestExecutionVolumes.head))

    } else if (possibleLimits > 1) {
      createBySurplusComparison(referencePrice, bestAuctions)

    } else throw new AuctionException("Found invalid number of best auctions!")
  }


  def createByInLimitRangeCheck(referencePrice: BigDecimal,
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

  def createByRefPriceComparison(referencePrice: BigDecimal,
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


  private case class AuctionByLimit(auctions: List[AuctionMatch]) {

    /**
     * @return auction with highest executable volume
     */
    val highestExecutionVolumes: Set[AuctionMatch] = {
      if ( auctions.filter(_.isLimitBasedMatch).isEmpty ) Set.empty
      else auctions.filter(_.isLimitBasedMatch).groupBy(_.executableVolume).maxBy(_._1)._2.toSet
    }
  }
}

object Auction {
  def apply(orderbook: Orderbook) = new Auction(orderbook)
}
