package org.exchange.matching

import org.exchange.model.{MatchingException, Orderbook, Order, Execution}

/**
 * The result container of a balancing operation. If orders could be matched they will be added to the previousExecutions,
 * otherwise to the unmatchable orders.
 *
 * @author ratzlow@gmail.com
 * @since 2013-01-12
 *
 * @param executions of the orders that could be matched
 */
case class MatchResult_1(orderbook: Orderbook, executions: List[Execution] = Nil) {

  private val buySurplus = orderbook.buyOrders.foldLeft(0)( _ + _.openQty)
  private val sellSurplus = orderbook.sellOrders.foldLeft(0)( _ + _.openQty)
  private val isBuySurplusOnly = buySurplus > 0 && sellSurplus == 0
  private val isSellSurplusOnly = sellSurplus > 0 && buySurplus == 0

  // prices make up the prices closest to the market
  private val minExecutableBuyLimit = if ( !executions.isEmpty ) executions.last.buy.price else BigDecimal(0)
  private val maxExecutableSellLimit = if ( !executions.isEmpty ) executions.last.sell.price else BigDecimal(0)


  def auctionPrice(referencePrice: BigDecimal = 0) : BigDecimal = {


    // if we have a buy surplus the other market side will get the best price
    // -> so best price for sell side is considered
    var price: BigDecimal = 0

    val isOneSideSurplus = isSellSurplusOnly || isBuySurplusOnly
    val isBothSideSurplus = buySurplus > 0 && sellSurplus > 0
    val noSurplus = buySurplus == 0 && sellSurplus == 0
    // TODO (FRa) : (FRa) : consider exactly one limit

    if ( isOneSideSurplus ) {
      price = auctionPriceBySurplus()

    // surplus on both sides -> consider reference price
    } else if ( isBothSideSurplus ) {
      price = auctionPriceByClosestLimit(referencePrice, orderbook.buyOrders.map(_.price), orderbook.sellOrders.map(_.price))

    // no surplus := all orders executed -> last execution (head) is most far away from market so this price will be considered
    } else if ( noSurplus ) {
      price = auctionPriceByClosestLimit( referencePrice, executions.map(_.buy.price), executions.map(_.sell.price) )

    }

    price
  }

  private def auctionPriceBySurplus() : BigDecimal = {

    // buy surplus := is an ask market -> ask price will be improved
    if (isBuySurplusOnly) {
      // take buy limit that is up next highest sell limit
      val upNextBuyLimit = executions.map(_.buy.price).sorted.collectFirst({
        case(limit) if( limit > maxExecutableSellLimit) => limit })
      upNextBuyLimit.get

    // ask surplus := is a buy market -> bid price will be improved
    } else if ( isSellSurplusOnly) {
      // take sell limit that is down next to lowest buy limit
      val downNextBuyLimit = executions.map(_.sell.price).sorted.reverse.collectFirst({
        case(limit) if( limit < minExecutableBuyLimit) => limit })
      downNextBuyLimit.get

    } else throw new MatchingException("Invalid auction price determination approach. Surplus is not sufficient!")
  }


  /**
   * If orders do not cross check which unmatched limit order's price is closest to given reference price.
   *
   * @param referencePrice orientation point to derive the auction price for limit order that is nearest to it
   * @return auction price
   */
  private def auctionPriceByClosestLimit(referencePrice: BigDecimal,
                                         buyLimits: List[BigDecimal],
                                         sellLimits: List[BigDecimal]): BigDecimal = {
    require(referencePrice != 0, "Need reference price for orientation!")

    // prices most away from market but still executable
    val consideredLimits: List[BigDecimal] = buyLimits ::: sellLimits
    val minUnmatchedLimit: BigDecimal = consideredLimits.min
    val maxUnmatchedLimit: BigDecimal = consideredLimits.max

    val isRefPriceWithinLimits = referencePrice >= minUnmatchedLimit && referencePrice <= maxUnmatchedLimit
    // is there a limit equal to the referencePrice?
    var price: BigDecimal = 0
    if (isRefPriceWithinLimits) {

      val equalLimitRef = consideredLimits.collectFirst({
        case (limit: BigDecimal) if (limit == referencePrice) => referencePrice
      })

      if (equalLimitRef.isDefined) {
        price = equalLimitRef.get

        // approximate to given limit
      } else {

        // refPrice is exactly in the middle of min/max limits -> take highest possible limit
        val limitSpanwide: BigDecimal = (maxUnmatchedLimit - minUnmatchedLimit).abs
        if (limitSpanwide > 0 && limitSpanwide / 2 == referencePrice) {
          price = maxUnmatchedLimit

          // auction price is limit closest to refPrice
        } else if (limitSpanwide > 0 && limitSpanwide / 2 != referencePrice) {

          val closestLimitBuyPrice = buyLimits.minBy({ (p) => (p - referencePrice).abs })
          val closestLimitSellPrice = sellLimits.minBy({ (p) => (p - referencePrice).abs })
          val buyDelta: BigDecimal = (referencePrice - closestLimitBuyPrice).abs
          val sellDelta: BigDecimal = (referencePrice - closestLimitSellPrice).abs

          price =
            if (buyDelta < sellDelta) closestLimitBuyPrice
            else if (sellDelta < buyDelta) closestLimitSellPrice
            else closestLimitBuyPrice.max(closestLimitBuyPrice)
        }
      }
    }

    price
  }
}