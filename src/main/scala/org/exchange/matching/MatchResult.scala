package org.exchange.matching

import org.exchange.model.{Orderbook, Order, Execution}

/**
 * The result container of a balancing operation. If orders could be matched they will be added to the previousExecutions,
 * otherwise to the unmatchable orders.
 *
 * @author ratzlow@gmail.com
 * @since 2013-01-12
 *
 * @param executions of the orders that could be matched
 */
case class MatchResult(orderbook: Orderbook, executions: List[Execution] = Nil) {


  def auctionPriceHighestLimit : BigDecimal = {
    auctionPrice { (e: Execution) => e.buy.price > e.sell.price }
  }

  def auctionPriceLowestLimit : BigDecimal = {
    auctionPrice { (e: Execution) => e.buy.price < e.sell.price }
  }

  /**
   * If orders do not cross check which unmatched limit order's price is closest to given reference price.
   *
   * @param referencePrice orientation point to derive the auction price for limit order that is nearest to it
   * @return auction price
   */
  def auctionPrice(referencePrice: BigDecimal): BigDecimal = {
    require( referencePrice != 0, "Need reference price for orientation!")
    require( executions.isEmpty, "Orders have been matched. Derive auction price from executions!")

    val closestLimitBuyPrice = minDeltaLimit( referencePrice, orderbook.buyOrders )
    val closestLimitSellPrice = minDeltaLimit( referencePrice, orderbook.sellOrders )

    if ( (referencePrice - closestLimitBuyPrice).abs <= (referencePrice - closestLimitSellPrice).abs )
      closestLimitBuyPrice
    else closestLimitSellPrice
  }

  //
  // inner impl
  //

  private def minDeltaLimit( referencePrice: BigDecimal, orders: List[Order] ) : BigDecimal = {
    orders match {
      case List() => BigDecimal(0)
      case _ => orders.minBy { (o: Order) => (o.price - referencePrice).abs }.price
    }
  }

  private def auctionPrice(compare: (Execution) => Boolean): BigDecimal = {
    executions match {
      case e :: rest => if (compare(e)) e.buy.price
                        else e.sell.price
      case _ => BigDecimal(0)
    }
  }
}