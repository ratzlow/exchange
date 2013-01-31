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
case class MatchResult(orderbook: Orderbook,
                       executions: List[Execution] = Nil) {


  def auctionPriceHighestLimit : BigDecimal = {
    auctionPrice { (e: Execution) => e.buy.price > e.sell.price }
  }

  def auctionPriceLowestLimit : BigDecimal = {
    auctionPrice { (e: Execution) => e.buy.price < e.sell.price }
  }

  private def auctionPrice(compare: (Execution) => Boolean): BigDecimal = {
    executions match {
      case e :: rest => if (compare(e)) e.buy.price
                        else e.sell.price
      case _ => BigDecimal(0)
    }
  }
}
