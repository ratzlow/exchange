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
     executions match {
       case e :: rest => if (e.buy.price > e.sell.price) e.buy.price
                         else e.sell.price
       case _ => BigDecimal(0)
     }
  }
}
