package org.exchange.matching.auction

import org.exchange.model.{Orderbook, Execution}

/**
 * The result of an auction on an orderbook. It contains the limit on which the auction was executed, the matches found
 * and the orderbook with orders that could not be sample matched.
 * This is not the final result of an auction but only the intermediate result to determine the auction price! It will
 * not consider matchable orders if they did not contribute to the price finding.
 *
 * @param limit used as the boundary for to determine good enough price
 * @param executions that could be performed on orderbook and references the orders contained in orderbook from before
 *                   matching
 * @param orderbook _after_ matching containing the unmatched orders, or orders that could not be used for price finding.
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-16
 */
case class AuctionMatch(limit: BigDecimal, executions: List[Execution], orderbook: Orderbook) {

  val bidSurplus = orderbook.buyOrders.foldLeft(0)(_ + _.openQty)
  val askSurplus = orderbook.sellOrders.foldLeft(0)(_ + _.openQty)
  val executableVolume : Int = executions.foldLeft(0)(_ + _.executionSize)
}
