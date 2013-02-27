package org.exchange.matching.auction

import org.exchange.model.{Order, OrderType, Orderbook, Execution}

/**
 * TODO: comment
 *
 * @param limit used as the boundary for to determine good enough price
 * @param executions that could be performed on orderbook and references the orders contained in orderbook from before
 *                   matching
 * @param orderbook after matching containing the unmatched orders
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-16
 */
case class AuctionMatch(limit: BigDecimal, executions: List[Execution], orderbook: Orderbook) {

  val bidSurplus = orderbook.buyOrders.foldLeft(0)(_ + _.openQty)
  val askSurplus = orderbook.sellOrders.foldLeft(0)(_ + _.openQty)
  val executableVolume : Int = executions.foldLeft(0)(_ + _.executionSize)
}
