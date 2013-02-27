package org.exchange.matching.auction

import org.exchange.model.{Orderbook, Execution}

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-16
 */
case class AuctionMatch(limit: BigDecimal, executions: List[Execution], orderbook: Orderbook) {

  val bidSurplus = orderbook.buyOrders.foldLeft(0)(_ + _.openQty)
  val sellSurplus = orderbook.sellOrders.foldLeft(0)(_ + _.openQty)
  val executableVolume : Int = executions.foldLeft(0)(_ + _.executionSize)
}
