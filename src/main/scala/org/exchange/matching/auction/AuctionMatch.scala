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

  /**
   * @return true ... the orderbook contains at least one order that has a limit price set that could be used to
   *         determine an auction price
   */
  val isLimitBasedMatch : Boolean = {
    executions.exists( (e) => isLimitOrder(e.buy) || isLimitOrder(e.sell)) ||
    orderbook.buyOrders.exists( isLimitOrder(_)) ||
    orderbook.sellOrders.exists( isLimitOrder(_))
  }

  private def isLimitOrder(order: Order) : Boolean =
    order.orderType == OrderType.LIMIT || order.orderType == OrderType.STOP_LIMIT
}
