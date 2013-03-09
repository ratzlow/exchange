package org.exchange.model


/**
 * Keep the orders for a given security. It contains order from buy and sell side which are to be matched against
 * each sell.
 *
 * This class is stateful and thus not thread safe.
 *
 * @param isin ID of security
 * @param buyOrders bids that should be matched, by default empty list
 * @param sellOrders asks that should be matched, by default empty list
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
case class Orderbook( isin: String, var buyOrders: List[Order] = List.empty, var sellOrders: List[Order] = List.empty) {
  require( !isin.isEmpty )

  /**
   * Add order to orderbook's buy- or sell orders.
   *
   * @param order that should be listed in orderbook
   */
  def +=( order: Order ) {
    require( !Option(order).isEmpty )
    require( !Option(order.side).isEmpty )
    require( order.isin == isin )

    val side: Side.Side = order.side
    if ( side == Side.BUY ) buyOrders = order :: buyOrders
    else if ( side == Side.SELL ) sellOrders = order :: sellOrders
    else throw new OrderbookException("Unknown order side!")
  }
}
