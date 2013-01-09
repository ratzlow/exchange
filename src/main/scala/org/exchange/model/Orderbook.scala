package org.exchange.model

import collection.mutable

/**
 * Keep the orders for a given security. It contains order from buy and sell side which are to be matched against
 * each other.
 *
 * This class is stateful and thus not thread safe.
 *
 * @param isin ID of security
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
class Orderbook( isin: String ) {
  require( !isin.isEmpty )

  private val buyOrders = mutable.MutableList[Order]()
  private val sellOrders = mutable.MutableList[Order]()

  /**
   * Add order to orderbook's buy- or sell orders.
   *
   * @param order that should be listed in orderbook
   */
  def +=( order: Order ) {
    require( order != null )
    require( order.getSide != null )
    require( order.getIsin == isin )

    val side: Side.Side = order.getSide
    if ( side == Side.BUY ) buyOrders += order
    else if ( side == Side.SELL ) sellOrders += order
    else throw new OrderbookException("Unknown order side!")
  }


  //
  // ro access to orderbook state
  //

  def getBuyOrders = buyOrders.toList
  def getSellOrders = sellOrders.toList
}
