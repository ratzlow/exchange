package org.exchange.model

import Side._
import OrderType._

/**
 * The order submitted by the trader. An order of buy side needs to be matched against order(s) of the sell side.
 *
 * @param side ... @see Side (FIX:54)
 * @param orderQty ... number of shares (FIX:38)
 * @param price ... the price
 * @param isin ... instrument identifier
 * @param cummulatedQty ... Total quantity (e.g. number of shares) filled. (FIX:14)
 * @param orderType ... how to execute an order (FIX: 40)
 */
case class Order( side: Side, orderType: OrderType = OrderType.LIMIT,
                  orderQty: Int, price: BigDecimal, isin: String, cummulatedQty: Int = 0 ) {
  /**
   * @param executedShareSize number of shares executed
   * @return updated Order with increased filled fullSize
   */
  def +=( executedShareSize: Int ) : Order = this.copy(cummulatedQty = this.cummulatedQty + executedShareSize)

  def openQty : Int = orderQty - cummulatedQty
}

object Order {
  def newBuy(orderQty: Int, price: BigDecimal, isin: String) : Order = new Order(Side.BUY, OrderType.LIMIT, orderQty, price, isin, 0)
}



