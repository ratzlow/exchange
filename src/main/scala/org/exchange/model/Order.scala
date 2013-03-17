package org.exchange.model

import Side._
import OrderType._
import org.scala_tools.time.Imports._

/**
 * The order submitted by the trader. An order of buy side needs to be matched against order(s) of the sell side.
 *
 * @param side ... @see Side (FIX:54)
 * @param orderQty ... number of shares (FIX:38)
 * @param price ... the limit price (FIX:44)
 * @param isin ... instrument identifier
 * @param cummulatedQty ... Total quantity (e.g. number of shares) filled. (FIX:14)
 * @param orderType ... how to execute an order (FIX: 40)
 * @param timestamp ... when order was added to orderbook
 */
case class Order( side: Side, orderType: OrderType = OrderType.LIMIT,
                  orderQty: Int, price: BigDecimal, isin: String, cummulatedQty: Int = 0,
                  timestamp: DateTime = DateTime.now) {

  /**
   * In the case of partial execution part of the order is not yet filled, so left open for further execution or until
   * it is remove from the book.
   */
  private val openQuantiy = orderQty - cummulatedQty

  //
  // public API
  //

  /**
   * @param executedShareSize number of shares executed
   * @return updated Order with increased filled size
   */
  def +=( executedShareSize: Int ) : Order = this.copy(cummulatedQty = this.cummulatedQty + executedShareSize)

  def openQty : Int = openQuantiy
}

object Order {

  def apply(side: Side, orderType: OrderType, orderQty: Int, isin: String) : Order = {
    require(orderType != LIMIT && orderType != STOP_LIMIT, "Must not be an order of limit type!")
    new Order(side, orderType, orderQty, BigDecimal(0), isin, 0)
  }
}



