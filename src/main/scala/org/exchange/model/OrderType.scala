package org.exchange.model

/**
 * How to execute the order.
 *
 * FIX:40
 */
object OrderType extends Enumeration {
  type OrderType = Value
  val MARKET, LIMIT, STOP_LIMIT = Value
}
