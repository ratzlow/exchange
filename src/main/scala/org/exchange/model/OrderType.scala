package org.exchange.model

/**
 * How to execute the order.
 *
 * FIX:40
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
object OrderType extends Enumeration {
  type OrderType = Value
  val MARKET, LIMIT, STOP_LIMIT, HIDDEN = Value
}
