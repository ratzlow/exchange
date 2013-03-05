package org.exchange.model

/**
 * The side on the orderbook.
 *
 * FIX:54
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
object Side extends Enumeration {
  type Side = Value
  val BUY, SELL = Value
}
