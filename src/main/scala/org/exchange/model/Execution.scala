package org.exchange.model

import com.typesafe.scalalogging.slf4j.{Logging, Logger}


/**
 * The orders being matched. The executionSize represents the number of shares to reduce on both
 * sides.
 *
 * @author fratzlow
 * @since 2012-12-31
 *
 * @param buy buy side order
 * @param sell sell side order
 */
case class Execution(buy: Order, sell: Order, executionSize : Int) extends Logging {
  require( buy.side == Side.BUY )
  require( sell.side == Side.SELL )
  logger.debug( buy.toString + " <==> " + sell + " execSize = " + executionSize)
  require( buy.openQty > 0 && sell.openQty > 0 && executionSize > 0)
  assertSufficientSize(buy)
  assertSufficientSize(sell)

  private def assertSufficientSize(order: Order) {
    require( order.openQty >= executionSize, order.side + ".openyQty = " + order.openQty + " execSize = " + executionSize )
  }

  override def toString : String = { "buy = " + buy + " <==> " + sell + " @#"  + executionSize }
}