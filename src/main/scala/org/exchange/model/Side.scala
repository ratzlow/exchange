package org.exchange.model

/**
 * The side on the orderbook.
 *
 * FIX:54
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
sealed trait Side

case object Buy extends Side

case object Sell extends Side
