package org.exchange.model

/**
 * How to execute the order.
 *
 * FIX:40
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
sealed trait OrderType

case object Market extends OrderType

case object Limit extends OrderType

case object StopLimit extends OrderType

case object Hidden extends OrderType

