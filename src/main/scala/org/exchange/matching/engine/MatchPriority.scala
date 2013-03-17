package org.exchange.matching.engine

import org.exchange.model.Order

/**
 * Sorting algorithms that make up the order of Orders in the orderbook, so which one is closest to the market and gets
 * higher prio to be matched.
 *
 * @author ratzlow@gmail.com
 * @since 2013-03-16
 */
object MatchPriority {

  def sellPriceTimePriority(one: Order, other: Order): Boolean = {
    if (one.price < other.price) true
    else if (one.price > other.price) false
    else timePriority(one, other)
  }

  def sellPricePriority(one: Order, other: Order): Boolean = one.price < other.price

  def buyPricePriority(one: Order, other: Order): Boolean = one.price > other.price

  def buyPriceTimePriority(one: Order, other: Order): Boolean = {
    if (one.price > other.price) true
    else if (one.price < other.price) false
    else timePriority(one, other)
  }

  private def timePriority( one: Order, other: Order ) : Boolean = one.timestamp.isBefore(other.timestamp)
}