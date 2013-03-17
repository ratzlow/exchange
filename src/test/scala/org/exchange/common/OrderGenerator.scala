package org.exchange.common

import org.scala_tools.time.Imports._
import org.exchange.model.{OrderType, Order, Side}
import org.exchange.model.Side._

/**
 * Factory to create test orders that can be added to the book.
 *
 * @author ratzlow@gmail.com
 * @since 2013-03-17
 */
case class OrderGenerator(isin: String) {

  def newSell(size: Int, price: BigDecimal) = new Order(Side.SELL, orderQty = size, price = price, isin = isin)

  def newBuy(size: Int, price: BigDecimal) = new Order(Side.BUY, orderQty = size, price = price, isin = isin)
}


object OrderGenerator {

  def newBuy(size: Int, price: BigDecimal, timestamp: DateTime) = newOrder(Side.BUY, size, price, timestamp)

  def newSell(size: Int, price: BigDecimal, timestamp: DateTime) = newOrder(Side.SELL, size, price, timestamp)


  //
  // internal impl
  //

  private def newOrder(side: Side, size: Int, price: BigDecimal, timestamp: DateTime = DateTime.now) =
    new Order(side, OrderType.LIMIT, size, price, "CoCa", timestamp = timestamp)
}
