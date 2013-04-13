package org.exchange.common

import org.scala_tools.time.Imports._
import org.exchange.model._
import org.exchange.model.OrderType._
import util.Random
import org.exchange.model.OrderType

/**
 * Factory to create test orders that can be added to the book.
 *
 * @author ratzlow@gmail.com
 * @since 2013-03-17
 */
case class OrderGenerator(isin: String) {
  private final val random = new Random()

  def newSell(size: Int, price: BigDecimal) = new Order(Sell, orderQty = size, price = price, isin = isin)

  def newBuy(size: Int, price: BigDecimal) = new Order(Buy, orderQty = size, price = price, isin = isin)

  def newOrders(numberOfOrders: Int, side: Side, sizeRange: (Int, Int), priceRange: (Int, Int),
                orderTypes: OrderType.OrderType*): Seq[Order] = {

    require(!orderTypes.isEmpty)

    val _sizeRange = sizeRange._1 to sizeRange._2
    val _priceRange = priceRange._1 to priceRange._2
    for (i <- 0 until numberOfOrders;
         // pick random order type from given set
         orderType = orderTypes(random.nextInt(orderTypes.length)))
    yield OrderGenerator.newOrder(side,
      _sizeRange(random.nextInt(_sizeRange length)),
      _priceRange(random.nextInt(_priceRange length)))
  }
}

object OrderGenerator {

  def newBuy(size: Int, price: BigDecimal, timestamp: DateTime) = newOrder(Buy, size, price, timestamp = timestamp)

  def newSell(size: Int, price: BigDecimal, timestamp: DateTime) = newOrder(Sell, size, price, timestamp = timestamp)


  //
  // internal impl
  //

  private def newOrder(side: Side, size: Int, price: BigDecimal, orderType: OrderType = OrderType.LIMIT,
                       timestamp: DateTime = DateTime.now) =
    new Order(side, orderType, size, price, "CoCa", timestamp = timestamp)
}
