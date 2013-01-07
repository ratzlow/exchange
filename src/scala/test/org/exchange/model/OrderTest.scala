package org.exchange.model

import org.scalatest.FunSuite

class OrderTest extends FunSuite {
  private val isin: String = "CoCa"

  test("buy and sell order creation") {
    val buyOrder = new Order(Side.BUY, 20, BigDecimal(100), isin)
    assert( buyOrder.getSide == Side.BUY )

    val sellOrder = new Order(Side.SELL, 20, BigDecimal(100), isin)
    assert( sellOrder.getSide == Side.SELL )

    val orderBook = new Orderbook( isin )
    orderBook += buyOrder
    orderBook += sellOrder

    assert(orderBook.getBuyOrders.size == 1)
    assert(orderBook.getBuyOrders.contains(buyOrder))

    assert(orderBook.getSellOrders.size == 1)
    assert(orderBook.getSellOrders.contains(sellOrder))
  }

  test("Checking preconditions") {
    val orderBook = new Orderbook(isin)
    intercept[IllegalArgumentException] {
      orderBook += new Order(Side.BUY, 1, BigDecimal(1), "wrongIsin")
    }
  }
}
