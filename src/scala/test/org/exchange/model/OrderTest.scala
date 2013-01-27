package org.exchange.model

import org.scalatest.FunSuite

class OrderTest extends FunSuite {
  private val isin: String = "CoCa"

  test("Instantiation of order") {
    val order: Order = Order.newBuy(100, BigDecimal(20), isin)
  }
/*  test("buy and sell order creation") {
      val buyOrder = new Order(Side.BUY, 20, BigDecimal(100), isin)
      assert( buyOrder.side == Side.BUY )

      val sellOrder = Order(Side.SELL, 20, BigDecimal(100), isin)
      assert( sellOrder.side == Side.SELL )

      val orderBook = Orderbook( isin )
      orderBook += buyOrder
      orderBook += sellOrder

      assert(orderBook.buyOrders.size == 1)
      assert(orderBook.buyOrders.contains(buyOrder))

      assert(orderBook.sellOrders.size == 1)
      assert(orderBook.sellOrders.contains(sellOrder))
    }

    test("Checking preconditions") {
      val orderBook = Orderbook(isin)
      intercept[IllegalArgumentException] {
        orderBook += new Order(Side.BUY, 1, BigDecimal(1), "wrongIsin")
      }
    }*/
}
