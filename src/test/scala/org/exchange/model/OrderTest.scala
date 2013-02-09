package org.exchange.model

import org.scalatest.FunSuite

class OrderTest extends FunSuite {
  private val isin: String = "CoCa"


  test("buy and sell order creation") {
      val buyOrder = new Order(Side.BUY, OrderType.LIMIT, 20, BigDecimal(100), isin)
      assert( buyOrder.side == Side.BUY )

      val sellOrder = Order(Side.SELL, OrderType.LIMIT, 20, BigDecimal(100), isin)
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
        orderBook += new Order(Side.BUY, OrderType.LIMIT, 1, BigDecimal(1), "wrongIsin")
      }
    }
}
