package org.exchange.model

import org.scalatest.FunSuite

class ExecutionTest extends FunSuite {

  test("Check constraints for execution. Try to overfill it...") {
    val isin: String = "CoCa"
    val buyOrder = new Order(Buy, Limit, 20, BigDecimal(100), isin)

    val sellOrder: Order = new Order(Sell, Limit, orderQty = 30, price = BigDecimal(100), isin = isin)
    assert(buyOrder.orderQty < sellOrder.orderQty)

    // adding another/same order that exceeds the execution orderQty will lead in an error
    intercept[IllegalArgumentException] {
      Execution(buyOrder, sellOrder, 40)
    }
  }
}
