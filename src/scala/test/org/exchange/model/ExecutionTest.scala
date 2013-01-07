package org.exchange.model

import org.scalatest.FunSuite

class ExecutionTest extends FunSuite {

   test("Check constraints for execution. Try to overfill it...") {
     val isin: String = "CoCa"
     val buyOrder = new Order(Side.BUY, 20, BigDecimal(100), isin)

     val sellOrder: Order = new Order(Side.SELL, size = 30, price = BigDecimal(100), isin)
     assert( buyOrder.getSize < sellOrder.getSize )

     val execution = new Execution(buyOrder)
     // adding another/same order that exceeds the execution size will lead in an error
     intercept[OrderbookException] {
       execution += sellOrder
     }
   }
 }
