package org.exchange.matching

import org.scalatest.FunSuite
import org.exchange.model.{Execution, Side, Order, Orderbook}

/**
 * Calculate matching price based on current order book. This test expects all orders to be limit orders even though
 * no execution type as such is specified yet for the order.
 */
class MatchEngineTest extends FunSuite {

  private val isin: String = "CocaCola"

  test("exactly one limit. Sizes on both sides are equal so all orders should be matched.") {
    val orderbook = new Orderbook(isin)
    val expectedOrderbookSize = 700

    // Bid/Buy side
    orderbook += newBuy(200, 202)
    orderbook += newBuy(200, 201)
    orderbook += newBuy(300, 200)

    // Ask/Sell side
    orderbook += newSell(400, 197)
    orderbook += newSell(200, 198)
    orderbook += newSell(100, 200)
    expect(expectedOrderbookSize)( assertEqualOrderbookSizes(orderbook) )

    val executions = matchOrders(orderbook)

    val executedSize = executions.foldLeft(0)(_ + _.getSize)
    expect( expectedOrderbookSize)( executedSize )

    // TODO (FRa) : (FRa) : any other way to check this predicate is true; forall() doesn't show the failed elem
    executions.foreach( e => assert(e.isFullyExecuted === true) )
  }


  test("Not all orders can be matched since one side has a surplus. The highest limit will be considered") {
    val orderbook = new Orderbook(isin)
    val expectedOrderbookBuySize = 600
    val expectedOrderbookSellSize = 500

    // Bid/Buy side
    orderbook += newBuy(400, 202)
    orderbook += newBuy(200, 201)

    // Ask/Sell side
    orderbook += newSell(300, 199)
    orderbook += newSell(200, 198)
    expect(expectedOrderbookBuySize)( orderbook.getBuyOrders.foldLeft(0)( _ + _.getSize) )
    expect(expectedOrderbookSellSize)( orderbook.getSellOrders.foldLeft(0)( _ + _.getSize) )

    val executions = matchOrders(orderbook)
    val expectedSurplus = expectedOrderbookBuySize - expectedOrderbookSellSize

    val partialExecution = executions.find( !_.isFullyExecuted ).get
    expect(expectedSurplus)(partialExecution.getOpenQty)
    expect(Side.BUY)(partialExecution.getOneOrder.getSide)
    expect(BigDecimal(201))(partialExecution.getOneOrder.getPrice)
  }




  def matchOrders(orderbook: Orderbook) : List [Execution] = {
    // match the orders, orders are removed from orderbook
    val matchEngine = new MatchEngine(orderbook)
    matchEngine.calc()

    val executions = matchEngine.getExecutions
    assert(!executions.isEmpty)
    executions
  }

  private def assertEqualOrderbookSizes(orderbook: Orderbook) : Int = {
    // TODO (FRa) : (FRa) : this looks strange, should be rather one val assignment
    val buyBookSize = orderbook.getBuyOrders.foldLeft(0)( _ + _.getSize)
    val sellBookSize = orderbook.getSellOrders.foldLeft(0)(_ + _.getSize)

    assert(buyBookSize === sellBookSize)

    buyBookSize
  }

  private def newBuy(size: Int, price: Int) = new Order(Side.BUY, size, BigDecimal(price), isin)
  private def newSell(size: Int, price: Int) = new Order(Side.SELL, size, BigDecimal(price), isin)
}
