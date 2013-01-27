package org.exchange.matching

import org.scalatest.FunSuite
import org.exchange.model.{Execution, Side, Order, Orderbook}

/**
 * Calculate matching price based on current order book. This test expects all orders to be limit orders even though
 * no execution type as such is specified yet for the order.
 */
class MatchEngineTest extends FunSuite {

  private val isin: String = "CocaCola"

  test("exactly buy limit. Sizes on both sides are equal so all orders should be matched.") {
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

    val matchResult = matchOrders(orderbook)

    val executions: List[Execution] = matchResult.executions
    val executedSize = executions.foldLeft(0)(_ + _.executionSize)
    expect( expectedOrderbookSize)( executedSize )

    // TODO (FRa) : (FRa) : any sell way to check this predicate is true; forall() doesn't show the failed elem
    assert( matchResult.orderbook.buyOrders.isEmpty)
    assert( matchResult.orderbook.sellOrders.isEmpty)
  }


  test("Not all orders can be matched since buy side has a surplus. The highest limit will be considered") {
    val orderbook = new Orderbook(isin)
    val expectedOrderbookBuySize = 600
    val expectedOrderbookSellSize = 500

    // Bid/Buy side
    orderbook += newBuy(400, 202)
    orderbook += newBuy(200, 201)

    // Ask/Sell side
    orderbook += newSell(300, 199)
    orderbook += newSell(200, 198)
    expect(expectedOrderbookBuySize)( orderbook.buyOrders.foldLeft(0)( _ + _.orderQty) )
    expect(expectedOrderbookSellSize)( orderbook.sellOrders.foldLeft(0)( _ + _.orderQty) )

    val matchResult = matchOrders(orderbook)
    val balancedOrderbook = matchResult.orderbook
    val expectedSurplus = expectedOrderbookBuySize - expectedOrderbookSellSize

    expect(0) {balancedOrderbook.sellOrders.size}
    expect(1) {balancedOrderbook.buyOrders.size}
    val surplusOrder: Order = balancedOrderbook.buyOrders.head
    expect(expectedSurplus)(surplusOrder.openQty)
    expect(BigDecimal(201))(surplusOrder.price)
  }




  def matchOrders(orderbook: Orderbook) : MatchResult = {
    // match the orders, orders are removed from orderbook
    val matchEngine = MatchEngine()
    val matchResult: MatchResult = matchEngine.balance(orderbook)

    assert( !matchResult.executions.isEmpty)
    assert( matchResult.orderbook != Unit )
    matchResult
  }

  private def assertEqualOrderbookSizes(orderbook: Orderbook) : Int = {
    // TODO (FRa) : (FRa) : this looks strange, should be rather buy val assignment
    val buyBookSize = orderbook.buyOrders.foldLeft(0)( _ + _.orderQty)
    val sellBookSize = orderbook.sellOrders.foldLeft(0)(_ + _.orderQty)

    assert(buyBookSize === sellBookSize)

    buyBookSize
  }

  private def newBuy(size: Int, price: Int) = new Order(Side.BUY, size, BigDecimal(price), isin)
  private def newSell(size: Int, price: Int) = new Order(Side.SELL, size, BigDecimal(price), isin)
}
