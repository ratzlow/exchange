package org.exchange.matching

import org.scalatest.FunSuite
import org.exchange.model._
import org.exchange.model.Execution
import org.exchange.model.Orderbook

/**
 * Calculate matching price based on current order book. This test expects all orders to be limit orders even though
 * no execution type as such is specified yet for the order.
 * Since matching for an auction is executed time prio doesn't account.
 *
 * @author fratzlow
 */
class MatchEngineTest extends FunSuite {

  private val isin: String = "CocaCola"

  test("There are several possible limits and there is both an ask and bid surplus") {
    val orderbook = new Orderbook(isin)
    val marketBuy: Order = new Order(Side.BUY, OrderType.MARKET, 100, BigDecimal(0), isin)
    val marketSell: Order = new Order(Side.SELL, OrderType.MARKET, 100, BigDecimal(0), isin)
    val limitBuy: Order = new Order(Side.BUY, OrderType.LIMIT, 100, BigDecimal(199), isin)
    val limitSell: Order = new Order(Side.SELL, OrderType.LIMIT, 100, BigDecimal(202), isin)

    orderbook += marketBuy
    orderbook += limitBuy

    orderbook += marketSell
    orderbook += limitSell

    val matchEngine = MatchEngine()
    val result: MatchResult = matchEngine.balance(orderbook)

    // market orders are not considered to determine the auction price
    // limit orders don't cross
    expectResult(result.orderbook.buyOrders.size)(1)
    expectResult(result.orderbook.buyOrders.head)(limitBuy)

    expectResult(result.orderbook.sellOrders.size)(1)
    expectResult(result.orderbook.sellOrders.head)(limitSell)
    expectResult(result.executions.size)(0)


    // auction price will be the one with closest order limit
    expectResult( BigDecimal(199)){ result.auctionPrice(BigDecimal(199)) }
    expectResult( BigDecimal(199)){ result.auctionPrice(BigDecimal(200)) }
    expectResult( BigDecimal(202)){ result.auctionPrice(BigDecimal(201)) }
  }


  test("Exactly one buy limit. Sizes on both sides are equal so all orders should be matched.") {
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
    expectResult(expectedOrderbookSize)( assertEqualOrderbookSizes(orderbook) )

    val matchResult = matchOrders(orderbook)
    expectResult(200)(matchResult.auctionPriceHighestLimit)

    val executions: List[Execution] = matchResult.executions
    val executedSize = executions.foldLeft(0)(_ + _.executionSize)
    expectResult( expectedOrderbookSize)( executedSize )

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
    expectResult(expectedOrderbookBuySize)( orderbook.buyOrders.foldLeft(0)( _ + _.orderQty) )
    expectResult(expectedOrderbookSellSize)( orderbook.sellOrders.foldLeft(0)( _ + _.orderQty) )

    val matchResult = matchOrders(orderbook)
    val balancedOrderbook = matchResult.orderbook
    val expectedSurplus = expectedOrderbookBuySize - expectedOrderbookSellSize

    expectResult(201)(matchResult.auctionPriceHighestLimit)
    expectResult(199)(matchResult.auctionPriceLowestLimit)
    expectResult(0) (balancedOrderbook.sellOrders.size)
    expectResult(1) {balancedOrderbook.buyOrders.size}
    val surplusOrder: Order = balancedOrderbook.buyOrders.head
    expectResult(expectedSurplus)(surplusOrder.openQty)
    expectResult(BigDecimal(201))(surplusOrder.price)
  }


  private def matchOrders(orderbook: Orderbook) : MatchResult = {
    // match the orders, orders are removed from orderbook
    val matchEngine = MatchEngine()
    val matchResult: MatchResult = matchEngine.balance(orderbook)

    assert( !matchResult.executions.isEmpty)
    assert( Option(matchResult.orderbook).isDefined )
    matchResult
  }

  private def assertEqualOrderbookSizes(orderbook: Orderbook) : Int = {
    // TODO (FRa) : (FRa) : this looks strange, should be rather buy val assignment
    val buyBookSize = orderbook.buyOrders.foldLeft(0)( _ + _.orderQty)
    val sellBookSize = orderbook.sellOrders.foldLeft(0)(_ + _.orderQty)

    assert(buyBookSize === sellBookSize)

    buyBookSize
  }

  private def newBuy(fullSize: Int, price: Int) = new Order(side = Side.BUY, orderQty = fullSize, price = BigDecimal(price), isin = isin)
  private def newSell(size: Int, price: Int) = new Order(side = Side.SELL, orderQty = size, price = BigDecimal(price), isin = isin)
}
