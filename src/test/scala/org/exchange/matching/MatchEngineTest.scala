package org.exchange.matching

import auction.{AuctionResult, Auction}
import org.scalatest.{GivenWhenThen, FunSuite}
import org.exchange.model._
import org.exchange.model.Orderbook
import java.util.{GregorianCalendar, Calendar, Date}

/**
 * Calculate matching price based on current order book. This test expects all orders to be limit orders even though
 * no execution type as such is specified yet for the order.
 * Since matching for an auction is executed time prio doesn't account.
 *
 * @author fratzlow
 */
class MatchEngineTest extends FunSuite with GivenWhenThen {

  private val isin: String = "CocaCola"

  test("1) Exactly one buy limit. Sizes on both sides are equal so all orders should be matched.") {
    val orderbook = new Orderbook(isin)

    // Bid/Buy side
    orderbook += newBuy(200, 202)
    orderbook += newBuy(200, 201)
    orderbook += newBuy(300, 200)

    // Ask/Sell side
    orderbook += newSell(400, 197)
    orderbook += newSell(200, 198)
    orderbook += newSell(100, 200)

    val balancing: AuctionResult = Auction(orderbook).conduct()

    expectResult(200)(balancing.auctionPrice.get)
    expectResult(700)(balancing.executableQuantity)
    // TODO (FRa) : (FRa) : => assert orderbook is empty!
    expectResult(0)(balancing.askSurplus)
    expectResult(0)(balancing.bidSurplus)
  }

  test("2) Not all orders can be matched since buy side has a surplus. The highest limit will be considered") {
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

    val matchResult = Auction(orderbook).conduct()

    expectResult(201)(matchResult.auctionPrice.get)
    expectResult(100)(matchResult.bidSurplus)
    expectResult(0)(matchResult.askSurplus)
    expectResult(500)(matchResult.executableQuantity)
    expectResult(None)(matchResult.referencePrice)
  }

  test("3) There are several possible limits and there is a surplus on the ask. Opposite case to 2)") {
    val orderbook = new Orderbook(isin)
    val expectedOrderbookBuySize = 500
    val expectedOrderbookSellSize = 600

    // Bid/Buy side
    orderbook += newBuy(300, 202)
    orderbook += newBuy(200, 201)

    // Ask/Sell side
    orderbook += newSell(400, 199)
    orderbook += newSell(200, 198)
    expectResult(expectedOrderbookBuySize)( orderbook.buyOrders.foldLeft(0)( _ + _.orderQty) )
    expectResult(expectedOrderbookSellSize)( orderbook.sellOrders.foldLeft(0)( _ + _.orderQty) )

    val matchResult = Auction(orderbook).conduct()

    expectResult(199)(matchResult.auctionPrice.get)
    expectResult(0)(matchResult.bidSurplus)
    expectResult(100)(matchResult.askSurplus)
    expectResult(500)(matchResult.executableQuantity)
    expectResult(None)(matchResult.referencePrice)
  }

  /**
   * market orders are not considered to determine the auction price
   * limit orders don't cross
   */
  test("4) There are several possible limits and there is both an ask and bid surplus") {
    val orderbook = new Orderbook(isin)
    val marketBuy: Order = new Order(Side.BUY, OrderType.MARKET, 100, 0, isin)
    val marketSell: Order = new Order(Side.SELL, OrderType.MARKET, 100, 0, isin)
    val limitBuy: Order = new Order(Side.BUY, OrderType.LIMIT, 100, 199, isin)
    val limitSell: Order = new Order(Side.SELL, OrderType.LIMIT, 100, 202, isin)

    orderbook += marketBuy
    orderbook += limitBuy

    orderbook += marketSell
    orderbook += limitSell

    expectResult(199){Auction(orderbook).conduct(Option(199)).auctionPrice.get}
    expectResult(199){Auction(orderbook).conduct(Option(200)).auctionPrice.get}
    expectResult(202){Auction(orderbook).conduct(Option(201)).auctionPrice.get}
    expectResult(202){Auction(orderbook).conduct(Option(202)).auctionPrice.get}
  }


  test("7a) There is no elible limit as there are only orders in the book which are not executable") {
    val orderbook = new Orderbook(isin)
    Given("Orders in book are not crossing")
    orderbook += new Order(Side.BUY, OrderType.HIDDEN, 80, 200, isin)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 80, 199, isin)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 80, 201, isin)

    When("The auction is conducted")
    val conducted: AuctionResult = Auction(orderbook).conduct()

    expectResult(None){conducted.auctionPrice}
    Then("No auction price can be derived so only highest visible bid limit")
    expectResult(199){conducted.highestVisibleBidLimit.get}

    And("lowest visible ask limit are published")
    expectResult(201){conducted.lowestVisibleAskLimit.get}

    And("Cummulated quantities of either side are the same before and after the auction")
    val cumQty = (orders: List[Order]) => orders.foldLeft(0)(_ + _.orderQty)
    expectResult( cumQty(orderbook.buyOrders))( cumQty(conducted.orderbook.buyOrders) )
    expectResult( cumQty(orderbook.sellOrders))( cumQty(conducted.orderbook.sellOrders) )
  }


  test("7b) partial execution of an order within the opening auciton") {
    Given("The auction is opened with an auction price and orders need to be matched. " +
          "Time priority is used to execute one fully and one partially")
    val orderbook = new Orderbook(isin)
    orderbook += new Order( Side.BUY, OrderType.LIMIT, 300, 200, isin, timestamp = createTimestamp(9, 0))
    orderbook += new Order( Side.BUY, OrderType.LIMIT, 300, 200, isin, timestamp = createTimestamp(9, 1))
    orderbook += new Order( Side.SELL, OrderType.LIMIT, 400, 200, isin, timestamp = createTimestamp(9, 0))

    When("BUY orders have different timestamp and same price")

    Then("earlier order must have precedence and gets fully executed")


    When("Second buy order only gets partially executed")
    Then("Remaining shares will be transfered into continous trading if order is not restricted to auctions only?")
  }

  // TODO (FRa) : (FRa) : use implicite?!
  private def createTimestamp(hour: Int, min: Int) : Date = {
    val cal: Calendar = new GregorianCalendar(2013, Calendar.FEBRUARY, 18, hour, min)
    cal.getTime
  }


  private def newBuy(fullSize: Int, price: Int) = new Order(side = Side.BUY, orderQty = fullSize, price = price, isin = isin)
  private def newSell(size: Int, price: Int) = new Order(side = Side.SELL, orderQty = size, price = price, isin = isin)
}
