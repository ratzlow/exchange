package org.exchange.matching.auction

import org.scalatest.{GivenWhenThen, FunSuite}
import org.exchange.model._
import org.exchange.model.Orderbook
import org.scala_tools.time.Imports._
import org.exchange.matching.engine.MatchResult
import org.exchange.common.OrderGenerator

/**
 * Calculate matching price based on current order book. This test expects all orders to be limit orders even though
 * no execution type as such is specified yet for the order.
 * Since matching for an auction is executed time prio doesn't account.
 *
 * @author fratzlow
 */
class AuctionMatchTest extends FunSuite with GivenWhenThen {

  private val isin: String = "CocaCola"
  private val orderGenerator = new OrderGenerator(isin)

  test("1) Exactly one buy limit. Sizes on both sides are equal so all orders should be matched.") {
    val orderbook = new Orderbook(isin)

    // Bid/Buy side
    orderbook += orderGenerator.newBuy(200, 202)
    orderbook += orderGenerator.newBuy(200, 201)
    orderbook += orderGenerator.newBuy(300, 200)

    // Ask/Sell side
    orderbook += orderGenerator.newSell(400, 197)
    orderbook += orderGenerator.newSell(200, 198)
    orderbook += orderGenerator.newSell(100, 200)

    val balancing: AuctionConditions = Auction(orderbook).deriveAuctionConditions()

    expectResult(200)(balancing.auctionPrice.get)
    expectResult(700)(balancing.executableQuantity)
    expectResult(0)(balancing.orderbook.buyOrders.size)
    expectResult(0)(balancing.orderbook.sellOrders.size)
    expectResult(0)(balancing.askSurplus)
    expectResult(0)(balancing.bidSurplus)
  }

  test("2) Not all orders can be matched since buy side has a surplus. The highest limit will be considered") {
    val orderbook = new Orderbook(isin)
    val expectedOrderbookBuySize = 600
    val expectedOrderbookSellSize = 500

    // Bid/Buy side
    orderbook += orderGenerator.newBuy(400, 202)
    orderbook += orderGenerator.newBuy(200, 201)

    // Ask/Sell side
    orderbook += orderGenerator.newSell(300, 199)
    orderbook += orderGenerator.newSell(200, 198)
    expectResult(expectedOrderbookBuySize)( orderbook.buyOrders.foldLeft(0)( _ + _.orderQty) )
    expectResult(expectedOrderbookSellSize)( orderbook.sellOrders.foldLeft(0)( _ + _.orderQty) )

    val matchResult = Auction(orderbook).deriveAuctionConditions()

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
    orderbook += orderGenerator.newBuy(300, 202)
    orderbook += orderGenerator.newBuy(200, 201)

    // Ask/Sell side
    orderbook += orderGenerator.newSell(400, 199)
    orderbook += orderGenerator.newSell(200, 198)
    expectResult(expectedOrderbookBuySize)( orderbook.buyOrders.foldLeft(0)( _ + _.orderQty) )
    expectResult(expectedOrderbookSellSize)( orderbook.sellOrders.foldLeft(0)( _ + _.orderQty) )

    val matchResult = Auction(orderbook).deriveAuctionConditions()

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

    expectResult(199){Auction(orderbook).deriveAuctionConditions(Option(199)).auctionPrice.get}
    expectResult(199){Auction(orderbook).deriveAuctionConditions(Option(200)).auctionPrice.get}
    expectResult(202){Auction(orderbook).deriveAuctionConditions(Option(201)).auctionPrice.get}
    expectResult(202){Auction(orderbook).deriveAuctionConditions(Option(202)).auctionPrice.get}
  }


  test("5) There are several possible limits an surplus on hand") {

    Given("The orderbook is setup with 2 orders on buy and sell side")
    val orderbook = Orderbook(isin)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 300, 202, isin)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 200, 201, isin)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 300, 199, isin)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 200, 198, isin)

    // reference : expected auction price
    val prices = Map( 200 -> 201, 202 -> 201, 198 -> 199 )
    for ( (referencePrice, expectedAuctionPrice) <- prices ) {
      When("reference price is EUR " + referencePrice)
      val auctionPrice = Auction(orderbook).deriveAuctionConditions(Some(referencePrice) ).auctionPrice.get
      expectResult(expectedAuctionPrice)(auctionPrice)
      Then("the auction price will " + expectedAuctionPrice + " EUR")
    }
  }


  test("6) Only market orders are executable in the order book") {
    Given("The orderbook is setup with 1 orders on buy and sell side")
    val orderbook = Orderbook(isin)
    orderbook += Order(Side.BUY, OrderType.MARKET, 900, isin)
    orderbook += Order(Side.SELL, OrderType.MARKET, 800, isin)

    And("No limit can serve as a price indicator")
    this.intercept[AuctionException]{
      Auction(orderbook).deriveAuctionConditions()
    }
    Then("Auction cannot be conducted and explod")

    When("Reference price is provided")
    val referencePrice: BigDecimal = 123
    val conducted: AuctionConditions = Auction(orderbook).deriveAuctionConditions(Some(referencePrice))
    expectResult(referencePrice)(conducted.auctionPrice.get)
    expectResult(0)(conducted.askSurplus)
    expectResult(100)(conducted.bidSurplus)
    expectResult(800)(conducted.executableQuantity)
    Then("The auction Price == reference price and orders could be matched")
  }


  test("7a) There is no eligible limit as there are only orders in the book which are not executable") {
    val orderbook = new Orderbook(isin)
    Given("Orders in book are not crossing")
    orderbook += new Order(Side.BUY, OrderType.HIDDEN, 80, 200, isin)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 80, 199, isin)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 80, 201, isin)

    When("The auction is conducted")
    val conducted: AuctionConditions = Auction(orderbook).deriveAuctionConditions()

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


  test("7b) partial execution of an order within the opening auction") {
    Given("The auction is opened with an auction price and orders need to be matched. " +
          "Time priority is used to execute one fully and one partially")

    val orderbook = new Orderbook(isin)
    val buy_1: Order = new Order(Side.BUY, OrderType.LIMIT, 300, 200, isin, timestamp = createTimestamp(9, 0))
    val buy_2: Order = new Order(Side.BUY, OrderType.LIMIT, 300, 200, isin, timestamp = createTimestamp(9, 1))

    orderbook += buy_1
    orderbook += buy_2
    orderbook += new Order( Side.SELL, OrderType.LIMIT, 400, 200, isin, timestamp = createTimestamp(9, 0))

    val auctionResult: AuctionConditions = Auction(orderbook).deriveAuctionConditions()
    val actualAuctionPrice: BigDecimal = auctionResult.auctionPrice.get
    expectResult(200)(actualAuctionPrice)

    val matchResult: MatchResult = Auction(orderbook).conduct(Some(actualAuctionPrice))
    expectResult( 0 )(matchResult.orderbook.sellOrders.size)
    expectResult( 1 )(matchResult.orderbook.buyOrders.size)

    When("Second buy order only gets partially executed")
    val unmatchedBuyOrder = matchResult.orderbook.buyOrders.head
    expectResult(200)(unmatchedBuyOrder.openQty)
    expectResult(100)(unmatchedBuyOrder.cummulatedQty)
    expectResult(buy_2.timestamp)(unmatchedBuyOrder.timestamp)
    Then("Remaining shares will be transfered into continous trading if order is not restricted to auctions only?")
  }

  private def createTimestamp(hour: Int, min: Int) : DateTime = {
    DateTime.now.withHour(hour).withMinuteOfHour(min)
  }
}