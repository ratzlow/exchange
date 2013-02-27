package org.exchange.matching

import auction.{AuctionResult, AuctionException, Auction}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.exchange.model._
import org.exchange.model.Orderbook
import scala.Some

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-12
 */
class AuctionMatchingSpec extends FeatureSpec with GivenWhenThen {

  private val symbol: String = "IBM"


  feature("5) There are several possible limits an surplus on hand") {

    Given("The orderbook is setup with 2 orders on buy and sell side")
    val orderbook = Orderbook(symbol)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 300, 202, symbol)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 200, 201, symbol)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 300, 199, symbol)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 200, 198, symbol)

    // reference : expected auction price
    val prices = Map( 200 -> 201, 202 -> 201, 198 -> 199 )
    for ( (referencePrice, expectedAuctionPrice) <- prices ) {
      When("reference price is EUR " + referencePrice)
      val auctionPrice = Auction(orderbook).conduct(Some(referencePrice) ).auctionPrice.get
      expectResult(expectedAuctionPrice)(auctionPrice)
      Then("the auction price will " + expectedAuctionPrice + " EUR")
    }
  }


  feature("6) Only market orders are executable in the order book") {
    Given("The orderbook is setup with 1 orders on buy and sell side")
    val orderbook = Orderbook(symbol)
    orderbook += Order(Side.BUY, OrderType.MARKET, 900, symbol)
    orderbook += Order(Side.SELL, OrderType.MARKET, 800, symbol)

    And("No limit can serve as a price indicator")
    this.intercept[AuctionException]{
      Auction(orderbook).conduct()
    }
    Then("Auction cannot be conducted and explod")

//    When("Reference price is provided")
//    val referencePrice: BigDecimal = 123
//    val conducted: AuctionResult = Auction(orderbook).conduct(Some(referencePrice))
//    expectResult(referencePrice)(conducted.auctionPrice.get)
//    expectResult(0)(conducted.askSurplus)
//    expectResult(100)(conducted.bidSurplus)
//    expectResult(800)(conducted.executableQuantity)
//    Then("The auction Price == reference price and orders could be matched")

  }
}
