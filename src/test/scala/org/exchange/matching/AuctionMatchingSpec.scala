package org.exchange.matching

import engine.MatchEngine
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.exchange.model.{OrderType, Side, Order, Orderbook}

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-12
 */
class AuctionMatchingSpec extends FeatureSpec with GivenWhenThen {

  feature("Matching orderbook in auction") {

    Given("The orderbook is setup with 2 orders on buy and sell side")
    val symbol: String = "IBM"
    val orderbook = Orderbook(symbol)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 300, 202, symbol)
    orderbook += new Order(Side.BUY, OrderType.LIMIT, 200, 201, symbol)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 300, 199, symbol)
    orderbook += new Order(Side.SELL, OrderType.LIMIT, 200, 198, symbol)

    When("orderbook is balanced, but orders are not really executed since it is an auction mode")
    val matchEngine = MatchEngine()
    val balancing: MatchResult_1 = matchEngine.balance(orderbook)

    Then("Some potential executions should have been found")
    assert( Option(balancing.orderbook).isDefined )
    assert( Option(balancing.executions).isDefined )
    expectResult(true) { !balancing.executions.isEmpty }

    // reference : expected auction price
    val prices = Map( 200 -> 201, 202 -> 201, 198 -> 199 )
    for ( (referencePrice, expectedAuctionPrice) <- prices ) {
      When("reference price is EUR " + referencePrice)
      expectResult(expectedAuctionPrice)(balancing.auctionPrice(referencePrice))
      Then("the auction price will " + expectedAuctionPrice + " EUR")
    }
  }
}
