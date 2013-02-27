package org.exchange.matching.auction

import org.exchange.model.Orderbook

import org.exchange.matching.engine.{MatchResult, MatchEngineImpl}

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-16
 */
class Auction {

  // TODO (FRa) : (FRa) : perf imp: execute loop in parallel
  def balance( orderbook: Orderbook, referencePrice: Option[BigDecimal] = None ) : AuctionResult = {

    val possibleLimits: Set[BigDecimal] =
      ((orderbook.sellOrders.map( _.price ) ++ orderbook.buyOrders.map(_.price))).toSet

    // function literal that matches the order book for a given limit
    val matchByLimit = (limit: BigDecimal) => {
      val matchEngine = new MatchEngineImpl(Option(limit))
      val balanced: MatchResult = matchEngine.balance(orderbook)
      new AuctionMatch(limit, balanced.executions, balanced.orderbook)
    }

    // match on all limits greater zero -> side effect: populate limit:match map
    val auctions = possibleLimits.filter(_ > 0).map( matchByLimit(_) )

    new AuctionResult( referencePrice, auctions.toList )
  }
}
