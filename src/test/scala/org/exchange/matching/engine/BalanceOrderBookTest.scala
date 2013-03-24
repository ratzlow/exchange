package org.exchange.matching.engine

import org.exchange.common.OrderGenerator
import org.exchange.model.{OrderType, Orderbook, Side, Order}
import org.exchange.model.OrderType._
import org.scalatest.FunSuite

/**
 * Execute some scenarios to fill and balance an orderbook. All actions are based on one time execution. Continuous
 * trading isn't considered here.
 *
 * @author ratzlow@gmail.com
 * @since 2013-03-24
 */
class BalanceOrderBookTest extends FunSuite {
  private val numberOfOrders: Int = 10000

  test("Match orderbook with only orders of type: limit") {
    executeFilledOrderbook(OrderType.LIMIT)
  }

  test("Match orderbook with only orders of type: stoplimit") {
    executeFilledOrderbook(OrderType.STOP_LIMIT)
  }


  private def executeFilledOrderbook( orderTypes: OrderType* ) {
    val generator = new OrderGenerator("IBM")

    val sells: Seq[Order] = generator.newOrders(numberOfOrders, Side.SELL, (150, 300), (1000, 1100), orderTypes: _* )
    val buys: Seq[Order] = generator.newOrders(numberOfOrders, Side.BUY, (150, 300), (1000, 1100), OrderType.LIMIT)

    // fill orderbook
    val orderbook = new Orderbook("CoCa", buys.toList, sells.toList)
    val balancedBook: MatchResult = MatchEngine().balance(orderbook)
    val unbalancedBuys: List[Order] = balancedBook.orderbook.buyOrders
    val unbalancedSells: List[Order] = balancedBook.orderbook.sellOrders
    profile(s"Balancing the orderbook with $numberOfOrders") {
      println(s"executions=${balancedBook.executions.size} => " +
        s"open bids=${unbalancedBuys.size} : " +
        s"open asks=${unbalancedSells.size}"
      )
    }

    // check unmatched orders are closest to the market
    var bestBuy: Option[Order] = None
    if (!unbalancedBuys.isEmpty) {
      bestBuy = unbalancedBuys.headOption
      val sortedBestBuy = unbalancedBuys.maxBy(_.price)
      expectResult(bestBuy.get)(sortedBestBuy)
    }

    var bestSell: Option[Order] = None
    if (!unbalancedSells.isEmpty) {
      bestSell = unbalancedSells.headOption
      val sortedBestSell = unbalancedSells.minBy(_.price)
      expectResult(bestSell.get)(sortedBestSell)
    }

    // best unmatched orders on top of the market do not cross -> are not "good enough"
    println(s"(best buy price) ${bestBuy.get.price} < ${bestSell.get.price} (best sell price)")
    assert(bestBuy.get.price < bestSell.get.price)
  }

  private def profile(msg: String) ( f: => Unit ) {
    val start: Long = System.nanoTime()
    f
    println(s"duration for $msg = ${(System.nanoTime() - start) / 1e6} ms")
  }
}
