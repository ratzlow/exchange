package org.exchange.matching.engine

import org.scalatest.{GivenWhenThen, FunSuite}
import org.scala_tools.time.Imports._
import org.exchange.common.OrderGenerator._

/**
 * Verify several operations that can be performed on the orderbook.
 *
 * @author ratzlow@gmail.com
 * @since 2013-03-10
 */
class MatchPriorityTest extends FunSuite with GivenWhenThen {

  test("Test ordering based on price/time priority: BUY") {
    val now = DateTime.now
    val order_1 = newBuy(2, 1, now.plusSeconds(5))
    val order_2 = newBuy(4, 10, now.plusSeconds(10))
    val order_3 = newBuy(6, 5, now.plusSeconds(20))
    val order_4 = newBuy(6, 5, now.plusSeconds(15))
    val orders = List(order_1, order_2, order_3, order_4)

    val orderAr = orders.toArray
    expectResult(order_1)(orderAr(0))
    expectResult(order_2)(orderAr(1))
    expectResult(order_3)(orderAr(2))
    expectResult(order_4)(orderAr(3))

    val sortedOrderAr = orders.sortWith(MatchPriority.buyPriceTimePriority).toArray
    expectResult(order_2)(sortedOrderAr(0))
    expectResult(order_4)(sortedOrderAr(1))
    expectResult(order_3)(sortedOrderAr(2))
    expectResult(order_1)(sortedOrderAr(3))
    Then("The order with the highest price should be on top of the book")
  }

  test("Test ordering based on price/time priority: SELL") {
    val now = DateTime.now
    val order_1 = newSell(2, 1, now.plusSeconds(5))
    val order_2 = newSell(4, 10, now.plusSeconds(10))
    val order_3 = newSell(6, 5, now.plusSeconds(20))
    val order_4 = newSell(6, 5, now.plusSeconds(15))
    val orders = List(order_1, order_2, order_3, order_4)

    val orderAr = orders.toArray
    expectResult(order_1)(orderAr(0))
    expectResult(order_2)(orderAr(1))
    expectResult(order_3)(orderAr(2))
    expectResult(order_4)(orderAr(3))

    val sortedOrderAr = orders.sortWith(  MatchPriority.sellPriceTimePriority ).toArray
    expectResult(order_1)(sortedOrderAr(0))
    expectResult(order_4)(sortedOrderAr(1))
    expectResult(order_3)(sortedOrderAr(2))
    expectResult(order_2)(sortedOrderAr(3))
    Then("The order with the lowest price should be on top of the book")
  }
}
