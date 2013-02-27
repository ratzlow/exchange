package org.exchange.matching.engine

import org.exchange.model._
import org.exchange.model.Orderbook
import org.exchange.model.Order
import scala.Option

/**
 * Match the orders against each other. First orders will be ordered with orders nearest to the market first. This is
 * defined by various rules that determine what is "nearest to the market".
 *
 * @param referencePrice if provided will be consulted in good price check. Used in auction matching. Default is None.
 *
 * @author ratzlow@gmail.com
 * @since 2013-01-12
 */
class MatchEngineImpl( referencePrice: Option[BigDecimal] = None ) extends MatchEngine {

  //
  // API entry point
  //

  override def balance(orderbook: Orderbook) : MatchResult = {
    //TODO (FRa) : (FRa) : move ordering of orders to insertion time according to price/time prio
    val orderedBuyOrders: List[Order] = orderbook.buyOrders.sortWith(_.price > _.price)
    val orderedSellOrders: List[Order] = orderbook.sellOrders.sortWith(_.price < _.price)
    balance(orderedBuyOrders, orderedSellOrders, Nil)
  }

  //
  // internal impl
  //

  private def balance( buyOrders: List[Order], sellOrders: List[Order],
                       previousExecutions : List[Execution] = List.empty ) : MatchResult = {

    // TODO (FRa) : (FRa) : in absence of return stmt is it possible to write code like:
    // if (!precondition) return -> avoids complexity of body
    if (!buyOrders.isEmpty && !sellOrders.isEmpty) {
      val buy: Order = buyOrders.head
          val sell: Order = sellOrders.head
          val optionalExec: Option[Execution] = execute(buy, sell)

          optionalExec match {
            // orders couldn't be matched because they are to far from the market, so stop matching
            case None =>
              // TODO (FRa) : (FRa) : add isin
              new MatchResult(new Orderbook("??", buyOrders, sellOrders), previousExecutions)

            // orders were matched
            case Some(execution) =>
              val leftBuys = unmatchedOrders( execution.buy, buyOrders.tail, execution.executionSize )
              val leftSells = unmatchedOrders( execution.sell, sellOrders.tail, execution.executionSize )
              val executions = execution :: previousExecutions
              // TODO (FRa) : (FRa) : check if this is really tail recursive -> would otherwise risk StackOverflow
              balance( leftBuys, leftSells, executions )
          }
    }else new MatchResult( new Orderbook("???", buyOrders, sellOrders), previousExecutions)
  }


  private def unmatchedOrders(order: Order, orders: List[Order], executionSize: Int ) : List[Order] = {
    val newCumQty = order.cummulatedQty + executionSize

    if ( newCumQty < order.orderQty ) {
      val updatedOrder: Order = order.copy(cummulatedQty = newCumQty)
      updatedOrder :: orders
    } else orders
  }

  /**
   * The actual order match. Here we will hook in matching rules later on.
   *
   * @param one
   * @param other
   * @return if orders can be matched an execution otherwise None
   */
  private def execute(one: Order, other: Order) : Option[Execution] = {
    val executionSize = Math.min(one.openQty, other.openQty)

    // the sell order can be fully added to order to be executed
    if (executionSize >= 0 && isGoodExecutionPrice(one, other) && canBeMatched(one, other) )
      Some( Execution(one, other, executionSize) )
    else None
  }

  // TODO (FRa) : (FRa) : make available as strategy for auctions vs. continuous trading
  private def canBeMatched( one: Order, other: Order ) : Boolean = {
    // only market orders are executable against each other or non-market orders are
    (one.orderType == OrderType.MARKET && other.orderType == OrderType.MARKET) ||
    (one.orderType != OrderType.MARKET && other.orderType != OrderType.MARKET)
  }

  //
  // START: price checks put in extrac class
  //

  /**
   * Check if the prices for both orders are acceptable to each other.
   *
   * @param one
   * @param other
   * @return
   */
  def isGoodExecutionPrice(one: Order, other: Order): Boolean = {
    isGoodPrice(one.price, other) && isGoodPrice(other.price, one)
  }

  /**
   * Price will always be okay if we match against a market order
   */
  private def isGoodPrice( tradePrice: BigDecimal, order: Order ) : Boolean = {
    def isGoodSellPrice : Boolean = {
      require( tradePrice >= 0 )
      require(order.side == Side.SELL)
      tradePrice >= order.price && referencePrice.getOrElse(tradePrice) >= order.price
    }

    def isGoodBuyPrice : Boolean = {
      require( tradePrice >= 0 )
      require( order.side == Side.BUY)
      tradePrice <= order.price && referencePrice.getOrElse(tradePrice) <= order.price
    }

    if (order.side == Side.BUY) isGoodBuyPrice
    else if (order.side == Side.SELL) isGoodSellPrice
    else throw new IllegalArgumentException("Invalid order side " + order)
  }

}
