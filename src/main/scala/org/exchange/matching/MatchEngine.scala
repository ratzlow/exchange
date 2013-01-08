package org.exchange.matching

import org.exchange.model._
import collection.mutable


class MatchEngine(orderbook: Orderbook) {

  val leftUnmatchedOrders = new QueueContainer(orderbook.getBuyOrders.sortWith(_.getPrice > _.getPrice), Side.BUY)
  val rightUnmatchedOrders = new QueueContainer(orderbook.getSellOrders.sortWith(_.getPrice > _.getPrice), Side.SELL)
  val executions = new mutable.MutableList[Execution]

  def calc() {
    if ( !leftUnmatchedOrders.getOrders.isEmpty ) {
      val order = leftUnmatchedOrders.getOrders.dequeue()
      matchOrder( order, order.getSize )
    }
  }


  private def matchOrder( orderToMatch: Order, openQty: Int ) {

    val execution = new Execution( orderToMatch, openQty )
    executions += execution
    val otherSideOrders = getOtherSideOrders( orderToMatch.getSide )

    // TODO (FRa) : (FRa) : rewrite in a more functional style
    while( !otherSideOrders.isEmpty ) {
      val unmatched = otherSideOrders.dequeue()
      val openQty: Int = execution.getOpenQty

      // if current order is fully executed start with next
      if ( openQty == 0 ) {
        matchOrder( unmatched, unmatched.getSize )

      // the orderToExecute order can be fully added to order to be executed
      } else if (openQty >= unmatched.getSize && isGoodExecutionPrice(execution, unmatched) ) {
        execution += unmatched

      // only partially executed by the match, so the partial one will needs
      // execution from the other side
      } else if (openQty > 0 && unmatched.getSize > openQty && isGoodExecutionPrice(execution, unmatched) ) {
        execution +=(unmatched, openQty)
        matchOrder(unmatched, unmatched.getSize - openQty)
      }
    }
  }

  //
  // START: price checks put in extrac class
  //


  def isGoodExecutionPrice(execution: Execution, orderToExecute: Order): Boolean = {
    isGoodPrice(execution.getOneOrder.getPrice, orderToExecute) &&
    isGoodPrice(orderToExecute.getPrice, execution.getOneOrder)
  }

  /**
   * Expects both order to be limit orders
   * // TODO (FRa) : (FRa) : impl Limit orderType
   */
  // TODO (FRa) : (FRa) : prices of side need to be comared against market price later on
  private def isGoodPrice( tradePrice: BigDecimal, order: Order ) : Boolean = {
    def isGoodSellPrice : Boolean = {
      require( tradePrice >= 0 )
      require(order.getSide == Side.SELL)
      tradePrice >= order.getPrice
    }

    def isGoodBuyPrice : Boolean = {
      require( tradePrice >= 0 )
      require( order.getSide == Side.BUY)
      tradePrice <= order.getPrice
    }

    if (order.getSide == Side.BUY) isGoodBuyPrice
    else if (order.getSide == Side.SELL) isGoodSellPrice
    else throw new IllegalArgumentException("Invalid order side" + order)
  }


  // END: price checks


  private def getOtherSideOrders(side: Side.Side) : mutable.Queue[Order] =
    if (leftUnmatchedOrders.getSide == side) rightUnmatchedOrders.getOrders
    else if (rightUnmatchedOrders.getSide == side) leftUnmatchedOrders.getOrders
    else new mutable.Queue  // TODO (FRa) : (FRa) : use predef EmptyQueue


  def getExecutions = executions.toList

  class QueueContainer( orders: List[Order], side: Side.Side ) {
    val queue = new mutable.Queue[Order]
    queue ++= orders

    def getOrders = queue
    def getSide = side
  }
}
