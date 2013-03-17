package org.exchange.matching.engine

import org.exchange.model.{Execution, Orderbook}

/**
 * The outcome of a matched orderbook.
 *
 * @param orderbook with unmatched or partially matched orders
 * @param executions matched orders or by default empty list
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-24
 */
case class MatchResult(orderbook: Orderbook, executions: List[Execution] = Nil) {
}
