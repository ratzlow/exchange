package org.exchange.matching.engine

import org.exchange.model.{Execution, Orderbook}

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-24
 */
case class MatchResult(orderbook: Orderbook, executions: List[Execution] = Nil) {
}
