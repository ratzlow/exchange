package org.exchange.matching.engine

import org.exchange.model.Orderbook
import org.exchange.matching.MatchResult

/**
 * Public interface every match engine must conform with. Test suites will be written against it.
 *
 * @author ratzlow@gmail.com
 * @since 2013-01-$
 */
trait MatchEngine {

  /**
   * Balances a given orderbook.
   */
  def balance(orderbook: Orderbook) : MatchResult
}

/**
 * Factory to return concrete instances of MatchEngine that might use different matching algos.
 */
object MatchEngine {
  def apply() : MatchEngine = new MatchEngineImpl()
}
