package org.exchange.matching.auction

import org.exchange.model.Orderbook

/**
 * TODO: comment
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-24
 */
case class AuctionResult(orderbook: Orderbook, referencePrice: Option[BigDecimal] = None,
                         auctionPrice: Option[BigDecimal] = None,
                         bidSurplus: Int, askSurplus: Int, executableQuantity: Int) {
}