package org.exchange.matching.auction

import org.exchange.model.Orderbook

/**
 * Aggregates all parameters that resulted from an auction of a given orderbook.
 *
 * @param orderbook after the auction was performed. Contains all the orders that could not be executed or were not
 *                  considered for the price finding process.
 * @param referencePrice criteria to find auction price to use for the auction // TODO (FRa) : (FRa) : more details
 * @param lowestVisibleAskLimit limit on ask side nearest to the market if no auction price can be determined
 * @param highestVisibleBidLimit limit on bid side nearest to the market if no auction price can be determined
 *
 * @author ratzlow@gmail.com
 * @since 2013-02-24
 */
case class AuctionResult(orderbook: Orderbook,
                         referencePrice: Option[BigDecimal] = None,
                         auctionPrice: Option[BigDecimal] = None,
                         bidSurplus: Int, askSurplus: Int,
                         executableQuantity: Int,
                         lowestVisibleAskLimit: Option[BigDecimal] = None,
                         highestVisibleBidLimit: Option[BigDecimal] = None) {
}