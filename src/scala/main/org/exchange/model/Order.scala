package org.exchange.model

import Side._

/**
 * The order submitted by the trader. An order of one side needs to be matched against order(s) of the other side.
 *
 * @param side ... {@see Side}
 * @param size ... number of shares
 * @param price ... the price
 * @param isin ... instrument identifier
 */
case class Order(side: Side, size: Int, price: BigDecimal, isin: String) {

  override def toString : String =
    "side=" + side + " " +
    "size=" + size+ " " +
    "price=" + price + " " +
    "isin=" + isin
}

