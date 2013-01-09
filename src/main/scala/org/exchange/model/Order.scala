package org.exchange.model

import Side._

class Order(side: Side, size: Int, price: BigDecimal, isin: String) {

  def getSide = side
  def getSize = size
  def getPrice = price
  def getIsin = isin

  override def toString : String =
    "side=" + side + " " +
    "size=" + size+ " " +
    "price=" + price + " " +
    "isin=" + isin
}

