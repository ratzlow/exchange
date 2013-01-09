package org.exchange.model

import collection.mutable

/**
 * Represents a match of one:many which is required for partial execution.
 * An order can be executed against multiple matching opposite orders. If the order was already partially
 * matched then this execution can be filled for the remaining part only.
 *
 * @author fratzlow
 * @since 2012-12-31
 */
class Execution(one: Order, openSize: Int) {
  private val partialMatches= mutable.LinkedHashMap[Order, Int]()

  /**
   * Size of this execution of the given one order that should be filled by this execution. It can be the full order size
    * of _one_ or only a fraction.
    */
  private val size = openSize

  /**
   * The unfilled size of this execution. 0 <= #openQty <= #size
   */
  private var openQty = openSize

  //
  // auxilary constructors
  //

  def this(one: Order) = this( one, one.getSize)

  //
  // public API
  //

  def +=(nextManyMatch: Order, partialMatchingSize: Int) {
    add(nextManyMatch, partialMatchingSize)(openQty >= partialMatchingSize &&
                                            nextManyMatch.getSize >= partialMatchingSize)
  }

  def +=(nextManyMatch: Order) {
    add(nextManyMatch, nextManyMatch.getSize)(openQty >= nextManyMatch.getSize)
  }

  //
  // internal impl
  //

  /**
   * Add given order to the contained order that needs to be filled. If order sizes don't match one-by-one consider only
   * a fraction. The remaining size should be considered by another execution.
   *
   * @param nextManyMatch order to add to #one for matching
   * @param matchingSize what number of shares of given #nextManyMatch should be considered for this match
   * @param isSizeOkay size check logic to cover partial fill case and prevent from overfilling
   */
  private def add(nextManyMatch: Order, matchingSize: Int)(isSizeOkay: => Boolean) {
    require(nextManyMatch.getSide != one.getSide)

    if ( isSizeOkay ) {
      partialMatches.put( nextManyMatch, matchingSize )
      openQty -= matchingSize
    } else {
      val msg: String = "Cannot overfill order! openQty = " + openQty + " matchingSize = " + matchingSize
      throw new OrderbookException(msg)
    }
  }


  //
  // accessors
  //

  def getOpenQty = openQty
  def getOneOrder = one
  def getMany = partialMatches.keySet
  def isFullyExecuted = openQty == 0
  def getSize = size

  //
  // overrides
  //

  override def toString : String = {
    "\noneOrder = " + one + "@#" + size + " manyMatches = " +
      partialMatches.foldLeft("")( (s: String, tuple: (Order, Int)) => s + tuple._1 + "@#" + tuple._2)
  }
}
