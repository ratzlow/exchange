package org.exchange.matching.auction

/**
 * If invalid conditions for an auction are encountered during price determination or the actual execution this
 * exception is thrown.
 *
 * @param msg ... reason for this exception
 * @author ratzlow@gmail.com
 * @since 2013-02-27
 */
case class AuctionException(msg: String) extends Exception(msg) { }
