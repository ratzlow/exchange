package org.exchange.model

/**
 * Will be thrown if an invalid operation is tried to be done on the orderbook.
 *
 * @param msg what is wrong the book
 *
 * @author ratzlow@gmail.com
 * @since 2012-12-31
 */
class OrderbookException(msg: String) extends Exception(msg) {}
