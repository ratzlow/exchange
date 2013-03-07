exchange [![Build Status](https://travis-ci.org/suls/exchange.png?branch=master)](https://travis-ci.org/suls/exchange)
========

Playground for some Scala code. Goal is the implementation of a fully working stock exchange that supports auctions,
continuous trading, price feeds, order life cycle etc. For more detailed docs see $ROOT/docs directory.
This project is at a very early stage and lot of exciting stuff is yet to come.

If you like to contribute drop me a line :-)


Requirements
------------
* sbt - <http://www.scala-sbt.org/0.12.1/docs/Getting-Started/Setup.html>

Usage
-----
To create the project files for your IDE (eg Intellij):

	sbt gen-idea
	                                                        
Or to simply run all the tests:

    sbt test

Links
-----
* CI - <https://travis-ci.org/suls/exchange>

Open tasks
-----------
* add logging
* add matching for market, stoplimit orders against limit orders (define what good price means in this combination)
* make matching rules injectable strategies as traits (auction vs. cont trading)
* move all tests to FunSuite to produce better output in IDE (fix IDE plugin?)
* execute with 1e6 orders to check for stack overflow on recursive algos
* use proper date time API

Open questions
--------------
* in absence of return stmt is it possible to write code like:  if (!precondition) return -> avoids complexity of body
