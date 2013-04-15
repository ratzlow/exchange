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

For the test coverage, run:

    sbt scct:test

And check `target/scala-2.10/coverage-report/index.html` afterwards.

Links
-----
* CI - <https://travis-ci.org/suls/exchange>
