Introduction
============

A test suite for the Haskell library [language-python](https://github.com/bjpop/language-python) (a parser for Python 2 and 3).

License
-------

language-python-test is released as open source software under the terms of the 3 clause BSD License. See the file LICENCE.txt in the [source code repository of language-python-test](https://github.com/bjpop/language-python-test).

This package also contains files from the CPython test suite. Those files are found in the
sub-directories test/CPython_test_suite_v2 and test/CPython_test_suite_v3. The license for those files is
contained in those directories. 

Installation
------------

Using cabal version 3.0.0.0 or greater:

```
cabal build
cabal install
```

Usage
-----

The package builds the following executable programs:

* language-python-parse-pretty
* language-python-roundtrip
* language-python-tokens

The first program parses a Python file as input and pretty prints it back again.

The second program performs a round-trip of parse, pretty print, parse and pretty print. It checks that the first
pretty print is equal to the second pretty print. This is not a perfect test for correctness, but it does check that the parser and pretty printer agree to some extent, and is usually quite good at finding errors.

The third program performs lexical analysis on the input Python file and pretty prints the resulting token stream.

The test suite (which tests the behaviour of language-python) uses the shelltest tool. To run the tests you need to have shelltest installed:

```
cabal install shelltestrunner
```

The tests are found in the sub-directory called tests.

You can run the tests like so from the top directory of the language-python-test package:

```
shelltest --color --execdir test/ -- -j1
```

If you have installed into a cabal sandbox, then you might need to adjust your path:

```
PATH=$HOME/.cabal/bin/:$PATH shelltest --color --execdir test/ -- -j1 
```

We provide a Makefile for convenience which does the same thing. You can run it like so:

```
make test
```

or, with PATH adjustment:

```
PATH=$HOME/.cabal/bin/:$PATH make test 
```
