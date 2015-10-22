Introduction
============

A lexer, parser and pretty printing library for Python 2 and 3.

License
-------

language-python is released as open source software under the terms of the 3 clause BSD License. See the file LICENCE.txt in the [source code repository of language-python](https://github.com/bjpop/language-python).

Installation
------------

language-python can be installed with cabal:

    cabal install language-python

or, if you prefer, in a sandbox (recommended):

    cabal sandbox init
    cabal update
    cabal install happy
    cabal install alex
    cabal install --dry-run
    cabal install

use `-jN` for `N` threads of parallel building in the `cabal install` step if you have a multi-core machine.

Testing
-------

Test cases are provided in a separate package 
[language-python-test](https://github.com/bjpop/language-python-test).
