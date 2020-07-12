Introduction
============

A lexer, parser and pretty printing library for Python 2 and 3.

Intended use case
-----------------

Generally speaking this library was written with the intention of parsing Python for the purposes of program transformation and compilation. It can also be used for Python code generation, but that is not the primary goal. As a consequence the "Abstract Syntax Tree" is not particularly abstract, and might be better described as a "Concrete Syntax Tree". 

License
-------

language-python is released as open source software under the terms of the 3 clause BSD License. See the file LICENCE.txt in the [source code repository of language-python](https://github.com/bjpop/language-python).

Installation
------------

language-python can be installed with cabal (cabal version 3.0.0.0 onwards):

```
cabal build all
```

Testing
-------

Test cases are provided in a language-python-test. Test binaries can be built and run as below (cabal version 3.0.0.0 onwards):

```
cabal install shelltestrunner
cabal install --overwrite-policy=always language-python-test
cd language-python-test/
PATH=$HOME/.cabal/bin/:$PATH make test
```

If all goes well you should see a long list of test cases and a summary at the end, that should look like the following:
```
         Test Cases    Total        
 Passed  519           519          
 Failed  0             0            
 Total   519           519  
```

Pull requests
-------------

Pull requests are greatly appreciated. If you plan to submit a pull request, please test your code in the test suite first. New test cases are desirable, especially if you are fixing a bug or adding a new feature.
