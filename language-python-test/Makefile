# Copyright   : (c) 2014 Bernie Pope
# License     : BSD-style
# Maintainer  : florbitous@gmail.com

# A convenience Makefile.

.PHONY: test
test:
	shelltest --color --execdir test/ -- -j1

.PHONY: test_features
test_features:
	shelltest --color --execdir test/features -- -j1

.PHONY: test_cpython2
test_cpython2:
	shelltest --color --execdir test/CPython_test_suite_v2 -- -j1

.PHONY: test_cpython3
test_cpython3:
	shelltest --color --execdir test/CPython_test_suite_v3 -- -j1
