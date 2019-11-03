#!/bin/bash

for file in *.py; do
    testfile=`basename $file .py`.test
    cat > $testfile <<HERE
language-python-roundtrip 2 $file
<<<
>>>
>>>2
>>>=0 
HERE
done
