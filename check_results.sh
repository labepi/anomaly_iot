#!/bin/bash

for f in $(ls results/res_*)
do
    echo $f
    cat cat $f | grep OVERALL | grep -v feat
done
