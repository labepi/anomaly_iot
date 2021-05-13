#!/bin/bash

if [ $# -eq 0 ]
then
    dir="results"
else
    dir=$1
fi

for f in $(ls $dir/res_*)
do
    echo $f
    cat $f | grep OVERALL | grep -v feat
done
