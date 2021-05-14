#!/bin/bash

if [ $# -eq 0 ]
then
    dir="results"
    filter='.'
elif [ $# -eq 1 ]
then
    dir=$1
    filter='.'
else
    dir=$1
    filter=$2
fi

for f in $(ls $dir/res_* | egrep $filter)
do
    echo $f
    cat $f | grep OVERALL | grep -v feat
done
