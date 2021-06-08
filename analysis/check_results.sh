#!/bin/bash

if [ $# -eq 0 ]
then
    dir="results"
    filter='.'
    filter_pos='OVERALL'
elif [ $# -eq 1 ]
then
    dir=$1
    filter='.'
    filter_pos='OVERALL'
elif [ $# -eq 2 ]
then
    dir=$1
    filter=$2
    filter_pos='OVERALL'
else
    dir=$1
    filter=$2
    filter_pos=$3
fi

for f in $(ls $dir/res_* | egrep $filter)
do
    echo $f
    cat $f | grep $filter_pos | grep -v feat
done
