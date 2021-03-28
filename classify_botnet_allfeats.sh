#!/bin/bash

if [ $# -ne 4 ]
then
    echo 'Err: $D $tau_end $seed $series_len'
    exit 1
else
    D=$2
    tau_end=$3
    seed=$4
    series_len=$5
fi

for field in $(head -n 1 data/botnet/demonstrate_structure.csv | sed 's/,/ /gi')
do
    ./classify_botnet.sh $field 3 10 1 1000
done

