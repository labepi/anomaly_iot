#!/bin/bash

if [ $# -ne 4 ]
then
    echo 'Err: $D $tau_end $seed $series_len'
    exit 1
else
    D=$1
    tau_end=$2
    seed=$3
    series_len=$4
    #device=$5
fi

#for attack in $(seq 2 11)
#do
    Rscript classify_botnet_alljoin.R $D $tau_end $seed $series_len $attack
#done

