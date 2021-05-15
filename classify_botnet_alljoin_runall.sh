#!/bin/bash

if [ $# -ne 3 ]
then
    echo 'Err: $D $tau_end $seed'
    exit 1
else
    D=$1
    tau_end=$2
    seed=$3
    #series_len=$4
    #device=$4
fi

#for series_len in $(seq 200 100 900)
for series_len in $(seq 200 100 1000)
do
    ./classify_botnet_alljoin.sh $D $tau_end $seed $series_len &> 'results/alljoin/res_alljoin_D'$D'_T'$tau_end'_S'$seed'_L'$series_len'.txt' &
done

