#!/bin/bash

if [ $# -ne 6 ]
then
    echo 'Err: $D $tau_end $seed $series_len $device $attack'
    exit 1
else
    #d_name=$1
    D=$1
    tau_end=$2
    seed=$3
    series_len=$4
    device=$5
    attack=$6
fi

for field in $(head -n 1 data/botnet/demonstrate_structure.csv | sed 's/,/ /gi')
do
    d_name=$field
    ./classify_botnet_oneclass_device.sh $d_name $D $tau_end $seed $series_len $device $attack
done

