#!/bin/bash

if [ $# -ne 7 ]
then
    echo 'Err: $d_name $D $tau_end $seed $series_len $device $attack'
    exit 1
else
    d_name=$1
    D=$2
    tau_end=$3
    seed=$4
    series_len=$5
    device=$6
    attack=$7
fi

# a single call
Rscript classify_botnet_oneclass_device.R $d_name $D $tau_end $seed $series_len $device $attack

