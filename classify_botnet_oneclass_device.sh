#!/bin/bash

if [ $# -ne 6 ]
then
    echo 'Err: $D $tau_end $seed $series_len $device'
    exit 1
else
    d_name=$1
    D=$2
    tau_end=$3
    seed=$4
    series_len=$5
    device=$6
fi

# a single call
Rscript classify_botnet_oneclass_device.R $d_name $D $tau_end $seed $series_len $device 

