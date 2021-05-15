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

dev_names=(
    'Danmini_Doorbell'
    'Ecobee_Thermostat'
    'Ennio_Doorbell'
    'Philips_B120N10_Baby_Monitor'
    'Provision_PT_737E_Security_Camera'
    'Provision_PT_838_Security_Camera'
    'Samsung_SNH_1011_N_Webcam'
    'SimpleHome_XCS7_1002_WHT_Security_Camera'
    'SimpleHome_XCS7_1003_WHT_Security_Camera'
    )

for device in ${dev_names[@]}
do
    ./classify_botnet_oneclass_device_multifeats.sh $D $tau_end $seed $series_len $device &> \
        'results/multifeats/res_multifeats_D'$D'_T'$tau_end'_S'$seed'_L'$series_len'-'$device'.txt' &
done

