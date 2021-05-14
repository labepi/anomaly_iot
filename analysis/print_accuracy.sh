#!/bin/bash

if [ $# -eq 0 ]
then
    filter='.'
else
    filter=$1
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

i=1
for dev in ${dev_names[@]}
do
    #echo $dev
    for num in $(seq 200 100 1000)
    do
        res=$(./check_results.sh ../results/single/ $num'-.*'$dev | \
            grep DEBUG | grep $filter | cut -d' ' -f8,10 | sed 's/ /-/')
        for res_i in $res
        do
            echo $num' '$res_i' '$i | sed 's/-/ /'
        done
    done
    i=$((i+1))
done

#./check_results.sh ../results/single/ '600-.*838' | grep DEBUG | ./mean.awk -v col=10

