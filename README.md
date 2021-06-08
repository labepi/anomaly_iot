# anomaly_iot

IoT Botnet Detection based on Anomalies of Multiscale Time Series Dynamics

# Description

This project proposes a strategy for detecting IoT botnet attacks based
on anomalies of multiscale time series dynamics

# Folders and Files

- data/
    - This directory contains the data from the N-BaIoT dataset, used
      for the detection of botnet attacks.

- analysis/ 
    - Some scripts for analyzing the results.

- results/
    - The results of scripts executions.

- classify_botnet_single.R
    - This is the main classification script in R for the 'one model
      per device' experiment.
    - The shell scripts with same prefix are used to automatize
      the experiments.

- classify_botnet_alljoin.R
    - This is the main classification script in R for the 'one model
      for all devices' experiment.
    - The shell scripts with same prefix are used to automatize
      the experiments.

- features.R
    - The script with functions used for extracting features from the
      time series.

- config.R
    - Some configurations.

- includes.R
    - The required packages.

- README.md
    - This readme.

- utils.R
    - Some utility functions.

# Datasets

This is the structure expected for the files of N-BaIoT dataset:

- data/botnet/original/Danmini_Doorbell/
    - benign_traffic.csv  
    - gafgyt/  
        - combo.csv  
        - junk.csv  
        - scan.csv  
        - tcp.csv  
        - udp.csv
    - mirai/
        - ack.csv  
        - scan.csv  
        - syn.csv  
        - udp.csv  
        - udpplain.csv

- data/botnet/original/Ecobee_Thermostat/
    - benign_traffic.csv  
    - gafgyt/  
    - mirai/

- data/botnet/original/Ennio_Doorbell:
    - benign_traffic.csv  
    - gafgyt/

- data/botnet/original/Philips_B120N10_Baby_Monitor:
    - benign_traffic.csv  
    - gafgyt/  
    - mirai/

- data/botnet/original/Provision_PT_737E_Security_Camera:
    - benign_traffic.csv  
    - gafgyt/  
    - mirai/

- data/botnet/original/Provision_PT_838_Security_Camera:
    - benign_traffic.csv  
    - gafgyt/  
    - mirai/

- data/botnet/original/Samsung_SNH_1011_N_Webcam:
    - benign_traffic.csv  
    - gafgyt/

- data/botnet/original/SimpleHome_XCS7_1002_WHT_Security_Camera:
    - benign_traffic.csv  
    - gafgyt/  
    - mirai/

- data/botnet/original/SimpleHome_XCS7_1003_WHT_Security_Camera:
    - benign_traffic.csv  
    - gafgyt/  
    - mirai/

