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
    - 
