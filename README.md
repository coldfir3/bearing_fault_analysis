## Roller Bearing fault detection unsing CNN

## Overview

This code is bla bla bla

## Dependencies 

```{R}
keras
tidyverse
```

## Scripts

`data_import.R`: file to read the matlab files from the dataset and convert it to tidy CSV

## Dataset
This dataset was gathered from Case Western Reserve University (CWRU) Bearing Data Center at https://csegroups.case.edu/bearingdatacenter/pages/download-data-file

Since the original files were saved on matlab extension converted the dataset R tidy tibbles with the `data_import.R` script.

This script expects a folder organization such as follows:

* D:/datasets/bearing_fault_cwru/12k Drive End Bearing Fault Data
  + Ball
    - 1.0.mat
    - 1.1.mat
    - ...
    - 3.3.mat
  + Inner
  + Normal
  + Outer3
  + Outer6
  + Outer12

The original file names were renamed to fit the following pattern `a.b.mat` means that this file has a damage size of `a` with a load of `b`.