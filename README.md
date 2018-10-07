# Gladiators Readme

## Raw data creation

The raw zip file is stored in `data`. The munging scripts are stored in `/munge`. When run in order, the script will unzip the raw data file (10MB) to it's unzipped state (70MB), perform some minor data preparation activities, and then perform a stratified train-test split using a fixed seed.

Two new dataframes `raw_train` and `raw_test` are created, and stored as `rdata` objects under `/cache`.

## EDA

The EDA can start with `/src/01-A-EDA.R`.
