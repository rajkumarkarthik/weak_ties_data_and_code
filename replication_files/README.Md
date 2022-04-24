# Regression replication files

This directory contains data files and code to replicate all OLS regressions in the SI. It contains the following files:

1. `replicationScript.R` This is the principal script in the R programming language that loads in the aggregated data and runs the replication regressions.
2. `olsReplicationUtilities.R` This file contains useful functions that manipulate inputs to produce regression result tables.
3. `2015data_summarystats.RData` This data file contains the means and covariances of all variables in the 2015 edge-level dataset, its sample size, as well as regression estimate robust variance-covariance matrices for all the OLS regressions with the 2015 data.
4. `2019data_summarystats.RData` This data file contains the means and covariances of all variables in the 2019 node-level dataset, its sample size, as well as regression estimate robust variance-covariance matrices for all the OLS regressions with the 2019 data.
