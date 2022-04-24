# R packages needed to run this script: tidyverse, lfe, metafor
setwd(getSrcDirectory()[1])

# 2015 edge level data ------------
rm(list = ls())
# Functions to compute replication estimates from summary stats of the data
source("olsReplicationUtilities.R")
load("2015data_summarystats.RData")  # Summary stats of 2015 data 
# These summary stats include:
# 1. The means and covariances of all variables in the full dataset
# 2. The sample size of the full dataset
# 3. Robust variance-covariance matrices of type `vcov` for all OLS regressions

# OLS regs for 2015 collapsed to node level data -----------
# Table 6: First stage for 2015 collapsed
# Replication of Table 6 Spec 1
ols_replication(node15_means, node15_cov, nrow_node15, 9, c(2, 4:8), 
                vcov_firststage_node15_weakest)
# Replication of Table 6 Spec 2
ols_replication(node15_means, node15_cov, nrow_node15, 10, c(2, 4:8), 
                vcov_firststage_node15_medium)
# Replication of Table 6 Spec 3
ols_replication(node15_means, node15_cov, nrow_node15, 11, c(2, 4:8), 
                vcov_firststage_node15_strongest)

# OLS regs for Table 7 ----------
# 2015 edge level first stage 
# Table 7 column 1 replication 
ols_replication(edge_means, edge_cov, nrow_edge, 2, c(6, 8:20), 
                vcov_edge_firststage_mf)
# Table 7 column 2 replication 
ols_replication(edge_means, edge_cov, nrow_edge, 4, c(6, 8:20), 
                vcov_edge_firststage_ii)

# 2015 node level relationship between tie strength and job transmission------
# Replication of Table 8 Spec 1 
ols_replication(node15_means, node15_cov, nrow_node15, 1, 9:11, 
                vcov_node15_ols)

# OLS Reg for Table 9 ------------
# Edge level relationship b/w mutual connections & job transmission 
# Table 9 column 1 replication
ols_replication(edge_means, edge_cov, nrow_edge, 1, c(2, 13:20),
                vcov_ols_mf_0)
# Table 9 column 2 replication 
ols_replication(edge_means, edge_cov, nrow_edge, 1, c(2:3, 13:20),
                vcov_ols_mf_1)

# OLS regs for Table 10 ------------
# Edge level relationship between interaction intensity and job transmission
# Column 1 
# Table 10 column 1 replication
ols_replication(edge_means, edge_cov, nrow_edge, 1, c(4, 13:20),
                vcov_ols_ii_0)
# Table 10 column 2 replication
ols_replication(edge_means, edge_cov, nrow_edge, 1, c(4:5, 13:20),
                vcov_ols_ii_1)

# 2019 node level data ------------
rm(list = ls())
source("olsReplicationUtilities.R")
load("2019data_summarystats.RData")  # Summary stats of 2019 data 
# These summary stats include:
# 1. The means and covariances of all variables in the full dataset
# 2. The sample size of the full dataset
# 3. Robust variance-covariance matrices of type `vcov` for all OLS regressions

# 2019 node level first stage replication -----------
# Replication of Table 11 Spec 1 
ols_replication(node19_means, node19_cov, nrow_node19, 9, 2:8, 
                vcov_firststage19_allties)
# Replication of Table 11 Spec 2 
ols_replication(node19_means, node19_cov, nrow_node19, 10, 2:8, 
                vcov_firststage19_weakties)
# Replication of Table 11 Spec 3
ols_replication(node19_means, node19_cov, nrow_node19, 11, 2:8, 
                vcov_firststage19_strongties)

# Main 2019 node level OLS regs -----------
# Replication of Table 12 Spec 1 
ols_replication(node19_means, node19_cov, nrow_node19, 1, 10:11, 
                vcov_node19_ols_pretreat)
# Replication of Table 13 Spec 1 
ols_replication(node19_means, node19_cov, nrow_node19, 1, 12:13, 
                vcov_node19_ols_posttreat)

