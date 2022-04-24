library(tidyverse)
library(lfe)
library(metafor)

# Cluster robust SEs -----------
#' regtable_robust_se 
#' 
#' Replicates OLS regression with clustered or robust standard errors. It takes 
#' point estimates computed using means and covariances of all the variables 
#' in the data (computed by `matreg` from the package `metafor`) and adjusts
#' standard errors using a user-provided regression estimate variance-covariance
#' matrix. 
#' 
#' @param homosked_ols An object of type `matreg`, created using the means and 
#' covariances of all the variables in the data.
#' @param vcov_mat An R variance-covariance matrix of regression estimates,
#' typically obtained by a call to the function `vcov` or similar. 
#' @param sample_size The number of rows in the dataset used to run the 
#' regression we seek to replicate here. 
#' 
#' @return A table with regression point estimates obtained from `matreg`,
#' robust standard errors as provided in `vcov_mat`, and corresponding t-stats 
#' and p-values. 
#' 
regtable_robust_se <- function(homosked_ols, vcov_mat, sample_size) {
  vcov_mat %>% diag() %>% sqrt() -> se  # Robust standard errors 
  homosked_ols$tab[, "beta"] -> point_estimate  # Point estimates from matreg
  t_stat <- point_estimate / se  # t-statistics
  df <- sample_size - length(se)  # Degrees of freedom
  p_val <- 2*(pt(abs(t_stat), df, lower.tail = FALSE))  # Two-sided p-values 
  cbind(point_estimate, se, t_stat, p_val)
}

# Full OLS replication, including homoskedastic and robust standard errors------
#' ols_replication 
#' 
#' Provides a full homoskedastic and robust SE replication of an OLS regression.
#' The inputs are the means and covariances of all variables in the dataset, as
#' well as its sample size. These are used to provide a full replication of 
#' OLS estimates and homoskedastic inference. For an additional input of the 
#' `vcov` matrix of robust standard errors, the function also provides a summary
#' table of the OLS regression with robust standard errors and inference. 
#' 
#' @param data_means A vector with sample means of all variables in the dataset.
#' @param data_cov A matrix with sample variances and covariances of all 
#' variables in the dataset. 
#' @param sample_size The number of rows in the dataset on which the regression
#' was originally computed. 
#' @param outcome_index An integer identifying which variable in `data_means` 
#' (as well as `data_cov`) is the outcome variable in the regression. 
#' @param regressors_index A vector of integers identifying which variables in 
#' `data_means` (as well as `data_cov`) are the regressors in the regression. 
#' @param robust_cov The variance-covariance matrix of the regression estimates
#' with robust standard errors. It is of type `vcov`. 
#' 
#' @return The function does not return anything, but it prints two regression
#' tables. The first is OLS point estimates and homoskedastic standard errors,
#' which are computed with just sample size, variable means and covariances of 
#' the data. The second table takes the additional input of a robust SE `vcov`
#' matrix and prints the corresponding regression table with robust standard 
#' errors. 
ols_replication <- function(data_means, data_cov, sample_size, 
                            outcome_index, regressors_index, robust_cov) {
  print("Homoskedastic reg replication with just raw data summary stats")
  matreg(outcome_index, regressors_index, R = data_cov, n = sample_size, 
         cov = TRUE, means = data_means) -> homosked_ols
  homosked_ols$tab[, c("beta", "se", "tval", "pval")] -> homosked_ols_table
  colnames(homosked_ols_table) <- c("point_estimate", "se", "t_stat", "p_val")
  print(homosked_ols_table)
  
  print("Robust SEs replication with additional regression vcov matrix input")
  print(regtable_robust_se(homosked_ols, robust_cov, sample_size))
}


