# E.0.Dmetric_functions.R
#
# Purpose: this script defines the functions used to calculate the Dn and Dc metrics for 
# the hector-SA-npar analysis. 

# 0. Set Up ----------------------------------------------------------------------------------------------

# Required libs
library(zoo)
library(dplyr)

# 1. Functions -------------------------------------------------------------------------------------------
# Dn_func 
# is a function that will calculate the Dn distance stat for an observation and model 
# comparison. The input data table requires the run_name, observational and model data, and a 
# s2n column that contains a sample size variability. 

Dn_func <- function(data){
  
  # Check for required columns
  req_columns <- c('run_name', 'obs', 'model', 's2n')
  missing     <- req_columns[!req_columns %in% names(data)]
  if( length(missing) >= 1 ){stop("Missing ", paste(missing, collapse = ", "), " from data frame." )}
  
  # Calculate Dn 
  n  <- nrow(data)
  Dn <- (1 / n) * sum( ( (data$obs - data$model) ^ 2 ) / data$s2n)
  
  # Return Dn as a tibble
  run_name <- unique(data$run_name)
  tibble(run_name = run_name, Dn = Dn)
  
}


# Dc_func 
# is a function that will calculate the Dn metric cut off value. This function requires an observational 
# time series and the observational error values. This function will calculate b and a for the gama
# distribution from the input data.

Dc_func <- function(data, alpha){
  
  # Check for required columns
  req_columns <- c('obs', 's2n')
  missing     <- req_columns[!req_columns %in% names(data)]
  if( length(missing) >= 1 ){stop("Missing ", paste(missing, collapse = ", "), " from data frame." )}
  
  # Define the shape parameter
  n <- nrow(data)
  a <- n / 2 
  
  # Define the scale parameter
  rolling_sd <- rollapply(data$obs, width = 5, FUN = function(x){ 2 * sd(x) }, na.pad = T)
  sigma_sqrd <- mean( rolling_sd , na.rm = T ) ^ 2
  b          <- (1/n^2) * sum(sigma_sqrd / data$s2n)
  
  # Find Dc based on the alpha percentile
  Dc <- qgamma((1 - alpha), shape = a, scale = b)
  
  # Return Dc
  tibble(Dc = Dc, sigm_squared = sigma_sqrd, b = b, alpha = alpha)
  
}