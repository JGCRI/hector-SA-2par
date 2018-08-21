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
# time series and the observational error values and the difference in model vs observational variability. 

Dc_func <- function(data, alpha, sd_coef = 2){
  
  # Check for required columns
  req_columns <- c('obs', 's2n', 'sigma2')
  missing     <- req_columns[!req_columns %in% names(data)]
  if( length(missing) >= 1 ){stop("Missing ", paste(missing, collapse = ", "), " from data frame." )}
  
  # Define the shape parameter
  n <- nrow(data)
  a <- n / 2 

  
  b <- (1/n^2) * sum(data$sigma2 / data$s2n)
  
  # Find Dc based on the alpha percentile
  Dc <- qgamma((1 - alpha), shape = a, scale = b)
  
  # Return Dc
  tibble(Dc = Dc, b = b, alpha = alpha)
  
}


# join_Dmetric
# is a function that will combine two tibbles togeter by a dummb column and then drop the dummy column, 
# use this to join the Dn and Dc results into a single tibble. 

join_Dmetric <- function(data1, data2){
  
  data1$index <- 1
  data2$index <- 1
  
  full_join(data1, data2, by = 'index') %>% 
    select(-index)
}