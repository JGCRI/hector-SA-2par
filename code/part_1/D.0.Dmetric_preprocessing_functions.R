
# Purpose: The functions that are used to help make the Dn meteric input tables. 

# 0. Set Up --------------------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(zoo)


# sigma2 function --------------------------------------------------------------------------------------------------

# sigma2: is the function that calculates the sigma 2 values to be used in the Dn metric calculations. This 
# functions requires a tibble continaing a single set of observational values, the coefficent to mulitply the 
# standard deviation by, a boolean to indicate weather or not the rolling standard deviation should be 
# used or not. The rolling standard deviation should be used in the long time series with the competing long term 
# and cliamte variabtility signals. 

sigma2 <- function(data, sd_coef = 2, use_rolling_sd = TRUE){
  
  # Check for required columns
  req_columns <- c('obs', 's2n')
  missing     <- req_columns[!req_columns %in% names(data)]
  if( length(missing) >= 1 ) stop("Missing ", paste(missing, collapse = ", "), " from data frame." )
  
  # Define the scale parameter
  if(use_rolling_sd){
    
    rolling_sd <- rollapply(data$obs, width = 5, FUN = function(x){ sd_coef * sd(x) }, na.pad = T)
    sigma_sqrd <- mean( rolling_sd , na.rm = T ) ^ 2
    
  } else {
    
    sigma_sqrd <-  (sd_coef * sd(data$obs)) ^ 2
    
  }
  
  sigma_sqrd
  
}
