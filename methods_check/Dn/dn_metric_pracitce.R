# Purpose: practice using the Dn metric. 



# 0. Set Up ----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)


# 1. Example from the Booker 2006 paper ------------------------------------------------

# Temperature 
k <- 2 
n <- 5 
s2n <- 90.0

obs_temp <- c(75, 59, 47, 72, 62)
cal_temp <- c(77, 62, 47.3, 73, 65)


 dn_temp <- (1 / (n - k) ) * sum( ((obs_temp - cal_temp)^2) / s2n)


# Dew Point 
 k <- 2 
 n <- 5 
 s2n <- 26.0 
 
 obs_dew <- c(55.6, 53.2, 40, 55, 53)
 cal_dew <- c(54, 53, 41.5, 54.4, 55)
 
 
 dn_dew <- (1 / (n - k) ) * sum( ((obs_dew - cal_dew)^2) / s2n)
 
 
# Yay both dn temp and dew match those in the paper! 
 
 
 # Harlingen 
 
k <- 2
n <- 4 
s2n <- c(90.0, 26.0, 4.1, 6.3)

obs_city <- c(75, 55.6, 1015.3, 23)
cal_city <- c(77, 54, 1013.6, 30)


data <- data.frame(obs_city, cal_city, s2n)


apply(data, 1, function(data, n = 4, k = 2){ 
  (1 / (n - k) ) * sum( ((data[['obs_city']] - data[['cal_city']])^2) / data[['s2n']]) %>% 
    round(digits = 2)
    }) %>% 
  sum
# Dn is off by 0.04


# DFW
obs_city <- c(59, 53.2, 1014.1, 15)
cal_city <- c(62, 53, 1013.9, 10)

data <- data.frame(obs_city, cal_city, s2n)


apply(data, 1, function(data, n = 4, k = 2){ 
  (1 / (n - k) ) * sum( ((data[['obs_city']] - data[['cal_city']])^2) / data[['s2n']]) %>% 
   # round(digits = 5)
    signif(digits = 8)
}) %>% 
  sum

# Now my value is off by 0.01, I have come to the conculsion that results are being affected by 
# some different rounding errors.







# 2. Gamma Distribtuion from Booker 2006 paper -----------------------------------------

# Ah these match the plots in the 2006 paper! whoot whoot!! 

# dgamma is the PDF 
# pgamma is the CDF
x <- seq(0, 40, length.out = 1000)
values_d <- dgamma(x, shape = 2.5, scale = 0.53)


# Talked to As blanked that integreation is the sum * bin width 
# So sum(dgamma output * unique(diff(x))) should approximate to 1 


# Okay so now it is a question of how do we go from the probability 
# distribtuion to the cutt off value? 

x <- 0:1000
CDF <- pgamma(x, shape = 2.5, scale = 0.922)
difference <- abs(CDF - (1 - alpha))
index <- which.min(difference)


difference[index]
x[index]






# Things we will need 
  # some x values vector which may be hard to get... really depends... 
  # a = shape 
  # b = there is a complicated equation... will need to figure out which one we want to use 


# How do we get from gamma distribtuion to the cut off value?? 



