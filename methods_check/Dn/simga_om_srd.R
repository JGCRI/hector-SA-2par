# Purpose: Is to look at the detrending methods and figure out which methodlogy will 
# give us the varience we want to include in the sigma(o-m)^2. 

# Notes: this was not the best way to go about exploring methods. Not reproduceable and 
# is very confusing. But also hard becasue we are using so much data.

# 0. Set Up ---------------------------------------------------------------------------
# Load required libs
library( 'tidyr' )
library( 'readxl' )
library( 'forecast' )
library( 'dplyr' )
library( 'zoo' )
library( 'purrr' )

# Define the intermediate output sub directory
sub_dir <- 'rcp26'

# Set up directoires 
BASE           <- getwd() 
INT_OUTPUT_DIR <- file.path(BASE, 'int-out', sub_dir)

# 1. Define Functions ------------------------------------------------------------------------
# Dn_func: is a function that will calculate the Dn distance stat for an observation and model 
# comparison. The input data table requires the run_name, observational and model data, and a 
# s2n column that contains a sample size variability. 

Dn_func <- function(data){
  
  # Check for required columns
  req_columns <- c('run_name', 'obs', 'model', 's2n')
  if( any(!req_columns %in% names(data)) ){stop("Missing 1 or more required columns.")}
  
  # Calculate Dn 
  n  <- nrow(data)
  Dn <- (1 / n) * sum( ( (data[['obs']] - data[['model']]) ^2 ) / data[['s2n']])
  
  # Return Dn as a tibble
  run_name <- unique(data$run_name)
  tibble(run_name = run_name, Dn = Dn)
  
}


# 2. Import Dn input tables ----------------------------------------------------------------
# Import the various Dn files that were created in part G.

path <- list.files(INT_OUTPUT_DIR, "G.mean_Tgav_Dn_input_table.csv", full.names = T)
Tgav_Dn_input <- read.csv(path, stringsAsFactors = FALSE)

path <- list.files(INT_OUTPUT_DIR, "G.NOAA_CO2_Dn_input_table.csv", full.names = T)
CO2_Dn_input <- read.csv(path, stringsAsFactors = FALSE)

path <- list.files(INT_OUTPUT_DIR, "G.CDIAC_LandFlux_Dn_input_table.csv", full.names = T)
LandFlux_Dn_input <- read.csv(path, stringsAsFactors = FALSE)



# # What is bigger the varience or the standard deviation?? 
# Tgav_sd  <- sd(Tgav_Dn_input$obs)
# Tgav_var <- var(Tgav_Dn_input$obs)
# 
# Tgav_sd^2  == Tgav_var

# Okay so varience is sd * sd... which is really annoying when sd is a fraction... because 
# squaring it makes it smaller! So I think that I would like to try the Dn metrics 
# using the sd. In theory I don't think is really matters so long as we are consistent 
# with the values we use... 


# 3. Calculate Dn ----------------------------------------------------------------
# Find the Dn for every single Hector run by mapping the Dn_func to every
# run_name data frame.

Tgav_Dn_input %>%  
  split(.$run_name) %>% 
  # Find the Dn for every single run_name by mapping the Dn_func to every
  # run_name data frame.
  map(., function(data = .){ Dn_func(data) }) %>% 
  bind_rows -> 
  Tgav_Dn

CO2_Dn_input %>% 
  split(.$run_name) %>% 
  map(., function(data = .){ Dn_func(data) }) %>% 
  bind_rows -> 
  CO2_Dn

LandFlux_Dn_input %>% 
  split(.$run_name) %>% 
  map(., function(data = .){ Dn_func(data) }) %>% 
  bind_rows -> 
  LandFlux_Dn



# # Sanity check look at the Dn results 
summary(CO2_Dn$Dn)
summary(Tgav_Dn$Dn)
# summary(LandFlux_Dn$Dn)
# 
# 
hist(Tgav_Dn$Dn, breaks = 1000)      # This is sort of what we would expect the gamma distribtuion to look like! 
hist(CO2_Dn$Dn, breaks = 1000)      # This is not really a great gamma distribtuion but I guess the shape does vary
# hist(LandFlux_Dn$Dn, breaks = 1000) # This is a strange looking gamma distribtion as well, it looks exponential. 
# 


# 4. Calculate Dc ----------------------------------------------------------------

# First we are going to have to create the Dc inptus tables by adding the 
# Dn column to to Dn input tables. 

Tgav_Dc_input     <- full_join(Tgav_Dn_input, Tgav_Dn, by = "run_name")
CO2_Dc_input      <- full_join(CO2_Dn_input, CO2_Dn, by = "run_name")
LandFlux_Dc_input <- full_join(LandFlux_Dn_input, LandFlux_Dn, by = "run_name")


# Find the Dc for every single Hector run by mapping the Dc_func to every
# run_name data frame.


# Goal - what varience should we use to create the b paramter for the gamma 
# distribution??? 

plot(unique(Tgav_Dc_input$obs))
plot(unique(Tgav_Dc_input$temporal_vari))


# Find n and a based on the number of observations and 
# model points being compared.
n  <- length(unique(Tgav_Dc_input$obs))
a  <- n / 2
alpha <- 0.01

Tgav_Dc_input %>%
  filter(run_name == 'hectorSA-0001') -> 
  df

length(df$obs)

# With the moving average window of 5 we have 0 hector runs that
# are considered to be significant.
trend_temp <- ma(df$obs, order = 5, centre = T)
residuals  <- unique(obs_data$obs) - trend_temp

temporal_vari <- var(residuals, na.rm = T)


sigma_diff <- temporal_vari
b          <- (1/n^2) * sum(sigma_diff  / df[['s2n']])
Dc <- qgamma((1 - alpha), shape = a, scale = b, lower.tail = TRUE)
Tgav_Dc_input %>% 
  filter(Dn < Dc)


# With the moving average window of 100 we have a few Hector runs that are considered
# to be matching but this is not great.... HEY KALYN I THINK THAT THIS MIGHT BE IT!!!!
trend_temp <- ma(df$obs, order = 3, centre = T)
residuals  <- df$obs - trend_temp

temporal_vari <- (sd(residuals, na.rm = T) * 2 ) ^2

sigma_diff <- unique(temporal_vari)
b          <- (1/n^2) * sum(sigma_diff / df[['s2n']])
Dc <- qgamma((1 - alpha), shape = a, scale = b, lower.tail = TRUE)
Tgav_Dn %>% 
  filter(Dn < Dc) %>% 
  nrow()


# What about the rolling varience method?? --- if I really bump up the 
# rolling method then we get a handful otherwise there are none that pass thorugh... 

temporal_vari <- (rollapply(df$obs, width = 3, FUN = function(x){(sd(x) * 2)}))


mod_vari <- rollapply(df$model, width = 5, FUN = var) %>% mean



sigma_diff <- mean(temporal_vari)
b          <- (1/n^2) * sum((sigma_diff + mod_vari) / df[['s2n']])
Dc <- qgamma((1 - alpha), shape = a, scale = b, lower.tail = TRUE)
Tgav_Dn %>% 
  filter(Dn < Dc) %>% 
  nrow()



CO2_Dn_input %>% 
  filter(run_name == "hectorSA-0001") -> 
  df

n  <- length(unique(df$obs))
a  <- n / 2
alpha <- 0.01

trend_temp <- ma(df$obs, order = 3, centre = T)
residuals  <- abs(df$obs - trend_temp)

temporal_vari <- (sd(residuals, na.rm = T) * 2) ^2

temporal_vari <- mean(rollapply(df$obs, width = 5, FUN = function(x){(sd(x) * 2)^ 2}))

temporal_vari <- ((rollapply(df$obs, width = 5, FUN = function(x){(sd(x))}) %>% mean ) * 2 ) ^ 2


sigma_diff <- mean(temporal_vari)
b          <- (1/n^2) * sum(sigma_diff/ df[['s2n']])
Dc <- qgamma((1 - alpha), shape = a, scale = b, lower.tail = TRUE)
CO2_Dn %>% 
  filter(Dn < Dc) %>% 
  nrow()


# OAKY SO IT MIGHT BE THE AVERAGE 2 SD for some sliding set of years.... that then captures
# the wiggel and the trend but not too much of the trend..... IDK will need to create some sort of 
# graphic and send it to CH... 

# Okay what happens when we try to do this method with the land flux??? 

LandFlux_Dn_input %>% 
  filter(run_name == 'hectorSA-0001') -> 
  df


  
n  <- length(unique(df$obs))
a  <- n / 2
alpha <- 0.01

trend_temp <- ma(df$obs, order = 3, centre = T)
residuals  <- abs(df$obs - trend_temp)

temporal_vari <- (sd(residuals, na.rm = T) * 2) ^2

temporal_vari <- mean(rollapply(df$obs, width = 5, FUN = function(x){(sd(x) * 2)^ 2}))

temporal_vari <- ((rollapply(df$obs, width = 5, FUN = function(x){(sd(x)) * 2}) %>% mean ) ) ^ 2


sigma_diff <- mean(temporal_vari)
b          <- (1/n^2) * sum(sigma_diff/ df[['s2n']])
Dc <- qgamma((1 - alpha), shape = a, scale = b, lower.tail = TRUE)
LandFlux_Dn %>% 
  filter(Dn < Dc) %>% 
  nrow()




