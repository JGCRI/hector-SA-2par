# Purpose: Use NOAA CO2 data obtained from ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt 
# to create the observtions, moving average window to process Hector atmospheric CO2

# Notes I am going to need to spell check and also determine what size of window I want to use.


# Load the libs
library(tidyr)
library(caTools)
library(dplyr)

# This section is commented out so that the script can be sourced from the 
# run_all script. If you would like to run this script by it's self make sure that 
# the rest of the code in the set up section is not commented out. 
# # 0. Set Up ----------------------------------------------------------------
# 
# # The working directory should be the project directory. 
# BASE <- getwd()
# if(!(basename(BASE) == 'hector-SA-npar')){stop('working directory should be the project directory')}
# 
# 
# # Define the length of years to use in the moving average window
# windowYrs <- 15 

# Read in the NOAA observations. -- I think that when you comment this code you will need to add 
# some information about the source and what we are pulling out... Make sure that you know what 
# values you are actually using becasue you may be pulling out the wrong one 
obs_path    <- file.path(BASE, "input", "observations", "NOAA_co2_mm_mlo.txt")
data        <- read.table(obs_path) 
names(data) <- c("year", "month" , "decimal_date", "average", "interpolated", "trend", "days")


# 1. Clean Up Data ----------------------------------------------------------------

# The data frame does not contain any missing values or NAs so no gap filling is needed. 
# We want to process only the years with a full 12 months, the only years that are 
# incomplete are the 1958 and 2018, the first data collection year and the present year.

# Subset the data so that it only includes the complete years.
complete_years <- 1959:2017
data           <- filter(data, year %in% complete_years)


# Aggregate to annual values and save the standard deviation, use the interplated 
# values. Define the upper and lower boundaries by +/- 2 sd of the average. 
data %>% 
  group_by(year) %>% 
  summarise(value = mean(interpolated), sd = sd(interpolated)) %>%  
  ungroup %>%
  mutate(min_value = value - 2 * sd, 
         max_value = value + 2 * sd, 
         variable = "atm_CO2", 
         units = "ppm") -> 
  annual_data
  


# 2. Apply Moving Average ----------------------------------------------------------------

# Apply the moving average on the 

NOAA_CO2_ma <- annual_data

NOAA_CO2_ma$min_value <- runmean(annual_data$min_value, windowYrs, 
                                 alg = c( 'C' ), 
                                 endrule = c( 'mean' ), 
                                 align = c( 'center' ) )

NOAA_CO2_ma$max_value <- runmean(annual_data$max_value, windowYrs, 
                                 alg = c( 'C' ), 
                                 endrule = c( 'mean' ), 
                                 align = c( 'center' ) )

NOAA_CO2_ma$value     <- runmean(annual_data$value, windowYrs, 
                                 alg = c( 'C' ), 
                                 endrule = c( 'mean' ), 
                                 align = c( 'center' ) )


write.csv( NOAA_CO2_ma, './int-out/observations/G.NOAA_CO2_observation_ma .csv', row.names = F )

