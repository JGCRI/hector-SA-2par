# Purpose : Moving average on Hector atmospheric CO2 in preparation for comparison with 
# the atmopshirc CO2 from NOAA.

# Libraries
library(tidyr)
library(caTools)
library(dplyr)
library(purrr)


# This section is commented out so that the script can be sourced from the 
# run_all script. If you would like to run this script by it's self make sure that 
# the rest of the code in the set up section is not commented out. 
# # 0. Set Up -------------------------------------------------------
# 
# # The working directory should be the project directory. 
# BASE <- getwd()
# if(!(basename(BASE) == 'hector-SA-npar')){stop('working directory should be the project directory')}
# 
# # Define the rcp to process
# rcpXX <- "rcp26"
# 
# # Define the length of years to use in the moving average window
# windowYrs <- 15 


# 1. Import and Format Data ---------------------------------------------------------------

# read in processed observations
observation <- readr::read_csv( './int-out/observations/G.NOAA_CO2_observation_ma .csv' )
observation_years <- sort( unique( observation$year ) ) 

# read in hector CO2 
hector_res <- readr::read_csv( file.path(BASE, 'int-out', rcpXX, 'C.hector_run_cleanup.csv') )

# SUbset the data so that it only includes CO2 atm observations for the years within the 
# observational data set. Format the data frame so that variables are consistent with the 
# observational data set.
hector_res %>%  
  filter(variable == "Ca") %>% 
  gather(year, value, -run_name, -variable, -units) %>% 
  filter(year %in% observation_years) %>% 
  mutate(variable = unique(observation$variable), 
         units  = unique(observation$units), 
         year = as.integer(year)) -> 
  hector_data


# 2. Apply Moving Average ---------------------------------------------------------------

# Apply the moving average to every hector run seperatly.
split(hector_data, hector_data$run_name) %>% 
  map(function(x){ x$value <- runmean(x$value, windowYrs, 
                                      alg = c( 'C' ), 
                                      endrule = c( 'mean' ), 
                                      align = c( 'center')); x } ) %>%  
  bind_rows %>% 
  arrange(run_name, year) -> 
  hector_ma
  
write.csv( hector_ma, file.path(BASE, 'int-out', rcpXX, 'G.co2_hector_ma.csv'), row.names = F )

# End 
