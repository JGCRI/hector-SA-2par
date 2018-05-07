# Purpose: This script applies the moving average to the Hector temperature 
# results in preperation for the observation window filtering that will 
# occur in script D.3. Because the observational time series uses the mean temperature 
# from 1951 to 1990 as the temperature change reference period we must use the average 
# temperature from that period for Hector temperature inorder to compare the 
# observational and Hector temp. 

library( 'tidyr' )
library( 'caTools' )
library(dplyr)
library(purrr)


# This section is commented out so that the script can be sourced from the 
# run_all script. If you would like to run this script by it's self make sure that 
# the rest of the code in the set up section is not commented out.
# 0. Set Up ----------------------------------------------------------------
# # The working directory should be the project directory. 
# if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}
# 
# # Define the rcp to process
# rcpXX <- 'rcp26' 

# ---
# 1. read in processed observation and Hector data
observation <- read.csv( './int-out/observations/D.temperature_obervation_ma.csv', stringsAsFactors = F )
observation_years <- sort( unique( observation$year ) ) 


# read in Hector data and format.
hector_res  <- readr::read_csv( file.path('./int-out', rcpXX, 'C.hector_run_cleanup.csv'))

# Subset hector results for the temperature output and then 
# change data frame format from wide to long.
hector_tgav <- filter(hector_res, variable == "Tgav")

hector_tgav %>%
  select(-variable, -units) %>%
  gather(year, value, -run_name) %>%
  filter(year %in% observation_years) ->
  hector_tgav_long

# 2. Process Hector Temp  ------------------------------------------------------------------------------------------------

# In order to compare Hector temp with observational temp we will need to 
# calibrate Hector temp to the observational reference period (1951 : 1990). 
# Calculate the mean hector temperature over this period for each Hector run. 
# Then remove the reference temperature for each run. 

# Get reference temp. 
hector_tgav_long %>%  
  filter(year %in% 1951 : 1990) %>% 
  group_by(run_name) %>% 
  summarise(ref_value = mean(value)) %>% 
  ungroup -> 
  ref_temp
 
# Remove reference temp.
hector_tgav_long %>% 
  left_join(ref_temp) %>% 
  mutate(value = value - ref_value) %>% 
  select(run_name, year, value) -> 
  hector_tgav_refRemoved


# Now that the reference temperature has been removed apply the 
# moving average to each hector run. We are going to split the hector 
# data frame up by hector run and then map the moving average function to each 
# hector run, this is the most computationally efficent method to do this 50,000 times. 

# Split up the hector data frame.
hector_tgav_list <- split(hector_tgav_refRemoved, hector_tgav_refRemoved$run_name)

# Map the moving average function to each hector run data frame and then 
# concatenate results into a single data frame.
map(hector_tgav_list, function(x){ x$value <- runmean(x$value, windowYrs,
                                    alg = c( 'C' ),
                                    endrule = c( 'mean' ),
                                    align = c( 'center')); x } ) %>%
  bind_rows %>%
  arrange(run_name, year) ->
  ma_long


# Format the data frame so that it will match the expect data frame for the D.3 script. 
ma_long %>%
  mutate(year = paste0("X",year)) %>%
  spread(year, value) ->
  tgav_crop_ma


# Save ------ 
write.csv( tgav_crop_ma, file.path('./int-out', rcpXX, 'D.temperature_hector_ma.csv'), row.names = F )


