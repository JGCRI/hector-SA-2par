# Purpose: add flags to the runs that meeting the window filtering method requirements 
# for atmospheric CO2 observations from NOAA.

# Load Libs
library(tidyr)
library(dplyr)

# This section is commented out so that the script can be sourced from the 
# run_all script. If you would like to run this script by it's self make sure that 
# the rest of the code in the set up section is not commented out. 
# # 0. Set Up ----------------------------------------------------------------
# 
# # The working directory should be the project directory. 
# BASE <- getwd()
# if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}
# 
# # Define the rcp to process
# rcpXX <- 'rcp26'
# 
# # The percent of Hecotr run years that must fall within the observation range in order
# # for the run to pass the fitler.
# fall_in_threshold <- 0.7 


# 1. read in moving average observations and hector co2 --------------------------------------
observations <- readr::read_csv(file.path(BASE, 'int-out', "observations", 'G.NOAA_CO2_observation_ma .csv'))
hector_co2   <- readr::read_csv(file.path(BASE, 'int-out', rcpXX, 'G.co2_hector_ma.csv'))

# Rename the columns in the data frames to prevent joining problems latter on. 
hector_co2   <- rename(hector_co2, hector_value = value)
observations <- select(observations, min_value, max_value, variable, units, year)


# 2. Fall in tally --------------------------------------------------------------------------

# Join the hector and observation data together by variable, units, and year. Check to see if 
# the hector value for each year fall within the observational value range with 1,0 indicators 
# for true and false. Add up the total number of hector years that fall within the the obs range
# for the tally count. 
hector_co2 %>% 
  left_join(observations, by = c("variable", "units", "year")) %>%
  mutate(fall_in = if_else(hector_value >= min_value & hector_value <= max_value, 1, 0)) %>% 
  group_by(run_name) %>% 
  summarise(tally_count = sum(fall_in)) %>% 
  ungroup -> 
  hector_tally 


# Save a vector of the unqiue years used  in the observational data set.
filter_year_list <- unique( observations$year )

# Determine the threshold for the tally count as the fall in treshold * the number 
# of yeras in the data set. TThe count threshold is equal to the number of years
# for a hector run that must fall within the observational data in order for the 
# run to pass the CO2 filtering method.
count_threshold <- fall_in_threshold * length(filter_year_list)


# Subset the tally data frame so that it only includes the hector runs that pass the 
# Co2 observation filtering. 
CO2_select_run_names <- hector_tally$run_name[hector_tally$tally_count > count_threshold]


# 3. Update the filter_flag.csv --------------------------------------------------------

# Import the filter flag csv file
filter_flag <- readr::read_csv( file.path('./int-out', rcpXX, 'filter_flag.csv'))

# If the run name is in the CO2 selected run names vecotr it means that the hector run 
# has passed the CO2 observational filtering method. Add a 1 to the atmCO2_flag column 
# other wise flag = 0. 
filter_flag$atmCO2_flag <- 0  
filter_flag$atmCO2_flag <- ifelse( filter_flag$run_name %in% CO2_select_run_names, 1, 0 )

# Save 
write.csv( filter_flag, file.path('./int-out', rcpXX, 'filter_flag.csv'), row.names = F )

