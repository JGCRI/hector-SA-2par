# Purpose: Use NOAA CO2 data obtained from ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt 
# to as the observational points of comparison to calculate the Dn metric. The Dn calculations will need 
# a data frame containing hector values, observational values and an observational data variability value.


# 0. Set Up --------------------------------------------------------------------------------------

# Load required libraries
library(tidyr)
library(dplyr)


# Define the intermediate output sub directory
sub_dir <- 'rcp26'


# Set up directoires 
BASE <- getwd() # should be equal to the proect location 
if(basename(BASE) != "hector-SA-npar"){stop("Working directory should be project location.")}
INT_OUTPUT_DIR <- file.path(BASE, 'int-out', sub_dir)


# 1. Import and Format Obs Data ------------------------------------------------------------------

# Import the NOAA atmospheric CO2 data.
obs_path        <- file.path(BASE, "input", "observations", "NOAA_co2_mm_mlo.txt")
obs_data        <- read.table(obs_path) 
names(obs_data) <- c("year", "month" , "decimal_date", "average", "interpolated", "trend", "days")


# The data frame does not contain any missing values or NAs so no gap filling is needed. 
# We want to process only the years with a full 12 months, the only years that are 
# incomplete are the 1958 and 2018, the first data collection year and the current year.

# Subset the data so that it only includes the complete years.
complete_years <- 1959:2017
obs_data       <- filter(obs_data, year %in% complete_years)


# Aggregate to annual values and save the annual variation. 
obs_data %>% 
  group_by(year) %>% 
  summarise(obs = mean(interpolated), 
            s2n = var(interpolated)) %>%  
  ungroup %>%
  mutate(variable = "atm_CO2", 
         units = "ppm") -> 
  annual_obs_data


# 2. Import and Format Hector Data ---------------------------------------------------------------

hector_co2_path <- list.files(file.path(BASE, 'int-out', sub_dir), 'C.Ca_hector_run_cleanup.csv', full.names = T)
hector_data     <- readr::read_csv(hector_co2_path)

# Ensure that the data frame only contains the Ca variable and the same years from the 
# as the observation data set.
hector_data %>% 
  filter(variable == "Ca") %>% 
  filter(year %in% unique(annual_obs_data$year)) %>%  
  rename(model = value) ->
  hector_data


# 3. Prepare the Dn Input Data Frame -----------------------------------------------------------

hector_data %>%  
  full_join(annual_obs_data %>% select(year, obs, s2n), by = "year") -> 
  CO2_Dn_input_table

output_file <- file.path(BASE, 'int-out', sub_dir, 'D.atmCO2_Dmetric_input_table.csv')
write.csv( CO2_Dn_input_table, output_file, row.names = F )


# End 