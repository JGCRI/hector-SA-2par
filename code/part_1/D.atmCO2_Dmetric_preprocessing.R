# Purpose: Use NOAA CO2 data obtained from ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt 
# to as the observational points of comparison to calculate the Dn metric. The Dn calculations will need 
# a data frame containing hector values, observational values and an observational data variability value.


# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.

# 0. Set Up --------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(purrr)


# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_q10_only'

}


script_name <- 'D.atmCO2_Dmetric_preprocessing.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)


INT_OUTPUT_DIR <- file.path(BASE, 'out-1', sub_dir)


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

hector_co2_path <- list.files(file.path(BASE, 'out-1', sub_dir), 'C.Ca_hector_run_cleanup.csv', full.names = T)
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

output_file <- file.path(BASE, 'out-1', sub_dir, 'D.atmCO2_Dmetric_input_table.csv')
write.csv( CO2_Dn_input_table, output_file, row.names = F )

message(seperator)