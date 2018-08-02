# Purpose: Calculate the Dn statistics for each Hector run and the Dc cut off value. 
# This script requires functions defined in E.0.Dmetric_functions. This calculates the 
# Dn metric scores for each variable / observational product independently. 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.


# 0. Set Up ------------------------------------------------------------------------
library(purrr)
library(dplyr)

# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_q10_only'
}

script_name <- 'E.1.Dmetric_independent_script.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)


OUTPUT_DIR <- file.path(BASE, 'out-1', sub_dir)

# Load the D metric functions, (Dn_func, Dc_func)
source(file.path(BASE, 'code', 'part_1', 'E.0.Dmetric_functions.R'))


# 1. Temperature Only ---------------------------------------------------------------
# Calcualte the Dn and the Dc metrics to compare Hector Tgav with the observational 
# temperature record. 

# Import the temperature data. 
Tgav_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.Tgav_Dmetric_input_table.csv'), 
                          stringsAsFactors = FALSE ) 

# Calculate the Dn value for each Hector run 
Tgav_Dn_input %>%
  split(.$run_name) %>% 
  map_dfr(function(data = .x){ Dn_func(data) }) -> 
  Tgav_Dn_values


# Subset the Dn input so that it only includes entries for one Hector run, so that 
# is only contains one set of observational values. 
Tgav_Dn_input %>% 
  filter(run_name == 'hectorSA-0001') %>% 
  select(year, obs, s2n) %>% 
  Dc_func(alpha = 0.05, sd_coef = 2) %>% 
  mutate(variable = 'Tgav') ->
  Tgav_Dc


# Add the Dc data to the Dn data frame. 
Tgav_Dn_values$index <- 1 
Tgav_Dc$index        <- 1

Tgav_Dn_values %>% 
  left_join(Tgav_Dc, by = 'index') %>% 
  select(-index) -> 
  Tgav_Dmetric_results

# 2. Atmospheric CO2 Only ---------------------------------------------------------------
# Calcualte the Dn and the Dc metrics to compare Hector Ca with the observational 
# atmospheric CO2 record. 

# Import the temperature data. 
atmCO2_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.atmCO2_Dmetric_input_table.csv'), 
                          stringsAsFactors = FALSE ) 

# Calculate the Dn value for each Hector run 
atmCO2_Dn_input %>%
  split(.$run_name) %>% 
  map_dfr(function(data = .x){ Dn_func(data) }) -> 
  atmCO2_Dn_values


# Subset the Dn input so that it only includes entries for one Hector run, so that 
# is only contains one set of observational values. 
atmCO2_Dn_input %>% 
  filter(run_name == 'hectorSA-0001') %>% 
  select(year, obs, s2n) %>% 
  Dc_func(alpha = 0.05) %>% 
  mutate(variable = 'atm CO2') ->
  atmCO2_Dc


# Add the Dc data to the Dn data frame. 
atmCO2_Dn_values$index <- 1 
atmCO2_Dc$index        <- 1

atmCO2_Dn_values %>% 
  left_join(atmCO2_Dc, by = 'index') %>% 
  select(-index) -> 
  atmCO2_Dmetric_results

# 3. LandFlux CO2 Only ---------------------------------------------------------------
# Calcualte the Dn and the Dc metrics to compare Hector land flux with the observations
# from the global carbon project data.

# Import the temperature data.
LandFlux_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.LandFlux_Dmetric_input_table.csv'),
                            stringsAsFactors = FALSE )

# Calculate the Dn value for each Hector run
LandFlux_Dn_input %>%
  split(.$run_name) %>%
  map_dfr(function(data = .x){ Dn_func(data) }) ->
  LandFlux_Dn_values


# Subset the Dn input so that it only includes entries for one Hector run, so that
# is only contains one set of observational values.
LandFlux_Dn_input %>%
  filter(run_name == 'hectorSA-0001') %>%
  select(year, obs, s2n) %>%
  Dc_func(alpha = 0.05) %>%
  mutate(variable = 'Land Flux') ->
  LandFlux_Dc


# Add the Dc data to the Dn data frame.
LandFlux_Dn_values$index <- 1
LandFlux_Dc$index        <- 1

LandFlux_Dn_values %>%
  left_join(LandFlux_Dc, by = 'index') %>%
  select(-index) ->
  LandFlux_Dmetric_results

# 4. NPP Only ---------------------------------------------------------------------------

# Import the temperature data. 
NPP_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.NPP_Dmetric_input_table.csv'), 
                              stringsAsFactors = FALSE ) 

# Calculate the Dn value for each Hector run 
NPP_Dn_input %>%
  split(.$run_name) %>% 
  map_dfr(function(data = .x){ Dn_func(data) }) -> 
  NPP_Dn_values


# Subset the Dn input so that it only includes entries for one Hector run, so that 
# is only contains one set of observational values. 
NPP_Dn_input %>% 
  filter(run_name == 'hectorSA-0001') %>% 
  select(year, obs, s2n) %>% 
  Dc_func(alpha = 0.05, use_rolling_sd = FALSE) %>% 
  mutate(variable = 'NPP') ->
  NPP_Dc


# Add the Dc data to the Dn data frame. 
NPP_Dn_values$index <- 1 
NPP_Dc$index        <- 1

NPP_Dn_values %>% 
  left_join(NPP_Dc, by = 'index') %>% 
  select(-index) -> 
  NPP_Dmetric_results



# 5. Save Outputs ------------------------------------------------------------------------

# write.csv(Tgav_Dmetric_results, file = file.path(OUTPUT_DIR, 'E.Tgav_Dmetric_results'), row.names = FALSE)
# write.csv(atmCO2_Dmetric_results, file = file.path(OUTPUT_DIR, 'E.atmCO2_Dmetric_results'), row.names = FALSE)
# write.csv(LandFlux_Dmetric_results, file = file.path(OUTPUT_DIR, 'E.LandFlux_Dmetric_results'), row.names = FALSE)

# Save as a large file
Dmetric_results <- bind_rows(Tgav_Dmetric_results, atmCO2_Dmetric_results, NPP_Dmetric_results)
write.csv(Dmetric_results, file = file.path(OUTPUT_DIR, 'E.all_Dmetric_independent_results.csv'), row.names = FALSE)

message(seperator)


