
# Purpose: This script processes the HadCRUT annual global temperature ensemble members in preperation 
# to create the temperature Dn metric table. This script uses data that is too large to 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.

# 0. Set Up -------------------------------------------------------------------------------------------
# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_4_params'

}


library(purrr)
library(dplyr)
source(file.path(BASE, 'code', 'part_1', 'D.0.Dmetric_preprocessing_functions.R')) # The simga2 function


script_name <- 'D.Tgav_Dmetric_preprocessing.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)



# 1. Import Data & Load Functions --------------------------------------------------------------------
# This script uses "HadCRUT4 time series: ensemble members" avaibale from
# https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html

# Extract the individual enesmble members from the zip files. 
extract_to <- file.path(BASE, 'input', 'observations', 'HadCRUT.4.6.0.0.annual_ns_avg_realisations')

dir.create(extract_to, showWarnings = FALSE)

unzip(zipfile = file.path(BASE, 'input', 'observations', 'HadCRUT.4.6.0.0.annual_ns_avg_realisations.zip'), 
      exdir = extract_to)

# Import and concatenate all of the contents of the extracted zip file. 
concatenated_HadCRUT <- map_dfr(list.files(extract_to, full.names = T), read.table)


# Contents of the files are described at https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/ensemble_series_format.html
# Use information from the html to name the columns of the concatenated data frame.
names(concatenated_HadCRUT) <- c('year', 'value', 'sigma', 'sigma level of fully uncorrelated', 
                                 'sigma level of partially correlated measurement and sampling uncertainty', 
                                 'sigma level of uncertainty associated with limited global/regional coverage')


# 2. Calculate HadCRUT mean --------------------------------------------------------------------------------------

# Subset the HadCRUT data so that it only contains values from 1950 and onwards. 
concatenated_HadCRUT_1950 <- filter(concatenated_HadCRUT, year >= 1950, year <= 2016)
#concatenated_HadCRUT_1950 <- filter(concatenated_HadCRUT, year >= 1950, year <= 2005)

# Calcualte the HadCRUT annual ensemble mean valuue and the s2n from the sigma. 
concatenated_HadCRUT_1950 %>% 
  group_by(year) %>% 
  summarise(obs = mean(value), s2n = mean(sigma) ^ 2) %>% 
  ungroup %>% 
  mutate(sigma2 = sigma2(data = ., sd_coef = 2, use_rolling_sd = TRUE)) -> 
  obs_data



# 3. Calibrate Hector Tgav ----------------------------------------------------------------------------------------

# Since the HadCRUT data uses 1961-1990 as the reference period, Hector Tgav must also be calibrated to this
# reference period, see https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/ensemble_series_format.html. 

# Import Hector temperature. 
full_Hector_temp <- read.csv(file.path(BASE, 'out-1', sub_dir, 'C.Tgav_hector_run_cleanup.csv'), stringsAsFactors = FALSE)


# Find the average value for eacch run durring the reference year. 
full_Hector_temp %>% 
  filter(year %in% 1961:1990) %>% 
  group_by(run_name) %>% 
  summarise(ref_value = mean(as.numeric(value))) %>% 
  ungroup -> 
  reference_values_df


# Remove the reference values from the Hector temperature. 
full_Hector_temp %>%
  filter(year %in% obs_data$year) %>% 
  left_join(reference_values_df, by = 'run_name') %>% 
  mutate(model = as.numeric(value) - ref_value) %>% 
  select(run_name, year, model) -> 
  Hector_calibrated_temp
  

# 4. Create Dn metric input ----------------------------------------------------------------------------------------

# Join the calibrated Hector data and the observational data together in a single data frame. 
Hector_calibrated_temp %>%  
  left_join(obs_data, by = 'year') -> 
  Tgav_Dn_metric_input_table

output_file <- file.path(BASE, 'out-1', sub_dir, 'D.Tgav_Dmetric_input_table.csv')
write.csv(Tgav_Dn_metric_input_table, output_file, row.names = F)

message(seperator)
