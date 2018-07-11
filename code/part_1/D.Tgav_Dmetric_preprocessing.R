
# Purpose: This script processes the HadCRUT annual global temperature ensemble members in preperation 
# to create the temperature Dn metric table. This script uses data that is too large to 

# 0. Set Up -------------------------------------------------------------------------------------------

# The working direcotry should be the project location.
if( ! "hector-SA-npar.Rproj" %in% list.files() ) {
  stop('Working directory must be set to the project location')
  }

# Load libs
library(purrr)
library(dplyr)

# Diectories
BASE    <- getwd()   # The BASE name direcotry, should be the working direcoty (where the project is located).
sub_dir <- 'rcp26'   # The name of the out-1/sub_dir to process data from and write data out to 

# 1. Import Data --------------------------------------------------------------------------------------

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
concatenated_HadCRUT_1950 <- filter(concatenated_HadCRUT, year >= 1950, year <= 2017)


# Calcualte the HadCRUT annual ensemble mean valuue and the s2n from the sigma. 
concatenated_HadCRUT_1950 %>% 
  group_by(year) %>% 
  summarise(obs = mean(value), s2n = mean(sigma) ^ 2) %>% 
  ungroup -> 
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
  summarise(ref_value = mean(value)) %>% 
  ungroup -> 
  reference_values_df


# Remove the reference values from the Hector temperature. 
full_Hector_temp %>%
  filter(year %in% obs_data$year) %>% 
  left_join(reference_values_df, by = 'run_name') %>% 
  mutate(model = value - ref_value) %>% 
  select(run_name, year, model) -> 
  Hector_calibrated_temp
  

# 4. Create Dn metric input ----------------------------------------------------------------------------------------

# Join the calibrated Hector data and the observational data together in a single data frame. 
Hector_calibrated_temp %>%  
  left_join(obs_data, by = 'year') -> 
  Tgav_Dn_metric_input_table

output_file <- file.path(BASE, 'out-1', sub_dir, 'D.Tgav_Dmetric_input_table.csv')
write.csv(Tgav_Dn_metric_input_table, output_file, row.names = F)


# End 