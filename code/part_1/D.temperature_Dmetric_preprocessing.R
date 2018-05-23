# Purpose: Use global temps data from "GISS", "NOAA", "HadCRUT4", "Berkeley" the observational points of 
# comparison to calculate the Dn metric. The Dn calculations will need a data frame containing hector values, 
# observational values and an observational data variability value.

# 0. Set Up ------------------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Load required libs
library( 'tidyr' )
library( 'readxl' )


# Select which sub directory to search 
sub_dir <- 'rcp26'

# Set up directoires 
BASE <- getwd() 
INT_OUTPUT_DIR <- file.path(BASE, 'int-out', sub_dir)

# 1. Import and Format Obs Data ------------------------------------------------------------------------

# Import and format the observational data set. 

ob_df <- read_excel( './input/observations/globaltemps.xlsx', skip = 5 )

# Since we are looking at several observational data products we are going to use the mean as the 
# comparison for the Dn metric. Remove any years that do not have data from all of the four observational records.
ob_df %>%  
  rename(year = Year) %>% 
  # Discard the entries that have NAs for some of the observations.
  na.omit %>% 
  gather(source, value, GISS, NOAA, HadCRUT4, Berkeley) %>% 
  group_by(year) %>% 
  summarise(obs = mean(value), 
            s2n = var(value)) %>% 
  ungroup -> 
  obs_data


# 2. Import and Format Hector Data --------------------------------------------------
# Because the observational time series uses the mean temperature 
# from 1951 to 1990 as the temperature change reference period we must use the average 
# temperature from that period for Hector temperature inorder to compare the 
# observational and Hector temp. 

hector_tgav_path <- list.files(INT_OUTPUT_DIR, "C.Tgav_hector_run_cleanup.csv", full.names = T)
hector_data      <- read.csv(hector_tgav_path, stringsAsFactors = FALSE)

# Find the mean temperature for each run over the observation reference period.
hector_data %>%  
  filter(year %in% 1951 : 1990, 
         variable == "Tgav") %>% 
  group_by(run_name) %>% 
  summarise(ref_value = mean(value)) %>% 
  ungroup -> 
  ref_temp

# Remove reference temp.
hector_data %>% 
  left_join(ref_temp, by = 'run_name') %>% 
  mutate(value = value - ref_value) %>% 
  select(run_name, year, value) -> 
  hector_data


# Subset the Hector data frame so that it only includes the Tgav data 
# for the same years as the observational data.  
hector_data %>% 
  filter(year %in% obs_data$year) %>% 
  rename(model = value) -> 
  hector_data


# 3. Prepare the Dn Input Data Frame -----------------------------------------------------------

hector_data %>%  
  full_join(obs_data %>% select(year, obs, s2n), by = "year") -> 
  Tgav_Dn_input_table

output_file <- file.path(BASE, 'int-out', sub_dir, 'D.temperature_Dmetric_input_table.csv')
write.csv(Tgav_Dn_input_table, output_file, row.names = F)


# End 
