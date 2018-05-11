# Purpose: If we play around with the moving average length and the percent threshold to see if we can get any of the 
# rcp 26 runs to pass through the atm CO2 and land flux filter. 



# 0. Set Up -------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Define the directories
BASE <- getwd() # working directory should be set equal to the project location
if(basename(BASE) != "hector-SA-npar"){stop("Working directory should be the project location.")}

# Pick which data to process, the sub dir name in the int-out dir to process. 
sub_dir <- "rcp26"
rcpXX   <- sub_dir

# Define the input and output data dir
INPUT_DIR  <- file.path(BASE, "int-out", sub_dir )
OUTPUT_DIR <- file.path(BASE, "docs", "lab_notebooks", "atmCO2_landFlux_filtering", sub_dir)
dir.create(OUTPUT_DIR, showWarnings = F)

# Make an empty list to save the outputs in 
script_output = list()


# 1. Function ----------------------------------------------------------------------------------------------------------
# filter_obs: is a function that filters the filter_flags data frame by a possible combination of observation filters.
# data         - is the data frame of filter flag codes
# filter_flags - the names of the filter flags to check, will only keep runs that have flags equal to 1. 
# name         - the name to call this filtering method
filter_obs <- function(data, filter_flags, name){
  
  keep_cols <- names(data)[!grepl("flag", names(data))]
  
  data %>%
    select(c(keep_cols, filter_flags)) %>%
    gather(filter, flag, filter_flags) ->
    long_data

  grouping_names <- colnames(long_data)[!names(long_data) %in% "flag"]

  long_data %>%
    mutate(filter = name) %>%
    group_by_at(grouping_names) %>%
    summarise(flag_sum = sum(flag)) %>%
    mutate(flag = if_else(flag_sum == length(filter_flags), 1, 0)) %>%
    mutate(flag = as.integer(flag)) %>%
    ungroup %>%
    select(-flag_sum)
  
    }



# 2. Run Obs Filtering ----------------------------------------------------------------------------------------------------------------
# Run the filtering process for various window length years at a consistent threshold, this step takes a long time so if 
# possible comment it out. 
# various_windowYrs <- list(10, 15, 20, 30)
# 
# map(various_windowYrs, function(yr){
#   
#   windowYrs <<- yr
#   fall_in_threshold <<- 0.4
#   
#   # Make the observational windows 
#   source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.1.temperature_observation_processing.R"))
#   source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.1.cdiac_observation_processing.R"))
#   source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.1.cdiac_growth_processing.R"))
#   source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.1.co2_noaa_processing.R"))
#   
#   # Calculate the Hector moving averages 
#   source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.2.temperature_hector_processing.R"))
#   source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.2.flux_hector_processing.R"))
#   source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.2.growth_hector_processing.R"))
#   source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.2.co2_hector_processing.R"))
#   
#   
#   # Filter the Hector results by the observational windows.
#   source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.3.temperature_filtering.R"))
#   source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.3.flux_filtering.R"))
#   source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.3.growth_filtering.R"))
#   source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.3.co2_filtering.R" ))
#   
#   # Copy the results of the filtering process to the percent threshold investigation 
#   # dir.
#   new_name <- file.path(OUTPUT_DIR, paste0("filter_flag_", fall_in_threshold, "_", windowYrs, ".csv"))
#   file.copy(from = file.path(INPUT_DIR, "filter_flag.csv"), to = new_name, overwrite = T)
#   
# })




# 3. Run Count --------------------------------------------------------------------------------------------------------

# Import the filter flag files and add the threshold and moving average information.
list.files(OUTPUT_DIR, "filter_flag", full.names = T) %>% 
  map(function(file){
    
    # Import the data 
    data <- readr::read_csv(file)
    
    # Get information from the file name 
    info <- gsub("filter_flag_", "", basename(file))
    info <- gsub(".csv", "", info)
    
    # Add threshold and maYrs (moving average years) to the data frame
    data %>% 
      mutate(info = info) %>% 
      separate(info, c("threshold", "maYrs"), sep = "_", remove = T)
    
  }) %>%  
  bind_rows -> 
  data

# Apply the filtering process and then concatenate into a flat data frame.
bind_rows( data %>% 
             group_by(threshold, maYrs) %>% 
             filter_obs(filter_flags = "atmCO2_flag", name = "atm CO2"),
           
           data %>% 
             group_by(threshold, maYrs) %>% 
             filter_obs(filter_flags = c("atmCO2_flag", "landflux_flag") , name = "atm CO2 & land flux"),
           
           data %>% 
             group_by(threshold, maYrs) %>% 
             filter_obs(filter_flags = c("atmCO2_flag", "tempature_flag") , name = "atm CO2 & temp"),
           
           data %>%  
             group_by(threshold, maYrs) %>% 
             filter_obs(filter_flags = c("atmCO2_flag", "growth_flag") , name = "atm CO2 & growth") ) -> 
  filtered_flags


# Count the number of runs that pass through each filter combination.
filtered_flags %>% 
  group_by(threshold, maYrs, filter) %>% 
  summarise(run_count = sum(flag)) %>% 
  ungroup -> 
  run_count


# 4. Plot --------------------------------------------------------------------------------------------------------

# Plot the number of passing runs for each threshold / filter combination.
ggplot(run_count) + 
  geom_col(aes(filter, run_count, group = maYrs, fill = maYrs),  position = "dodge") + 
  geom_text(aes(x = filter, y = run_count,  group = maYrs, label = run_count), position = position_dodge(0.9), vjust=-.2) + 
  labs(x = "filter combination", 
       y = "run count", 
       title = "Atm CO2 + obs filter run count \n Exploring different moving average year lengths \n 40% threshold") -> 
  script_output$run_count


# 5. Save --------------------------------------------------------------------------------------------------------

# Save all of the script outputs
save(script_output, file = file.path(OUTPUT_DIR, "atmCO2_landFlux_filtering_figs.rda"))

# End 
  
