# Purpose: This script looks at the number of runs that pass through the different observation windows when 
# the percent threshold, the percent of years that must fall within the obs window for a year change. 


# 0. Set Up --------------------------------------------------------------------------------------------------------------

# Check to make sure that the working directory and the base dir path is the location of the project 
# directory. 
BASE <- getwd() 
if(basename(BASE) != "hector-SA-npar"){stop("Working directory should be the project location.")}


# Load libs 
library(dplyr);   library(tidyr)
library(ggplot2); library(purrr)


# Pick which data to process, the sub dir name in the int-out dir to process. 
sub_dir <- "rcp26_lawdomeCO2"

# Define the input and output data dir
INPUT_DIR  <- file.path(BASE, "int-out", sub_dir )
OUTPUT_DIR <- file.path(BASE, "docs", "lab_notebooks", "percent_threshold", sub_dir)
dir.create(OUTPUT_DIR, showWarnings = F)

# Create a place to store all of the results
script_output <- list()


# 1. Run Observation Filtering --------------------------------------------------------------------------------------------------------------

# From a list of defined thresholds do the observationvation filtering to create filter_flag files for the various
# threshold criteria. Output from this section will be analyzed and plotted in sections 2 and 3. This section 
# of code will only work if the observational windows and the hector moving averages have been created. 

# Define the sub dir to search 
# TODO change the rcpXX in part konw code to sub_dir
rcpXX <<- sub_dir

# Length of years to use in the moving average
windowYrs <- 15 

# Make the observational windows -- this part may be comment out, jsut needs to be run once.
#source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.1.temperature_observation_processing.R"))
#source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.1.cdiac_observation_processing.R"))
#source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.1.cdiac_growth_processing.R"))
#source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.1.co2_noaa_processing.R"))

# Calculate the Hector moving averages 
# This can take a while but must be done any time the sub directory is changed. 
source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.2.temperature_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.2.flux_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.2.growth_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.2.co2_hector_processing.R"))


# Now create a list of various trhesholds to use in the filtering setp. 
various_thresholds <- list(0.5, 0.6, 0.7, 0.8, 0.9, 1)

# Filter by various threshold criteria.
map(various_thresholds, function(x){
    
    # Define the sub dir to search and the fall in threshold to use. 
    fall_in_threshold <<- x
    
    # Filter the Hector results by the observational windows.
    source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.3.temperature_filtering.R"))
    source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.3.flux_filtering.R"))
    source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.3.growth_filtering.R"))
    source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.3.co2_filtering.R" ))
    
    # Copy the results of the filtering process to the percent threshold investigation 
    # dir.
    new_name <- file.path(OUTPUT_DIR, paste0("filter_flag_", fall_in_threshold, ".csv"))
    file.copy(from = file.path(INPUT_DIR, "filter_flag.csv"), to = new_name, overwrite = T)
    

  })

# 2. Run Count Plots --------------------------------------------------------------------------------------------------------------

# Import the filter flag files created in section 1 and concatenate into a single data frame. 

list.files(OUTPUT_DIR, "filter_flag", full.names = T) %>% 
  map(function(x){
    
    # Read in data 
    data <- readr::read_csv(x)
    
    # Get the threshold used from the file name
    threshold <- gsub(".csv", "", basename(x))
    threshold <- gsub("filter_flag_", "", threshold)
    
    # Create a threshold colum 
    data$threshold <- threshold
    
    # Return the data 
    data
    
  }) %>% 
  bind_rows -> 
  threshold_filter_flags


# Count the number of passing runs in filter, right now we are only looking at a single filter. 
threshold_filter_flags %>%  
  select(tempature_flag, landflux_flag, growth_flag, atmCO2_flag, threshold) %>%  
  # Format into long data frame in preperation for plotting
  gather(filter, flag, -threshold) %>% 
  group_by(threshold, filter) %>%  
  # Since the filter flags are binary values the where 1 is a run that passes through the obs
  # window the count of the passing runs = the sum of the 1s in the  vlaue. 
  summarise(run_count = sum(flag)) -> 
  sinlge_filter_run_count_df


# Create a column plot of the number of passing runs, include text for the number of runs. 
ggplot(sinlge_filter_run_count_df) +
  geom_col(aes(x = filter, y = run_count, group = threshold, fill = threshold), position = "dodge") + 
  geom_text(aes(x = filter, y = run_count,  group = threshold, label = run_count), position = position_dodge(0.9), vjust=-.2) + 
  # Add labels 
  labs( x = "filter used", 
        y = "number of passing runs", 
        title = paste0(sub_dir)) -> 
  script_output[["run_count_1"]]


# 3. Paramter Space Plot --------------------------------------------------------------------------------------------------------------

# Subset the filter flag data so that it only includes the passing runs and then format into long data type, long for 
# both filter used and paramter value. 

threshold_filter_flags %>%
  # Format filter flag information into long format and keep only the passing runs
  gather(filter, flag, tempature_flag, landflux_flag, growth_flag, atmCO2_flag) %>% 
  filter(flag == 1) %>% 
  # Format the paratmers values in to a colum
  gather(parameter, value, beta, q10, s, diff) -> 
  paramter_df
  
paramter_df %>% 
  ggplot(aes(filter, value, color = threshold), alpha = .5) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.7)) + 
  facet_wrap(~parameter, scales = "free") + 
  # Add lables 
  labs(x = "filter",
       y = "parameter value", 
       title = sub_dir) -> 
  script_output[["paramter_space"]]
  

# 4. Save -----------------------------------------------------------------------------------------------------
# Save the list of the figures 
save(script_output, file = file.path(OUTPUT_DIR, "percent_threshold_figs.rda"))




