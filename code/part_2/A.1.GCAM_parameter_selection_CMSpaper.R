# Purpose: This code selects the parameter sets based on Hector run results to be used 
# in GCAM policy runs for the CMS paper. 


# 0. Set up ----------------------------------------------------------------------------------------------
# Set up the dirs
BASE                   <- getwd()                       # Must be the project dir
runs_to_filter_DIR     <- "vary_beta_q10"               # Use the Dn results from the runs from the his output/out-1 directory classify the Hector runs
Dn_file                <- 'E.Dn_metric_results.csv'     # Define the Dn metric file to process 
runs_to_select_DIR     <- 'rcp26'                       # Use the stand alone hector 2100 temperature results to select the filtered region extremes.


# Load libs and support functions. 
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)

# Filter list, the names of the observational filters to save. 
filter_list <-  c("Tgav, NPP, atmCO2 multi optimized", 'Tgav')

# The number runs to select from the extreme ends of the filtered region to run through GCAM. 
select_n_runs <- 25


# 1. Import Data  --------------------------------------------------------------------------------
# Import the Dn results and make sure it only contains the results for the runs that pass through our filters of 
# interest (Dn <= Dc).
readr::read_csv(list.files(file.path(BASE,'output', 'out-1', runs_to_filter_DIR), Dn_file, full.names = T)) %>%  
  filter(filter_name %in% filter_list) %>% 
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  filter(passing) ->
  passing_Dn


# Import the stand alone Hector temperature data set that will be used in the selection process. 
# We are interested in the parameter combinations that resulted in the extreeme 2100 temperatures. 
read.csv(list.files(file.path(BASE, 'output', 'out-1', runs_to_select_DIR), "C.Tgav_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE) %>% 
  filter(year == 2100) %>% 
  select(run_name, variable, year, value) -> 
  Hector_2100_values

# Import the paramter values. 
param_mapping <- read.csv(list.files(file.path(BASE,'output', 'out-1', runs_to_filter_DIR), 'A.par4_combinations.csv', full.names = TRUE))

# 2. Select the extreeme runs -----------------------------------------------------------------
# Count the number of runs per filter_name, the total passing run count will be used to determine 
# the cut off for the extreme runs to keep. 
passing_Dn %>% 
  group_by(filter_name) %>%  
  summarise(count = n_distinct(run_name)) ->
  total_run_count

# Select the N most extreme runs from each filtered category. 
#
# Start by adding the filter category to the Hector 2100 temperature values. 
Hector_2100_values %>%  
  inner_join(passing_Dn, by = 'run_name') %>%  
  distinct %>% 
  # Clean up the list so that it only contains actual data 
  split(interaction(.$filter_name)) %>% 
  rlist::list.clean(fun = function(input){nrow(input) <= 1}) %>% 
  lapply(function(input){
    
    # Pick the min 
    input %>% 
      arrange(value) %>%  
      mutate(rank = 1:nrow(.)) %>% 
      left_join(total_run_count, by = 'filter_name') %>% 
      mutate(cut_off = count - select_n_runs + 1) %>% 
      filter(rank <= select_n_runs)  %>% 
      mutate(keep = 'min') -> 
      keep 
    
    input %>% 
      arrange(desc(value)) %>%  
      mutate(rank = 1:nrow(.)) %>% 
      left_join(total_run_count, by = 'filter_name') %>% 
      mutate(cut_off = count - select_n_runs + 1) %>% 
      filter(rank <= select_n_runs) %>%  
      mutate(keep = 'max') %>% 
      bind_rows(keep) 
    
    
  }) %>%
  bind_rows() -> 
  extreme_runs 

# Combine the data frame of the extreme runs and the parameter combinations. 
extreme_runs %>% 
  inner_join(param_mapping, by = 'run_name') %>% 
  select(run_name, beta, q10, s, diff, keep, filter_name) %>%
  distinct %>% 
  na.omit ->
 GCAM_param_values


# 3. Write GCAM parameter files ------------------------------------------------------------
# Becasue of the set up of the GCAM runs the max number of policy runs that run in 
# one batch is 60. We are going to set up two output/out-2 directories that will 
# be used as inputs for the GCAM policy runs. 

OUTPUT_DIR <- file.path(BASE, 'output', 'out-2')

out1 <- file.path(OUTPUT_DIR, 'CMSpaper_carbon1')
dir.create(out1, showWarnings = FALSE)
write.csv(x = GCAM_param_values[1:50, ], file = file.path(out1, 'A.Hector_GCAM_parameters.csv'), row.names = F)

out2 <- file.path(OUTPUT_DIR, 'CMSpaper_carbon2')
dir.create(out2, showWarnings = FALSE)
write.csv(x = GCAM_param_values[51:89, ], file = file.path(out2, 'A.Hector_GCAM_parameters.csv'), row.names = F)
