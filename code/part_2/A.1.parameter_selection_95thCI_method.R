# Purpose: This code selects the parameter sets based on Hector run results to be used 
# in GCAM. There are lots of different methods that could be used to select the parameter combinations 
# this method usese the parameter set that is closest to the upper and lower 95th CI for 2100 temp in 
# each of the categories. 


# 0. Set up ----------------------------------------------------------------------------------------------

# Load required libs
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(purrr)

# Set up the dirs
BASE    <- getwd()                                      # Must be the project dir
sub_dir <- "rcp26"                                      # Define the out-1 subdirectory to search 
Dn_file <- "E.all_Dmetric_independent_results.csv"      # Define the Dn metric file to process 

# Variables 
variable_list <- c("atm CO2", "Land Flux", "NPP", "Tgav")

# Load the selection functions 
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))

# Script output 
script_output = list()

# 1. Import and format data --------------------------------------------------------------------------------

# Import the Dn results
readr::read_csv(list.files(file.path(BASE,'out-1', sub_dir), Dn_file, full.names = T)) %>%  
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  select(run_name, variable, passing) %>% 
  spread(variable, passing) %>%  
  # Select the run_name and the observational prodcuts we are interested in processing. 
  select(run_name, `atm CO2`, `NPP`, `Tgav`) ->
  wide_passing_Dn

# Import a Hector data set to use in the selection process. We are interested in the 
# extreeme max/min temperatures in year 2100 for each possible combination. 
Hector_temp <- read.csv(list.files(file.path(BASE,'out-1', sub_dir), "C.Tgav_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE) 

Hector_temp %>% 
  filter(year == 2100, 
         variable %in% names(wide_passing_Dn)) %>% 
  select(run_name, variable, year, value) ->
  Hector_2100_values

# Combine the wide Dn metric data and the Hector 2100 values into a single data frame.
Dn_Hector_2100 <- full_join(wide_passing_Dn, Hector_2100_values, by = "run_name") 

# Count the runs.
run_count_df <- run_count(data = Dn_Hector_2100,  obs_list = c("atm CO2", "NPP", "Tgav")) 


# 2. Select the 95 the -------------------------------------------------------------------------------

# Use the select_95th_values function to select runs that are closest to the upper and lower 95th 
# CI for the 2100 temperature. 

categorized_2100_values <- categorize_runs(Dn_Hector_2100, obs_list = c("atm CO2", "NPP", "Tgav")) 

categorized_2100_values %>% 
  split(.$filter_name) %>% 
  map_dfr(select_95th_values) -> 
  selected_95th_Tgav_2100
  


# 3. Diagnostic Plots --------------------------------------------------------------------------------

# Make sure that the values selected look reasponable. 
ggplot(categorized_2100_values) + 
  geom_jitter(aes(filter_name, value, color = filter_name)) + 
  geom_boxplot(aes(filter_name, value), color = 'black', fill = NA, outlier.shape = NA) + 
  geom_point(data = selected_95th_Tgav_2100, aes(filter_name, value, shape = '95th'), size = 2)

# Histogram
ggplot(categorized_2100_values) + 
  geom_histogram(aes(value), bins = 50) + 
  facet_wrap('filter_name', scales = 'free')

# So I dont think that this method will work beacuse the 2100 temperatures are not distributed normally 
# so does nto work to use a t or z test. 
