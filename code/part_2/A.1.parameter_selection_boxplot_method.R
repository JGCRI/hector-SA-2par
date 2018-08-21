# Purpose: This code selects the parameter sets that correspond to the median, IQR, and whisker range of  
# a box plot for the 2100 temp. 

# 0. Set up ----------------------------------------------------------------------------------------------

# Load required libs
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)

# Set up the dirs
BASE    <- getwd()                                      # Must be the project dir
sub_dir <- "rcp26"                                      # Define the out-1 subdirectory to search 
Dn_file <- "E.all_Dmetric_independent_results.csv"      # Define the Dn metric file to process 

# Variables 
variable_list <- c("atm CO2", "NPP", "Tgav")

# Load the selection functions 
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))

# Script output 
script_output = list()

# 1.A. Select Extreme 2100 Values --------------------------------------------------------------------------------

# Import the Dn results
readr::read_csv(list.files(file.path(BASE,'out-1', sub_dir), Dn_file, full.names = T)) %>%  
  filter(variable %in% variable_list) %>% 
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  select(run_name, variable, passing) %>% 
  spread(variable, passing) -> 
  wide_passing_Dn

# Import a Hector data set to use in the selection process. We are interested in the 
# extreeme max/min temperatures in year 2100 for each possible combination. 
read.csv(list.files(file.path(BASE,'out-1', sub_dir), "C.Tgav_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE) %>% 
  filter(year == 2100) %>% 
  select(run_name, variable, year, value) -> 
  Hector_2100_values

# Combine the wide Dn metric data and the Hector 2100 values into a single data frame.
Dn_Hector_2100          <- full_join(wide_passing_Dn, Hector_2100_values, by = "run_name") 
categorized_2100_values <- categorize_runs(Dn_Hector_2100, variable_list)

# Now that the passing Dn metric and 2100 Hector values are combined into a single data 
# frame select the extreeme values in each observation category.
boxplot_values <- select_boxplot_values(Dn_Hector_2100, variable_list) 
  
# Count the runs.
run_count_df <- run_count(Dn_Hector_2100, variable_list) 


# 1.B. Diagnotic Plots --------------------------------------------------------------------------

# Plot the 2100 values and the extreme values that are going to be used in Hector-GCAM. This figure 
# is a good way to make sure that the code is doing what it should be. 

categorized_2100_values %>% 
  full_join(run_count_df, by = 'filter_name') %>% 
  ggplot() + 
  geom_jitter( aes(filter_name, value, color = filter_name) ) + 
  geom_boxplot(  aes(filter_name, value), outlier.colour = NA) +
  geom_point( data = boxplot_values, aes(filter_name, value)) + 
  labs(title = 'Hector-GCAM run selection') -> 
  diag_plot


# 2. Create the Selected Parameter Set -------------------------------------------------------------

# Import the parameter combinations and add the run_name column.
parameter_set          <- read.csv(file.path(BASE, 'out-1', sub_dir, 'A.par4_combinations.csv'), stringsAsFactors = FALSE)
parameter_set$run_name <- paste0( 'hectorSA-', sprintf( '%04d', parameter_set$run_index ))
                          
# Subset the parameter data frame so that it only includes the parameter sets selected. 
# Make sure that this data frame does not contain any duplicate entries, the selection 
# method can include duplicates but we do not want to spend computing time running 
# duplicate Hector-GCAM runs.
parameter_set %>% 
  filter(run_name %in% unique(boxplot_values$run_name)) %>% 
  select(-run_index) -> 
  Hector_GCAM_parameters

# Make a mapping file of the selected paramter runs to keep track of why a partifular run was selected. 
boxplot_values %>% 
  mutate(variable = 'Tgav') %>% 
  select(run_name, variable, filter_name, boxplot) %>% 
  distinct ->
  Hector_GCAM_parameter_mapping


# Save the output in the secondary output file
output_dir <- file.path(BASE, 'out-2', sub_dir)
dir.create(output_dir, showWarnings = FALSE)

file_name <- file.path(output_dir, 'A.Hector_GCAM_parameters_boxplot_selection.csv' )
write.csv( Hector_GCAM_parameters, file = file_name, row.names = FALSE )

file_name <- file.path(output_dir, 'A.Hector_GCAM_parameters_mapping_boxplot_selection.csv')
write.csv( Hector_GCAM_parameter_mapping, file = file_name, row.names = FALSE )
save(script_output, file = file.path(BASE, 'out-fig', 'Tgav_2100_boxplot_selection.rda'))

# End

