# Purpose: This code selects the parameter sets based on Hector run results to be used 
# in GCAM. There are lots of different methods that could be used to select the parameter combinations 
# this method pulls out the temperature extreemes from different layers / combinations of matching 
# observational combinations. 


# 0. Set up ----------------------------------------------------------------------------------------------

# Load required libs
library(dplyr)
library(tidyr)
library(ggplot2)

# Set up the dirs
BASE    <- getwd()                                      # Must be the project dir
sub_dir <- "rcp26"                                      # Define the int-out subdirectory to search 
Dn_file <- "E.all_Dmetric_independent_results.csv"      # Define the Dn metric file to process 

# Variables 
variable_list <- c("atm CO2", "Land Flux", "NPP", "Tgav")

# Load the selection functions 
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))

# Script output 
script_output = list()

# 1.A. Select Extreme 2100 Values --------------------------------------------------------------------------------

# Import the Dn results
readr::read_csv(list.files(file.path(BASE,'int-out', sub_dir), Dn_file, full.names = T)) %>%  
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  select(run_name, variable, passing) %>% 
  spread(variable, passing) -> 
  wide_passing_Dn

# Import a Hector data set to use in the selection process. We are interested in the 
# extreeme max/min temperatures in year 2100 for each possible combination. 
read.csv(list.files(file.path(BASE,'int-out', sub_dir), "C.Tgav_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE) %>% 
  filter(year == 2100) %>% 
  select(run_name, variable, year, value) -> 
  Hector_2100_values

# Combine the wide Dn metric data and the Hector 2100 values into a single data frame.
Dn_Hector_2100 <- full_join(wide_passing_Dn, Hector_2100_values, by = "run_name") 

# Now that the passing Dn metric and 2100 Hector values are combined into a single data 
# frame select the extreeme values in each observation category.
extreeme_values <- select_extreeme_values(data = Dn_Hector_2100,  obs_list = c("atm CO2", "Land Flux", "NPP", "Tgav")) 
  
# Count the runs.
run_count_df <- run_count(data = Dn_Hector_2100,  obs_list = c("atm CO2", "Land Flux", "NPP", "Tgav")) 


# 1.B. Plot Extreeme 2100 Tgav --------------------------------------------------------------------------

# Plot the range of the 2100 values. 
# In order to make the plot a little more coherent oroder the filters by the magnitude of the difference. 
extreeme_values %>% 
  group_by(filter_name) %>% 
  # Calculate the magnitude of the range.
  summarise(value = abs(diff(value))) %>% 
  ungroup  %>% 
  # Arrange the data frame by the range magnitude.
  arrange(desc(value)) -> 
  filter_factor_order

# Add the max value to the run count data frame to use as the location for where to plot the 
# run count. 
extreeme_values %>% 
  filter(extreeme == 'max') %>%  
  select(filter_name, value) %>%  
  left_join(run_count_df, by = 'filter_name') -> 
  run_count_df

# Use the filter names from the filter_factor_order data frame as factor levels.
extreeme_values$filter_name <- factor(x = extreeme_values$filter_name, levels = filter_factor_order$filter_name, order = TRUE)
run_count_df$filter_name    <- factor(x = run_count_df$filter_name, levels = filter_factor_order$filter_name, order = TRUE)

# Plot the range
extreeme_values %>% 
  ggplot(aes(filter_name, value, color = filter_name, fill = filter_name)) + 
  geom_line(size = 10) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=14)) + 
  geom_text(data = run_count_df, aes(x = filter_name, y = value, label = count), size = 5, vjust=-.2) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav Range', 
       caption = 'With the number of runs that match the obs combinations') -> 
  script_output$'2100_Tgav_range'



# 2. Create the Selected Parameter Set -------------------------------------------------------------

# Import the parameter combinations and add the run_name column.
parameter_set          <- read.csv(file.path(BASE, 'int-out', 'A.par4_combinations.csv'), stringsAsFactors = FALSE)
parameter_set$run_name <- paste0( 'hectorSA-', sprintf( '%04d', parameter_set$run_index ))
                          
# Subset the parameter data frame so that it only includes the parameter sets selected. 
# Make sure that this data frame does not contain any duplicate entries, the selection 
# method can include duplicates but we do not want to spend computing time running 
# duplicate Hector-GCAM runs.
parameter_set %>% 
  filter(run_name %in% unique(extreeme_values$run_name)) %>% 
  select(-run_index) -> 
  Hector_GCAM_parameters

# Save the output in the secondary output file
file_name <- file.path( BASE, 'sub-out', 'A.Hector_GCAM_parameters.csv' )
write.csv( Hector_GCAM_parameters, file = file_name, row.names = FALSE )

# End

