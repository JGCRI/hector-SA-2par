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
  

# 1.B. Plot Extreeme 2100 Tgav --------------------------------------------------------------------------

# Plot the range of the 2100 values. 
# In order to make the plot a little more coherent oroder the filters by the magnitude of the difference. 
extreeme_values %>% 
  group_by(filter_name) %>% 
  # Calculate the magnitude of the range.
  summarise(value = abs(diff(value))) %>% 
  ungroup  %>% 
  # Arrange the data frame by the range magnitude.
  arrange(desc(value)) %>%  
  pull(filter_name) -> 
  filter_factor_order

# Use the filter names from the filter_factor_order data frame as factor levels.
extreeme_values$filter_name <- factor(x = extreeme_values$filter_name, levels = filter_factor_order, order = TRUE)

# Plot the range
extreeme_values %>% 
  ggplot(aes(filter_name, value, color = filter_name, fill = filter_name)) + 
  geom_line(size = 7) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank()) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav Range by Filter') -> 
  script_output$'2100_Tgav_range'



# 2. ------------------------------------------------------------------------------------------------