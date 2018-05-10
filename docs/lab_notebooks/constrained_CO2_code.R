# Purpose: this code compares the results of the unconstrained vs constrained carbon cycle and the number of passing runs. 

# 0. Set Up -------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(map)

# Define the directories
BASE <- getwd() # working directory should be set equal to the project location
rcp26_DIR      <- file.path(BASE, "int-out", "rcp26")
lawdomeCO2_DIR <- file.path(BASE, "int-out", "rcp26_lawdomeCO2")

OUTPUT_DIR <- file.path(BASE, "docs", "lab_notebooks", "constrained_CO2")
dir.create(OUTPUT_DIR, showWarnings = F)

script_output <- list()

# 1. Function -------------------------------------------------------------------------------------------------------------

# filter_obs: is a function that filters the filter_flags data frame by a possible combination of observation filters.
     # data         - is the data frame of filter flag codes
     # filter_flags - the names of the filter flags to check, will only keep runs that have flags equal to 1. 
     # name         - the name to call this filtering method
filter_obs <- function(data, filter_flags, name){
  
  data %>% 
    filter_at(filter_flags, all_vars(. == 1)) %>% 
    mutate(filter = name, 
           flag = 1) %>% 
    select(run_index, beta, q10, s, diff, run_name, filter, constrained, flag) 
}



# 1. Import Data ----------------------------------------------------------------------------------------------------------

# Load the rcp 26 filter flag file and state that the file is constrained.
list.files(rcp26_DIR, "filter_flag.csv", full.names = T) %>% 
  readr::read_csv() %>% 
  mutate(constrained = as.integer(0)) -> 
  rcp26

# Load the constrained filter flag file and is unconstrained.
list.files(lawdomeCO2_DIR, "filter_flag.csv", full.names = T) %>% 
  readr::read_csv() %>% 
  mutate(constrained = as.integer(1)) -> 
  lawdomeCO2

# Combine the unconstrained and constrained  data into a single data frame.
data <- bind_rows(rcp26, lawdomeCO2)




# 2. Count Passing Runs -----------------------------------------------------------------------------------
# Use the filter_obs function defined in section 1 subset the filter_flags data frame to include 
# runs that pass specific observation filter combinations.

# Only use the CO2 filter observation filter.
data %>%  
  group_by(constrained) %>% 
  filter_obs(filter_flags = "atmCO2_flag", name = "atm CO2") %>% 
  bind_rows -> 
  CO2_only

# Figure out what runs pass through the CO2 atm and the land flux filter.
data %>%  
  group_by(constrained) %>% 
  filter_obs(filter_flags = c("atmCO2_flag", "landflux_flag") , name = "atm CO2 & land flux") %>% 
  bind_rows -> 
  CO2_landFlux

# Find the runs that pass through the CO2 and the temperature observation windows.
data %>%  
  group_by(constrained) %>%  
  filter_obs(filter_flags = c("atmCO2_flag", "tempature_flag") , name = "atm CO2 & temp") %>% 
  bind_rows -> 
  CO2_temp

# Find the runs that pass through the CO2 and the growth observation windows.
data %>%  
  group_by(constrained) %>%  
  filter_obs(filter_flags = c("atmCO2_flag", "growth_flag") , name = "atm CO2 & growth") %>% 
  bind_rows -> 
  CO2_growth

# Concatenate the filtered runs into a single data frame.
filtered_runs <- bind_rows(CO2_only, CO2_landFlux, CO2_temp, CO2_growth)

# Count the number of passing runs for the constrained CO2 levels for each observation filter 
# combinations applied. 
filtered_runs %>% 
  group_by(filter, constrained) %>%  
  summarise(run_count = as.integer(sum(flag))) %>% 
  mutate(constrained = if_else(constrained == 0, "No", "Yes")) -> 
  combo_run_count


# 3. Plot ---------------------------------------------------------------------------------------------------------------

# Create a column plot of the number of passing runs.
ggplot(combo_run_count) +
  geom_col(aes(x = filter, y = run_count, group = constrained, fill = constrained), position = "dodge") + 
  geom_text(aes(x = filter, y = run_count,  group = constrained, label = run_count), position = position_dodge(0.9), vjust=-.2) + 
  # Add labels 
  labs( x = "filter used", 
        y = "number of passing runs", 
        title = "70 % fall in criteria") -> 
  script_output[["run_count"]]


# 4. Save ---------------------------------------------------------------------------------------------------------------

save(script_output, file = file.path(OUTPUT_DIR, "constrained_figs.rda"))
