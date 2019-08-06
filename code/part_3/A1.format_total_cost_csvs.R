# Purpose: Format the GCAM total policy cost results to global values in the desired units. 

# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Directories
BASE    <- getwd()    # Should be the hector-SA-npar R project location. 
sub_dir <- 'CMSflux' # The name of the out-3/sub directory to pull data from

# 1. Import Data ---------------------------------------------------------------------------------------------

# Import the total policy cost csv files. 
list.files(file.path(BASE, 'output', 'out-3', sub_dir), 'gcam_rslts', full.names = TRUE) %>% 
  lapply(read_xlsx) %>% 
  bind_rows %>% 
  distinct -> 
  total_costs


# 2. Format Data ---------------------------------------------------------------------------------------------

# Format the total cost xlsx files of regional total policy costs to global total policy cost and convert the units. 
total_costs %>% 
  group_by(scenario, units, variable) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(run_name = substr(start = 1, stop = 13, x = scenario), 
         policy = substr(start = 15, stop = 20, x = scenario)) %>% 
  select(run_name, policy, variable, value, units) %>% 
  # Convert the units (GCAM returns values in millions of 1990 dollars.)
  mutate(value = 1e6 * value * 1.64753738, 
         units = 'USD$2015') -> 
  policy_costs


# 3. Save output -------------------------------------------------------------------------------

saveRDS(policy_costs, file = file.path(BASE, 'output', 'out-3', sub_dir, 'total_policy_cost.rds'))


