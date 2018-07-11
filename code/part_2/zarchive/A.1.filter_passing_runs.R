# Purpose: This is the code that selects the Hector parameter sets to pass along into to be 
# used in GCAM-hector. Selects the max, min, and middle 2100 temp for the Hector runs 
# that passed the filtering tests. Note, this is not functionalized. 


# 0. Set up ----------------------------------------------------------------
# The working directory should be the project directory. 
BASE <- getwd()
if(basename(BASE) != 'hector-SA-npar'){stop('working directory should be the project directory')}

# Library
library(dplyr)

# Select the out-1/ sub directory name
sub_dir <- "rcp26"


# Import Data  

# Import the filter flag csv which contains the results of the window filtering, use this data to 
# determine which hector parameter combinations should be used to create the hector gcam ini files in 
# the next level. Also import the raw hector results, right now we are only interested in the passing 
# along the parameter combinations that predict the min, max, and, mid 2100 temp.

filter_flag <- readr::read_csv(file.path("./out-1", sub_dir , "filter_flag.csv"))
hector_results_wide <- readr::read_csv(file.path( "./out-1", sub_dir,"C.hector_run_cleanup.csv"))


# Define Functions 

# select_param_set: is a function that filters the hector results data frame for or or more 
# flag names equal to one, in overwords the hector runs that pass the window filtering. Then 
# this function subsets the passing runs for the min, mid, and max values for a single variable. 
select_param_set <- function(data, flag_names, vari, param_names){
  
  # data - the data frame of the hector, flag, parameter results
  # flag_names - a vector or one ore more strings of the flag column names to use in the filtering process
  # vari - a string of the variable name to subset the passing hector runs by 
  # param_names - a vector of the parameter column to include in the returned tibble
  
  # Subset the data for the passing hector runs and variable of interest at year 2100
  data %>%  
    filter_at(flag_names, all_vars(. == 1)) %>% 
    filter(variable == vari) %>% 
    select(variable, flag_names, param_names, `2100`) ->
    to_select_from
  
  # Check filtering worked
  check <- distinct(to_select_from[ ,names(to_select_from) %in% c(flag_names, "variable")])
  if(nrow(check) > 1){stop("Problem with filtering method")}
  
  # Parse out parameter set for min temp. 
  min_tgav <- to_select_from[which.min(to_select_from$`2100`),]
  min_tgav[["keep"]] <- paste0("min ", vari)
  
  # Parse out parameter set for max temp.
  max_tgav <- to_select_from[which.max(to_select_from$`2100`),]
  max_tgav[["keep"]] <- paste0("max ", vari)
  
  # Parse out the parameter set for min temp. 
  median_tgav <- median(to_select_from$`2100`)
  mid_tgav <- to_select_from[which.min(abs(to_select_from$`2100` - median_tgav)),]
  mid_tgav[["keep"]] <- paste0("mid ", vari)
  
  bind_rows(min_tgav, max_tgav, mid_tgav) %>% 
    select(param_names, keep) %>% 
    mutate(filter_flag = paste(flag_names, collapse = " & ")) 
  
}


# 1. Format Hector Results  ----------------------------------------------------------------

# Determine which hector runs to keep based on the filter flags.
filter_names <- names(filter_flag)[grepl("flag", names(filter_flag))]
param_names  <- names(filter_flag)[!grepl("flag", names(filter_flag))]
hector_runs_to_keep <- filter_at(filter_flag, filter_names, any_vars( . == 1 ))

# Subset the hector results so that the data frame contains parameters from 
# the Hector runs that passed the window filtering for future_baseYr
hector_results_wide %>% 
  filter( run_name %in% hector_runs_to_keep$run_name ) %>% 
  left_join( filter_flag, by = "run_name" ) %>%  
  select(filter_names, param_names, variable, units, `2100` ) -> 
  hector_2100_flag


# 2. Select the parameters sets ------------------------------------------------------------

# The set of parameters that passes through the different filter combinations.
set1 <- select_param_set(hector_2100_flag, "tempature_flag", "Tgav", param_names)
set2 <- select_param_set(hector_2100_flag, c("tempature_flag", "landflux_flag"), "Tgav", param_names) 
set3 <- select_param_set(hector_2100_flag, c("tempature_flag", "growth_flag", "landflux_flag"), "Tgav", param_names) 


# Combine into the parameter set to use in level H to create the Hector gcam ini files 
# used in gcam. 
param_set <- bind_rows(set1, set2, set3)

# Subset Hector results so that it only includes the results from the runs that are from the paramter 
# sets data frame. 
hector_results_wide %>% 
  left_join(param_set, by = "run_name") %>% 
  filter(!is.na(keep)) %>% 
  tidyr::gather(year, value, matches("^(1|2)[0-9]{3}$")) -> 
  filtered_hector_results

# Save 
write.csv(param_set, file = file.path(".", "out-1", sub_dir, "2A.selected_parameter_sets.csv"), row.names = F)
write.csv(filtered_hector_results, file = file.path(".", "out-1", sub_dir, "2A.selected_hector_results.csv"), row.names = F)

