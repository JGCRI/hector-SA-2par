
# Purpose: This script formats the contents of out-3/queries csv into a format that 
# is ready for plotting and comparison. Notes, this is a part 3 script, it depends on 
# a number of non-automated processes being completed first! Check out the read me in the 
# code/part_3 section for more details.

# 0. Set Up ---------------------------------------------------------------------------------------------
# Make sure that the working directory is equal to the project directory
if( ! 'hector-SA-npar.Rproj' %in% list.files() ){ stop( 'Working dir must be the project location' ) }

# Load libs
library(dplyr)
library(tidyr)
library(purrr)
library(rgcam)
library(stringr)

# Directories
BASE    <- getwd()         # Should be the location of the project
sub_dir <- 'vary_4_params' # The name of the out-L/sub directory to pull data from

# 1. Import Data ---------------------------------------------------------------------------------------------

# Import the query csv files from out-3
files <- as.list(list.files(file.path(BASE, 'output', 'out-3', sub_dir, 'queries csv'), full.names = TRUE))
names(files) <- files

files %>% 
  purrr::map(function(path = .x){
    
    data <- read.csv(file = path, stringsAsFactors = FALSE, header = TRUE)
    data$query_name <- gsub('.csv', '', basename(path))
    data
      
  }) -> 
  input

# Rename the elements of the list
names(input) <- gsub('.csv', '', basename(names(input)))


# Import the mapping tibble
file.path(BASE, 'out-2', sub_dir, 'A.Hector_GCAM_parameters_mapping.csv') %>%  
  read.csv(stringsAsFactors = FALSE) -> 
  mapping_tibble


# 2. Format Data ---------------------------------------------------------------------------------------------

# This section of code changes the query data from wide to long format, the cms run name is 
# parsed out of the GCAM scenario name, the mapping tibble is used to add information, and 
# and some key columns are renamed to be consistent with other GCAM outputs. 
input %>% 
  map(function(x){
    
    # Swtich form wide to long format
    columns <- names(x)[grepl('X', names(x))]
    
    x %>%  
      gather(year, value, columns) %>%  
      mutate(year = as.integer(gsub('X', '', year))) %>% 
      
    # Parse out the cms run name from the GCAM sceanrio name 
      separate(scenario, c('run_name', 'scenario info'), sep = '_') %>% 
      select(-`scenario info`) %>%  
    
    # Change column names to be consistent with the other GCAM output data
      rename(units = Units)  %>% 
      
    # Add the run mapping information
      inner_join(mapping_tibble, by = 'run_name')
    
  }) -> 
  query_results

# GCAM reports results in 1990 dollars but we are interested in costs in 2015$, 
# uses a the converstion factor of 1.64753738 from Jae to correct the units. 

cost_queryies <- c('Discounted policy cost', 'Policy Cost By Period', 'Prices for all markets', 
                   'Regional primary energy costs', 'Undiscounted policy cost')


# GCAM results were 1990$ but we would like to report values in terms of 2015$. 
query_results %>% 
  modify_at(.at = cost_queryies, function(input){
    
      input %>% 
        mutate(value = value * 1.64753738, 
               units = 'USD$2015') 

  }) -> 
  query_results_cost


# 3. Save output -------------------------------------------------------------------------------

query_results_cost %>% 
  map(function(x){
    
    name <- paste0(unique(x$query_name), '.rda')
    save(x, file = file.path(BASE, 'output', 'out-3', sub_dir, name))
    
  })
  


