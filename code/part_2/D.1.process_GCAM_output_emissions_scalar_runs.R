
# Purpose: This script parses and processes query data from merged rgcam project and saves 
# results as a flat list as a rda file by by the selection method. From the scaled emissions 
# GCAM output the idea is that outputs from this script should be ready to plot. 

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
BASE    <- getwd() 
sub_dir <- 'scaled_emissions' # This script should only work with this sub directory 


# User Decisions - decide what project to process and what paramter mapping file to use. 
proj_name    <- 'proj_merge_scaled_emissions.proj'


# 1. Import Data ---------------------------------------------------------------------------------------

# Import the merged R project that contains all of the GCAM output data. 
path      <- list.files(file.path(BASE, 'out-2', sub_dir), proj_name, full.names = TRUE)
gcam_proj <- get(load(path))



# 3. Format Query Output  -------------------------------------------------------------------------------------

# The gcam_proj is curently sctrucured as an rgcam project output, a nested list. 
# Format the information in the tibble in preperation for the join with the mapping file. 

query_list        <- listQueries(gcam_proj)
names(query_list) <- query_list

map(query_list, function(query_name){
  
  # Extract the Query
  getQuery(gcam_proj, query_name) %>% 
    # Parse out information from the scenario name
    tidyr::separate(scenario, into = c('run_name', 'emissions_scalar', 'policy'), sep = '_') %>% 
    # Clean up the emissions scalar
    mutate(emissions_scalar = gsub('EF-', '', emissions_scalar)) %>% 
    mutate(emissions_scalar = gsub('p', '.', emissions_scalar)) %>% 
    # Rename the units
    rename(units = Units)  
  
}) -> 
  formatted_gcam_output


# 4. Calculate policy results ----------------------------------------------------------------------------------------

# Remove the reference run from the policy values for each query, only for the queries that contain data 
# for both the reference and target runs. 

map(formatted_gcam_output, function(input){
  
  # Save a copy of the input data tibble names. Latter on this will be used to 
  # select the columns to keep from the wide tibble.
  original_tibble_names <- names(input)
  
  # Check for reuired columns
  req_columns <- c('value', 'policy')
  if( any(! req_columns %in% original_tibble_names) ) {
    
    missing <- req_columns[ ! req_columns %in% original_tibble_names ]
    stop('input is missing: ', paste(missing, collapse = ', '))
    
  } 
  
  # Only remove the refernce run results from the taget run results if the query contains 
  # output from both runs, otherwise return the values as is. 
  policy_column_entires <- unique(input$policy)
  
  if(length(policy_column_entires) == 2) {
    
    # Add the only tag to the policy column and reformat the tibble 
    # to spread the policy column, this will allow us to remove the reference no policy run from the 
    # target run.
    input %>% 
      mutate(policy = paste0(policy, '_only')) %>% 
      spread(policy, value) -> 
      policy_wide_tibble 
    
    # Identify the target run name.
    target_column_name <- names(policy_wide_tibble)[ ! names(policy_wide_tibble) %in% c(original_tibble_names, 'nop_only')]
    
    # Remove the no policy refercne run from the target run.
    policy_wide_tibble['value']  <- policy_wide_tibble[target_column_name] - policy_wide_tibble['nop_only']
    policy_wide_tibble['policy'] <- 'policy' 
    
    
    # Select the columns to save and concatenate the policy results with the original input
    # data tibble. The tibble returned will contain the values for the nop, the target run 
    # and then the policy (target - nop) run.
    policy_wide_tibble %>% 
      select(original_tibble_names) %>%  
      bind_rows(input) %>% 
      filter(!is.na(value))
    
    
  } else { 
    
    # The query only contains values from one type of run return original input
    filter(input, !is.na(value))
    
  }
  
}) -> 
  output

# 5. Format Units -----------------------------------------------------------------------------------------

# The CO2 prices query reports units in tC, but we would like to convert the to 
# units of tCO2. In order to do this mulitply by 44/12 (the ratio of the molecular/atomic wegihts)
output$`CO2 prices` %>%  
  mutate(value = value * 44/12,
         units = '1990$/tCO2') -> 
  output$`CO2 prices`

# CO2 emissions query results must also be converted. 
output$`CO2 emissions by region` %>%  
  mutate(value = value * 44/12,
         units = 'MTCO2') -> 
  output$`CO2 emissions by region`


# GCAM results were 1990$ but we would like to report values in terms of 2015$. 
output %>% 
  modify(function(input){
    
    if( any(grepl('1990', input$units)) ){
      
      input %>% 
        mutate(value = if_else(grepl('1990', input$units), value * 1.64753738, value), 
               units = if_else(grepl('1990', input$units), gsub('1990', '2015', units), units)) 
      
    } else {
      
      input
      
    }
    
  }) -> 
  output


# 6. Save data -------------------------------------------------------------------------------


output_file <- file.path(BASE, 'out-2', sub_dir, 'query_results.rda')

save(output, file = output_file)





