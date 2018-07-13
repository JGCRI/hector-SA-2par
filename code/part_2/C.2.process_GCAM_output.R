
# Purpose: This script parses and processes query data from merged rgcam project and saves results as a flat 
# list as a rda file by by the selection method. The idea is that the output from this script is ready to 
# plot with limited processing in the visulization scripts. This script requires several user decisions 
# to be made in section 0. 

# 0. Set Up ---------------------------------------------------------------------------------------------
# Make sure that the working directory is equal to the project directory
if( ! 'hector-SA-npar.Rproj' %in% list.files() ){ stop( 'Working dir must be the project location' ) }

# Load libs
library(dplyr); library(tidyr)
library(purrr); library(rgcam)
library(stringr)

# Directories
BASE    <- getwd() 
sub_dir <- 'rcp26'

# User Decisions - decide what project to process and what paramter mapping file to use. 
proj_name    <- 'proj_merge_extreme.proj'
mapping_name <- 'A.Hector_GCAM_parameters_mapping.csv'


# Define the filer_name factor order, this vector will be used to orderr the filter_name 
# factor to help with the plotting process.
filterName_factor <- c('None', 'NPP', 'atm CO2', 'atm CO2, NPP', 'Tgav', 'NPP, Tgav', 'atm CO2, Tgav', 'atm CO2, NPP, Tgav') 


# 1. Import Data ---------------------------------------------------------------------------------------

# Import the merged R project that contains all of the GCAM output data. Also import the Hector paramter 
# mapping file to add information about why a particular hector paramter set combination was used. 
path      <- list.files(file.path(BASE, 'out-2'), proj_name, full.names = TRUE)
gcam_proj <- get(load(path))

path             <- list.files( file.path(BASE, 'out-2'), mapping_name, full.names = TRUE)
gcam_run_mapping <- read.csv(path, stringsAsFactors = FALSE)


# 3. Format Query Output  -------------------------------------------------------------------------------------

# The gcam_proj is curently sctrucured as an rgcam project output, a nested list. 
# Format the information in the tibble in preperation for the join with the mapping file. 

modify_depth(gcam_proj, 2, function(input){
  
  input %>% 
    mutate(run_name = str_extract(scenario, 'hectorSA-[0-9]{4}')) %>%  
    mutate(policy = gsub('hectorSA-[0-9]{4}', '', scenario)) %>% 
    mutate(policy = gsub('_', '', policy)) %>%
    inner_join(gcam_run_mapping, by = "run_name") %>%               # Add the filter_name category by joining the gcam_run_mapping file
    rename(units = Units) %>%                                       # Rename the units
    select(-scenario)->                                             # Drop the scenario column                          
    output
  
  output$filter_name <- factor(output$filter_name, filterName_factor, ordered = TRUE)   # Add factor levels

  output  # Return output
    
  }) -> 
  formatted_gcam_output


# 4. rgcam project -> list ----------------------------------------------------------------------------

# Exract all of the queries from and save results as individual tibbles in a list.
query_list        <- listQueries(formatted_gcam_output)
names(query_list) <- query_list

data <- map(query_list, getQuery, projData = formatted_gcam_output)


# 5. Calculate policy results ----------------------------------------------------------------------------------------

# Remove the reference run from the policy values for each query, only for the queries that contain data 
# for both the reference and target runs. 

map(data, function(input){
  
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

# TODO the max NPP and None 2100 parameters are too hot to solve a target policy run and return the referene run, 
# remove these runs to prevent confusion with plotting. 
output <- modify(output, function(input){ filter(input, ! run_name %in% c('hectorSA-0975', 'hectorSA-3591')) })


# 6. Save the data by selection reason -------------------------------------------------------------------------------

selection <- unique(output$`CO2 concentrations`$keep)

map(selection, function(keep, outputPath = outputDir, outputBaseName = 'GCAM'){
  
  modify(output, function(input){filter(input, keep == keep) }) %>% 
    save(file = file.path(outputPath, paste0(outputBaseName, '_', keep, '.rda' )))
  
})

# End