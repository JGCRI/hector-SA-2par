
# Purpose: This script parses and processes query data from merged rgcam project and saves results in a list.
# The idea is that the output from this script it ready to plot to limit the amount of data processing that 
# must be done in the visulaization scripts. See section 1 for important user decisions for this script.


# 0. Set Up ---------------------------------------------------------------------------------------------
# Make sure that the working directory is equal to the project directory
if( ! 'hector-SA-npar.Rproj' %in% list.files() ){ stop( 'Working dir must be the project location' ) }

# Load libs
library(dplyr); library(tidyr)
library(purrr); library(rgcam)

# Directories
BASE    <- getwd() 
sub_dir <- 'rcp26'

# 1. User Decisions -------------------------------------------------------------------------------------

# Define the filer_name factor order, this vector will be used to orderr the filter_name factor to help with 
# the plotting process.

filterName_factor <- c('None', 'NPP', 'atm CO2', 'atm CO2, NPP', 'Tgav', 'NPP, Tgav', 'atm CO2, Tgav', 'atm CO2, NPP, Tgav') 


# 2. Import Data ---------------------------------------------------------------------------------------

# Import the project created as part 2 leve C output that contains all of the rgcam Queries and import the 
# Hector_GCAM_parameters_mapping, the mapping file will be used to categorize the GCAM runs by passing 
# obersvational filter.

path      <- list.files(file.path(BASE, 'sub-out'), 'proj_merge.proj', full.names = TRUE)
gcam_proj <- get(load(path))


path             <- list.files( file.path(BASE, 'sub-out') , 'A.Hector_GCAM_parameters_mapping.csv', full.names = TRUE)
gcam_run_mapping <- read.csv(path, stringsAsFactors = FALSE)


# 3. Format Query Output  -------------------------------------------------------------------------------------


# The gcam_proj is curently sctrucured as an rgcam project output, a nested list. Here we use purrr:modify_depth 
# to format the query results. 

modify_depth(gcam_proj, 2, function(input){
  
  input %>% 
    separate(col = scenario, into  = c("run_name", "policy"), sep = "_", remove = TRUE) %>%        # Extract the run_name and the policy name from the scenario name
    inner_join(select(gcam_run_mapping, run_name, filter_name, extreme), by = "run_name") %>%      # Add the filter_name category by joining the gcam_run_mapping file
    rename(units = Units) ->                                                                       # Rename units 
    output 
    
  output$filter_name <- factor(output$filter_name, filterName_factor, ordered = TRUE)   # Add factor levels
    
  output  # Return output
    
  }) -> 
  formatted_gcam_output


# 4. Parse Out Queries of Intrest  ----------------------------------------------------------------------------

# Exract all of the queries from and save results as individual tibbles in a list.
query_list        <- listQueries(formatted_gcam_output)
names(query_list) <- query_list

data <- map(query_list, getQuery, projData = formatted_gcam_output)


# 5. Remove the Reference Values  ----------------------------------------------------------------------------

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


# 6. Save Output  -------------------------------------------------------------------------------------------

output_file <- file.path(BASE, 'sub-out', 'GCAM_output.proj')

save(output, file = output_file)

# End
