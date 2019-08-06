# Purpose: This script parses and processes query data from merged rgcam project and saves 
# results as a flat list as a rda file by by the selection method. The idea is that the 
# output from this script is ready to plot with limited processing in the visulization 
# scripts. This script requires several user decisions to be made in section 0. Because of how large 
# the land allocation data is it has to be processed by a seperate list otherwise the R project will 
# exceed the allowed memory limit. 

# 0. Set Up ---------------------------------------------------------------------------------------------
# Load libs
library(dplyr)
library(tidyr)
library(purrr)
library(rgcam)
library(stringr)

# A vector of sub directories that contains the projects to merge together before processing, this is 
# useful when the number of GCAM runs has to be split up into multiple batches. 
sub_dirs <- c('CMSpaper') 

# Specify where to write the output to, if there is only one sub directory being use as input then set the
# out_dir equal to the sub_dirs. 
out_dir  <- 'CMSflux'

# User Decisions - decide what project to process and what paramter mapping file to use. 
proj_name    <- 'proj_merge.proj'
mapping_name <- 'A.Hector_GCAM_parameters.csv'

# Directories
BASE     <- '.'                                             # Should be the hector-SA-npar project location
OUTPUT   <- file.path(BASE, 'output', 'out-2', out_dir); dir.create(OUTPUT)

# Define the filer_name factor order, this vector will be used to orderr the filter_name 
# factor to help with the plotting process.
filterName_factor <- c('None', 'NPP', 'atm CO2', 'atm CO2, NPP', 'Tgav', 'NPP, Tgav', 'atm CO2, Tgav', 'atm CO2, NPP, Tgav') 


# 1. Import Data ---------------------------------------------------------------------------------------

# If there is only one place to pull the data from (GCAM runs were done in one batch) the import results, 
# however if the GCAM runs were done in multiple batches then merge the projects and parameter mapping files.
if(length(sub_dirs)  == 1){
  
  # Import the merged R project that contains all of the GCAM output data. Also import the Hector paramter 
  # mapping file to add information about why a particular hector paramter set combination was used. 
  path      <- list.files(file.path(BASE, 'output', 'out-2', sub_dirs), proj_name, full.names = TRUE)
  gcam_proj <- get(load(path))
  
  path             <- list.files( file.path(BASE, 'output', 'out-2', sub_dirs), mapping_name, full.names = TRUE)
  gcam_run_mapping <- read.csv(path)
  
  
} else {
  
  # Merge both of the projects together and save output. 
  prjList   <- unlist(lapply(sub_dirs, function(x){file.path(BASE, 'output', 'out-2', x, proj_name)}))
  gcam_proj <- mergeProjects(prjname = file.path(OUTPUT, proj_name), prjlist = prjList, saveProj = TRUE)
  
  # Concatenate the parameter mapping files. 
  suppressWarnings(gcam_run_mapping <- bind_rows(lapply(sub_dirs, function(x){
    read.csv(file.path(BASE, 'output', 'out-2', x, mapping_name))
    })))
  
  # Save the paramter mapping file. 
  write.csv(gcam_run_mapping, file = file.path(OUTPUT, mapping_name), row.names = FALSE)
  
}


# 3. rgcam project -> list ----------------------------------------------------------------------------

# Exract all of the queries of intrest from the project and save as individual tibbles in a list. 
queries <- c("CO2 prices", "Climate forcing", "CO2 emissions by region", "Primary Energy Consumption (Direct Equivalent)", "Global mean temperature")
data        <- lapply(queries, getQuery, projData = gcam_proj)
names(data) <- queries

# 4. Format Query Output  -------------------------------------------------------------------------------------

# The gcam_proj is curently sctrucured as an rgcam project output, a nested list. 
# Format the information in the tibble in preperation for the join with the mapping file. 

modify(data, function(input){
  
  input %>% 
    distinct %>% 
    mutate(run_name = str_extract(scenario, 'hectorSA-[0-9]{4}')) %>%  
    mutate(policy = gsub('hectorSA-[0-9]{4}', '', scenario)) %>% 
    select(-scenario) %>% 
    mutate(policy = gsub('_', '', policy)) %>%
    left_join(gcam_run_mapping, by = "run_name") %>%               # Add the filter_name category by joining the gcam_run_mapping file
    rename(units = Units) 
  
}) -> 
  formatted_data 



# 5. Calculate policy results ----------------------------------------------------------------------------------------

# Remove the reference run from the policy values for each query, only for the queries that contain data 
# for both the reference and target runs. Most of the runs we are going to be looking at will only have 
# target GCAM results. 

map(formatted_data, function(input){
  
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

# 6. Format Units -----------------------------------------------------------------------------------------

# The CO2 prices query reports units in tC, but we would like to convert the to 
# units of tCO2. In order to do this mulitply by 44/12 (the ratio of the molecular/atomic wegihts)
output$`CO2 prices` %>%  
  mutate(value = value * 12/44,
         units = '1990$/tCO2') -> 
  output$`CO2 prices`

# CO2 emissions query results must also be converted. 
output$`CO2 emissions by region` %>%  
  distinct %>% 
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





# # 7. Land Allocation -------------------------------------------------------------------------------------------
# # Because of how large the land allocation data frame is it must be processed seperately from the other queries.
# # Get the land allocation query data here and rename the columns so that it is easier data to work with. 
# getQuery(gcam_proj, "Land Allocation") %>% 
#   rename(land_allocation = `land-allocation`) %>% 
#   select(units = Units, region, land_allocation, year, value, scenario) -> 
#   data
# 
# # Because of inconsistencies in the way that the land_allocaiton information is named there is a bit of 
# # processing that must be done to format the data. 
# 
# # Define the namming connvention for the land allocaiton column, this will be used to seperate the 
# # crop info from the other land use information. 
# standard_pattern <- '([A-Za-z0-9-]+)_([A-Za-z0-9-]+)_(RFD|IRR)_(hi|lo)'
# 
#  # Subset the land allocaiton data so that it only includes information for crops. 
# data %>%
#   filter(grepl(pattern = standard_pattern, land_allocation)) %>% 
#   # If the crop is biomass you will need to rename the crop to tree-biomass so 
#   # that the seperate by "_" can work. 
#   mutate(land_allocation = if_else(grepl(pattern = 'biomass_', land_allocation), 
#                                    gsub(pattern = 'biomass_', replacement = 'biomass-', x = land_allocation), land_allocation)) %>% 
#   mutate(land_allocation = if_else(grepl(pattern = 'Root_Tuber', land_allocation), 
#                                    gsub(pattern = 'Root_Tuber', replacement = 'RootTuber', x = land_allocation), land_allocation)) %>%  
#   # Seperate the land allocation information out into the crop, basin, irrigation, feterlizer codes. 
#   separate(land_allocation, into = c('crop', 'basin', 'irrigation', 'fertlizer'), sep = '_') -> 
#   crop_basin_irr_fert1
# 
# # Subset the land allocation data so that it only includes the crop_basins that do not have irrigation or fertlizer data. 
# # Add NA codes to the irrigation and ferzelier columns in preperation for the join with the crop basin irrigation 
# data %>% 
#   filter(!grepl(pattern = standard_pattern, land_allocation)) %>% 
#   separate(land_allocation, into = c('crop', 'basin'), sep = '_') %>%  
#   mutate(irrigation = NA,
#          fertlizer = NA) -> 
#   crop_basin_irr_fert2
# 
# # Calculate the crop_basin_irr_fert_global values. 
# crop_basin_irr_fert1 %>% 
#   bind_rows(crop_basin_irr_fert2) %>%  
#   distinct %>% 
#   group_by(units, crop, year, scenario) %>% 
#   summarise(value = sum(value)) %>%  
#   ungroup -> 
#   global_crop_basin_irr
# 
# # Add the mapping information to the global data set of crop, irrigation, fertlizer. 
# global_crop_basin_irr %>% 
#   mutate(run_name = gsub('_RF-2p6', '', scenario)) %>%  
#   full_join(gcam_run_mapping %>% 
#               select(run_name, filter_name, beta, q10, s, diff), 
#             by = 'run_name') -> 
#     output[['global crop irr fert']]
# 

# 8. Save Output --------------------------------------------------------------------------------------
# Save the output 
saveRDS(output, file = file.path(OUTPUT, 'C.GCAM_rslts.rds'))

