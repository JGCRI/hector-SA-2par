# Purpose: This code contains the functions used to select Hector runs based on different 
# criteria. The idea is that parameter combinations for the selected Hector runs will then 
# be used in GCAM-Hector.


# select_extreeme_values() -------------------------------------------------------------------------------

# This function selects the extreme max and min values within different filter categories. The function 
# uses two inputs, a wide data frame containing columns of T/F values indicating whether or not the Hector 
# run matches observations based on the Dn/Dc metric, and a value column of a Hector variable from a single year
# to select the max and min values from. The obs_list is a vector of all the observations to look into. 
# This function will take the extremes from the unfilter category and then layer / combine all of the 
# the observational products on top of each other.


select_extreeme_values <- function(data, obs_list){
  
  # The idea is that the selection function will be applied as many times as there should 
  # be layers of combinations of the observational products. Since we want to do 
  # all of the products individually and then stacked as many times as possible we will 
  # want to generate the observation combinations for as many times as we have observations.
  
  bind_rows(sapply(1:length(obs_list), FUN = function(index){
    
    # Generate all of the possible combinations for index number 
    # of stacked observations.
    filters <- combn(obs_list, index)
    
    # Now for each of the filters or possible observational combinations select the 
    # extreme max and min values for the Hector runs "match" all of the observations 
    # in the filter.
    bind_rows(apply(filters, 2, FUN = function(filters){
      
      # Format the a filter name based on the observation combination
      # used to subset the data.
      filter_name <- paste(filters, collapse = ', ' ) 
      
      data %>% 
        # Subset the data so that all of the Hector variables match
        # observations according to the Dn / Dc metrics. We can used 
        # filter_at to do this because we are working with a wide data 
        # frame that contains T/F if a Hector run has a matching Dn for 
        # a single variable.
        filter_at(filters, all_vars(. == TRUE)) %>% 
        # Add the filter name
        mutate(filter_name = filter_name) %>% 
        # Subset the passing filter runs for the min and max extreeme values.
        filter(value == min(value) | value == max(value)) %>% 
        select(run_name, variable, year, value, filter_name) -> 
        intermediate
      
      # Now add a column for indicating if the 2100 value is an extreeme
      # or a minimum.
      if(nrow(intermediate) == 1){
        
        # If there is only one Hector run that matches the observaitonal 
        # filter combination then extreeme will = NA.
        output <- intermediate
        output$extreeme <- 'NA'
        
      } else {
        
        # Add min and max extreem indicators.
        intermediate %>% 
          mutate(extreeme = if_else(value == max(value), 'max', 'min')) -> 
          output
      }
      
      # Only return the distinct output.
      distinct(output)
      
    }))
    
  }, simplify = F)) -> 
    filtered_extreems 
  
  
  # Now subset the data to find the unfiltered extreemes 
  data %>% 
    mutate(filter_name = 'None') %>% 
    # Subset all the Hector runs for the min and max extreeme values.
    filter(value == min(value) | value == max(value)) %>% 
    select(run_name, variable, year, value, filter_name) %>% 
    mutate(extreeme = if_else(value == max(value), 'max', 'min')) ->
    unfiltered_extreems
  
  # Concatenate the filtered and unfiltered extreemes into a single data frame 
  # and return. 
  bind_rows(filtered_extreems, unfiltered_extreems)
  
}


# run_count() -------------------------------------------------------------------------------

# This function counts the number of runs that match the different observational products. 


run_count <- function(data, obs_list){

# The idea is that the count function will be applied as many times as there should 
# be layers of combinations of the observational products. Since we want to do 
# all of the products individually and then stacked as many times as possible we will 
# want to generate the observation combinations for as many times as we have observations.

bind_rows(sapply(1:length(obs_list), FUN = function(index){
  
  # Generate all of the possible combinations for index number 
  # of stacked observations.
  filters <- combn(obs_list, index)
  
  # Now for each of the filters or possible observational combinations count select the 
  # number of passing runs.
  bind_rows(apply(filters, 2, FUN = function(filters){
    
    # Format the a filter name based on the observation combination
    # used to subset the data.
    filter_name <- paste(filters, collapse = ', ' ) 
    
    data %>% 
      filter_at(filters, all_vars(. == TRUE)) %>% 
      # Add the filter name
      mutate(filter_name = filter_name) %>% 
      group_by(filter_name) %>%  
      summarise(count = n()) %>% 
      ungroup -> 
      output
    
  }))
  
}, simplify = F)) -> 
  filtered_counts 


# Now subset the data to find the unfiltered counts 
data %>% 
  mutate(filter_name = 'None') %>% 
  group_by(filter_name) %>%  
  summarise(count = n()) %>% 
  ungroup ->
  unfiltered_counts

# Concatenate the filtered and unfiltered counts into a single data frame 
# and return. 
bind_rows(filtered_counts, unfiltered_counts)

}
