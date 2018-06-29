# Purpose: This code contains the functions used to select Hector runs based on different 
# criteria. The idea is that parameter combinations for the selected Hector runs will then 
# be used in GCAM-Hector.


# select_extreme_values() -------------------------------------------------------------------------------

# This function selects the extreme max and min values within different filter categories. The function 
# uses two inputs, a wide data frame containing columns of T/F values indicating whether or not the Hector 
# run matches observations based on the Dn/Dc metric, and a value column of a Hector variable from a single year
# to select the max and min values from. The obs_list is a vector of all the observations to look into. 
# This function will take the extremes from the unfilter category and then layer / combine all of the 
# the observational products on top of each other.


select_extreme_values <- function(data, obs_list){
  
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
        # Subset the passing filter runs for the min and max extreme values.
        filter(value == min(value) | value == max(value)) %>% 
        select(run_name, variable, year, value, filter_name) -> 
        intermediate
      
      # Now add a column for indicating if the 2100 value is an extreme
      # or a minimum.
      if(nrow(intermediate) == 1){
        
        # If there is only one Hector run that matches the observaitonal 
        # filter combination then extreme will = NA.
        output <- intermediate
        output$extreme <- 'NA'
        
      } else {
        
        # Add min and max extreme indicators.
        intermediate %>% 
          mutate(extreme = if_else(value == max(value), 'max', 'min')) -> 
          output
      }
      
      # Only return the distinct output.
      distinct(output)
      
    }))
    
  }, simplify = F)) -> 
    filtered_extreme
  
  
  # Now subset the data to find the unfiltered extremes 
  data %>% 
    mutate(filter_name = 'None') %>% 
    # Subset all the Hector runs for the min and max extreme values.
    filter(value == min(value) | value == max(value)) %>% 
    select(run_name, variable, year, value, filter_name) %>% 
    mutate(extreme = if_else(value == max(value), 'max', 'min')) ->
    unfiltered_extreme
  
  # Concatenate the filtered and unfiltered extreme into a single data frame 
  # and return. 
  bind_rows(filtered_extreme, unfiltered_extreme)
  
}


# categorize_runs() -------------------------------------------------------------------------------

# This function categorizes a run into the different observational product filters is matches 
# based on the Dn metric.


categorize_runs <- function(data, obs_list){
  
  if( any(! obs_list %in% names(data)) ){stop('Contents of obs_list not in data')}
  
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
        mutate(filter_name = filter_name)
      
    }))
    
  }, simplify = F)) -> 
    categorized_runs
  
  
  # Now categorize the runs into the unfiltered area 
  data %>% 
    mutate(filter_name = 'None') ->
    unfitlered
  
  # Concatenate the filtered and unfiltered  runs into a single data frame 
  # and return. 
  bind_rows(categorized_runs, unfitlered)
  
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
    
    # If there are no runs that pass through all of the filters then 
    # add a 0 run count. 
    if(nrow(output) == 0) {
      
      output <- tibble(filter_name = filter_name, count = 0)
      
    }
    
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


# select_95th_values() -----------------------------------------------------------------------

# This function selects runs that most closely resemble the upper and lower 95th CI values 
# for some input. Requires and data frame that contains a value and a filter_name columns. 

select_95th_values <- function(input){
  
  # Check for required columns
  req_cols <- c('value', 'filter_name')
  if( any(!req_cols %in% names(input)) ){stop('Input data frame is missing required columns')}
  
  # Calculate the mean, sd, and the sample size.
  mean  <- mean(input[['value']])
  sd    <- sd(input[['value']] )
  n     <- length(input[['value']])
  
  # Critical value for alpha equal 0.05 * sd / square root of the sample size. 
  # Assumes a normal distribtuion
  error <- 1.96 * ( sd / n ^ (.5) )
  
  # +/- the error from the mean to get the upper and lower 95 CI values.
  upper <- mean + error
  lower <- mean - error
  
  # Determine wich index is the closest to the upper and lower CI values. 
  upper_index <- which.min( abs(input[['value']] - upper) )
  lower_index <- which.min( abs(input[['value']] - lower) )
  
  bind_rows(tibble(run_name = input$run_name[upper_index], 
                   value = input$value[upper_index],
                   CI_value = upper, 
                   CI = 'upper'), 
            tibble(run_name = input$run_name[lower_index], 
                   value = input$value[lower_index],
                   CI_value = lower, 
                   CI = 'lower')) %>% 
    mutate(filter_name = unique(input[['filter_name']]))
  
}
