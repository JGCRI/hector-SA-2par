# Compare the rcp 26 and rcp 85 min and max temperatures from stand alone Hector, compare the 
# paramter values and select the parameters that are going to be used in the GCAM policy runs. 

# 0. Set Up -------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# The number of  the extreme runs to select from each catgory
select_n_runs <- 25


# 1. Import Data & Format ---------------------------------------------------------------------------
# Import the stand alone hector temperature
rcp26_data <- read.csv(file.path('.', 'output', 'out-1', 'rcp26', "C.Tgav_hector_run_cleanup.csv"))
#rcp85_data <- read.csv(file.path('.', 'output', 'out-1', 'rcp85', "C.Tgav_hector_run_cleanup.csv"))

# Add scenario information
rcp26_data$scenario <- 'rcp26'
#rcp85_data$scenario <- 'rcp85'

# Import the parameter values 
param_mapping <- read.csv(file.path('.', 'output', 'out-1', 'hist_emissions', "A.par4_combinations.csv")) 

# Import the categorized runs 
read.csv(file.path('.', 'output', 'out-1', 'hist_emissions', "E.Dn_metric_results.csv")) %>% 
  filter(filter_name %in% c('Tgav', 'Tgav, NPP, atmCO2 multi optimized')) %>% 
  filter(Dn <= Dc) %>% 
  left_join(param_mapping, by = 'run_name') %>% 
  select(run_name, filter_name, beta, q10, s, diff) ->
  filter_param_mapping


# 2. Quick Sanity Plot ---------------------------------------------------------------------
# These plots are not categorized by the observational filering process yet. 
# A summarized line plot
rcp26_data %>%  
  #bind_rows(rcp85_data) %>% 
  filter(year %in% floor(seq(from = 1750, to = 2100, length.out = 10))) %>% 
  ggplot(aes(year, value, color = scenario, group = run_name)) + 
  geom_line() + 
  facet_wrap('scenario') + 
  labs(title = 'Comparison of RCP 2.6 and 8.5 Tgav', 
       y = 'deg C') -> 
  plot_temp

# Add the observational filter information
plot_temp$data %>%  
  left_join(filter_param_mapping, by = 'run_name') %>% 
  na.omit -> 
  filtered_data 

ggplot() + 
  geom_line(data = filter(filtered_data, filter_name == 'Tgav'), 
            aes(year, value, color = filter_name, group = run_name)) + 
  geom_line(data = filter(filtered_data, filter_name != 'Tgav'), 
            aes(year, value, color = filter_name, group = run_name)) + 
  facet_wrap('scenario') 

# Density plot of the year 2100 temperature 
rcp26_data %>%  
#  bind_rows(rcp85_data)  %>% 
  filter(year == 2100) %>% 
  ggplot(aes(value, fill = scenario, color = scenario)) + 
  geom_density(alpha = 0.7) + 
  labs(title = '2100 Tgav PDF')

# Density plot of year 2100 obs filtered runs
filtered_data %>% 
  filter(year == 2100) %>% 
  ggplot(aes(value, fill = filter_name, color = filter_name)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap('scenario') + 
  labs(title = '2100 Tgav PDF')


# 3. Extract the Extreme Temp ----------------------------------------------------
# How many runs are in each category 
filtered_data %>%  
  select(run_name, filter_name) %>% 
  distinct() %>% 
  dplyr::group_by(filter_name) %>%  
  dplyr::summarise(count = n_distinct(run_name)) %>% 
  ungroup ->
  total_run_count

# Rank the runs by 2100 temperature 
filtered_data %>%  
  filter(year == 2100) %>% 
  distinct %>% 
  # Clean up the list so that it only contains actual data 
  split(interaction(.$filter_name, .$scenario)) %>% 
  rlist::list.clean(fun = function(input){nrow(input) <= 1}) %>% 
  lapply(function(input){
    
    # Pick the min 
    input %>% 
      arrange(value) %>%  
      mutate(rank = 1:nrow(.)) %>% 
      left_join(total_run_count, by = 'filter_name') %>% 
      mutate(cut_off = count - select_n_runs + 1) %>% 
      filter(rank <= select_n_runs)  %>% 
      mutate(keep = 'min') -> 
      keep 
    
    input %>% 
      arrange(desc(value)) %>%  
      mutate(rank = 1:nrow(.)) %>% 
      left_join(total_run_count, by = 'filter_name') %>% 
      mutate(cut_off = count - select_n_runs + 1) %>% 
      filter(rank <= select_n_runs) %>%  
      mutate(keep = 'max') %>% 
      bind_rows(keep) %>% 
      select(run_name, units, year, value, scenario, filter_name, beta, q10, s, diff, keep)
    
    
  }) %>%
  bind_rows() -> 
  extreme_runs 

extreme_runs %>% 
  group_by(filter_name, keep, scenario) %>%  
  summarise(n = n_distinct(run_name))


# Now lets look at the paramter values by looking at the parallel coordinate plots 
# Start by calcualting the mean paramter value. 
filter_param_mapping %>% 
  gather(param, param_value, beta, q10, s, diff) %>%  
  group_by(param) %>%  
  summarise(mean_param = mean(param_value)) %>% 
  ungroup -> 
  mean_param_mapping

# Normalize the paramter values by the mean sampeled param value 
extreme_runs %>%  
  gather(param, param_value, beta, q10, s, diff) %>%  
  left_join(mean_param_mapping, by = 'param') %>%  
  mutate(value = param_value / mean_param) -> 
  param_parallel_plot

ggplot(data = param_parallel_plot) + 
  geom_line(aes(param, param_value, color = filter_name, group = run_name), 
            alpha = 0.7, 
            size = 1.5) + 
  facet_grid(keep ~ scenario) + 
  theme_bw(base_size = 16) +
  theme(legend.position = 'bottom') + 
  labs(title = 'Min/Max Extreme 2100 Temp Parallel Coordinate Plots')



output_dir <- file.path('.', 'output', 'out-2', 'totalCost'); dir.create(output_dir)
# Save a copy of the parameter values to use in the GCAM runs 
extreme_runs %>% 
  select(run_name, scenario, filter_name, beta, q10 , s, diff, keep) -> 
  out 

# The runs that we are interested in the total discount policy costs for. 
out %>% 
  filter(run_name %in% c("hectorSA-0650", "hectorSA-1155", "hectorSA-0319", "hectorSA-2600")) -> 
  discount

  # Save the batch Hector GCAM parameters as a csv file
  write.csv(discount, file = file.path(output_dir, 'A.Hector_GCAM_parameters.csv'))
 
