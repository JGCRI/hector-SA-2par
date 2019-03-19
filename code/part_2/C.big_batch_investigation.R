# Compare the big batch Hector-GCAM reference results with the stand along hector results. 
# Use the Hector-GCAM results to select the runs to use in Hector-GCAM with the policy and the 
# reference (written out to a data base now).

# 0. Set Up --------------------------------------------------------------------------------------
# Library 
library(dplyr)
library(tidyr)
library(ggplot2)

# Directories
BASE_DIR <- getwd()
INPUT_DIR <- file.path(BASE_DIR, 'output', 'out-2', 'big_batch_all')

# 1. Import Data ---------------------------------------------------------------------------------
# Import the Hector GCAM results from the big batch runs
read.csv(file.path(INPUT_DIR, 'B.hector_gcam_rslts.csv')) %>% 
  filter(year <= 2100) %>% 
  mutate(run_name = gsub(pattern = '_nop', replacement = '', x = gcam_run))-> 
  gcam_rslts_all

# Import the Hector parameters 
read.csv(file.path(INPUT_DIR, "A.Hector_GCAM_parameters.csv")) %>%  
  select(-X) -> 
  param_mapping 

# Import the Dn results
readr::read_csv(list.files(file.path(BASE_DIR,'output', 'out-1', 'hist_emissions'), 'E.Dn_metric_results.csv', 
                           full.names = T)) %>%  
  filter(filter_name %in% c('Tgav', 'Tgav, NPP, atmCO2 multi optimized')) %>% 
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  filter(passing) %>%  
  select(run_name, filter_name, passing) -> 
  passing_runs_mapping

readr::read_csv(list.files(file.path(BASE_DIR,'output', 'out-1', 'hist_emissions'), 'E.Dn_metric_results.csv', 
                           full.names = T)) %>% 
  select(run_name) %>% 
  mutate(filter_name = 'None') %>% 
  bind_rows(passing_runs_mapping) ->
  passing_runs_mapping


# Import the Hector results 
read.csv(file.path(BASE_DIR, 'output', 'out-1', 'hist_emissions', 'C.Tgav_hector_run_cleanup.csv'),
         stringsAsFactors = FALSE) %>%  
  left_join(passing_runs_mapping, by = 'run_name') -> 
  hector_rslts
  
# Add the passing filter name infomration to the gcam results. 
gcam_rslts_all %>% 
  left_join(passing_runs_mapping, by = 'run_name') %>%
  na.omit -> 
  gcam_rslts

# 2. Temperature Plots --------------------------------------------------------------------------
# Temperature from GCAM-Hector 
ggplot(data = filter(gcam_rslts, filter_name == 'Tgav' & year <= 2015)) + 
  geom_line(aes(year, value, group = gcam_run, color = filter_name)) + 
  geom_line(data = filter(gcam_rslts, !filter_name %in% c('Tgav', 'None') & year <= 2015), 
            aes(year, value, group = gcam_run, color = filter_name)) + 
  labs(title = 'Global Mean Temp from Ref GCAM-Hector', 
       y = 'deg C',
       x = 'year') + 
  theme_bw() + 
  theme(legend.title = element_blank(), 
        legend.position = 'bottom') -> 
  plot_gcam_hector_filtered_line

# Temperature from stand alone Hector  
ggplot(data = filter(hector_rslts, filter_name == 'None' & year <= 2015)) + 
  geom_line(aes(year, value, group = run_name, color = filter_name)) + 
  geom_line(data = filter(hector_rslts, filter_name == 'Tgav' & year <= 2015), 
            aes(year, value, group = run_name, color = filter_name)) + 
  geom_line(data = filter(hector_rslts, !filter_name %in% c('None', 'Tgav') & year <= 2015), 
            aes(year, value, group = run_name, color = filter_name)) + 
  labs(title = 'Global Mean Temp from Hector Historical Emissions Driven', 
       y = 'deg C',
       x = 'year') + 
  theme_bw() + 
  theme(legend.title = element_blank(), 
        legend.position = 'bottom') -> 
  plot_gcam_hector_filtered_line

# 3. Parallel coordinate plots -------------------------------------------------------------
# It looks like the paramter combinations do not really change that much.
# Paralellel coordinate plots for the top 5% 2050 and 2100 runs 
# First figure out the min and max cut offs. 
gcam_rslts %>%
  filter(year %in% c(2015, 2100)) %>% 
  dplyr::group_by(year, filter_name) %>%  
  dplyr::summarise(cutoff_min = quantile(value, probs = .0019), 
                   cutoff_max = quantile(value, probs = .998)) -> 
  temp_cutoffs

param_mapping %>%  
  gather(param, value, -run_name, -run_index) %>% 
  group_by(param) %>%  
  summarise(mean_value = mean(value)) %>% 
  ungroup -> 
  mean_param_value

# Subset the Hector results for the min/max. 
gcam_rslts %>% 
  filter(year %in% c(2015, 2100)) %>% 
  left_join(temp_cutoffs, by = c('year', 'filter_name')) %>% 
  # Categorize the runs 
  mutate(keep = if_else(value <= cutoff_min, 'min', 'NA')) %>% 
  mutate(keep = if_else(value >= cutoff_max, 'max', keep)) %>%  
  filter(keep != 'NA') %>% 
  select(year, keep, run_name, filter_name) -> 
  gcam_rslts_cut_off
  
# How many runs?
gcam_rslts_cut_off %>% 
  filter(year == 2100) %>% 
  group_by(keep, filter_name) %>% 
  summarise(count = n_distinct(run_name))

gcam_rslts_cut_off %>% 
  left_join(param_mapping, by = 'run_name') %>% 
  gather(param, value, beta, diff, q10, s) %>% 
  left_join(mean_param_value, 
            by = "param") %>% 
  mutate(value = value / mean_value) ->
  parallel_coord_data

# Start by just making a single parallel coordinate plot... 
parallel_coord_data %>% 
 # filter(keep == 'min') %>% 
  mutate(year = as.character(year)) %>% 
  ggplot(aes(param, value, group = run_name, color = keep)) + 
  geom_line(alpha = 0.5) + 
  facet_wrap('year', ncol = 1) + 
  theme_bw()


# 4. Select policy runs -------------------------------------------------------------------------
# Select the 5 upper and lower runs to use as inputs into GCAM-Hector.
# Subset the GCAM results for the 2100 values.
gcam_rslts %>% 
  filter(year == 2100 & variable == 'Tgav') %>% 
  select(year, filter_name, run_name, variable, value) -> 
  gcam_2100_values

# The number of runs to select from the extremes 
extreme_runs <- 5

# Count the number of runs in each filter name.
gcam_2100_values %>% 
  group_by(filter_name) %>% 
  summarise(total_count = n_distinct(run_name)) %>% 
  ungroup -> 
  filter_run_counts

# Select the 5 warmest and 5 coolest runs for the runs that pass through 
# each obs filter.
gcam_2100_values %>%  
  left_join(filter_run_counts, by = 'filter_name') %>% 
  split(.$filter_name) %>%  
  lapply(function(input){
    
    input %>% 
      # arrange the runs by increasing temp and add a rank order.
      arrange(filter_name, value) %>%  
      mutate(rank = 1:nrow(.)) %>%  
      mutate(max_cutoff = nrow(.) - extreme_runs) %>%  
      # keep the first 5 and the last 5 runs
      mutate(keep = if_else(rank <= 5, 'min', 'NA')) %>% 
      mutate(keep = if_else(rank >= (max_cutoff + 1), 'max', keep)) %>% 
      filter(keep != 'NA') %>%  
      select(run_name, filter_name, keep)
 
  }) %>% 
  bind_rows %>% 
  # Format the select hector-SA-npar runs into a dataframe that can be used 
  # to generate the hector gcam ini files.
  select(run_name) %>% 
  distinct %>% 
  inner_join(param_mapping, by = 'run_name') %>% 
  select(-run_index) -> 
  extreme_params
  
# Write the output
output_dir <- file.path(BASE_DIR, 'output', 'out-2', 'big_batch_policy'); dir.create(output_dir)  
write.csv(extreme_params, file = file.path(output_dir, 'A.Hector_GCAM_parameters.csv'))

