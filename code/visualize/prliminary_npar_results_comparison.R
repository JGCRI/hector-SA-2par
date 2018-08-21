
# Purpose: Compare output from the different paramter combiantions, so the runs 
# that hold carbon cycle constant vs those that have all 4 parameters varying. 
# This script is intended to answer https://github.com/JGCRI/hector-SA-npar/issues/18 


# Thoughts: Hmmmmm we are going to have to wait till we get the results from the other Hector 
# runs but this is not what I was expecting. I would have thought that the paramter selection 
# would have picked all or been less restricive, I am supprised at how restrictive it is and 
# why NPP picked a q10 range that is in consistent, besides we don't think that q10 and 
# NPP would even be related unless there is some sort of feedback but I don't think that there 
# would be! I think that the most useful/obviously intersted plot from this is the 2100 temp 
# range from the varying 4 parameters mapping file. the vayring 4 params and the varyign q10 
# used the same q10 values. The beta paramter selection plot was more along the lines of what 
# I was expecting. The beta paramter space is more along the lines of what I thought was going to happen 
# vs the q10.

# 0. Set Up ---------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)

# The BASE directpry should be the project directory location
BASE <- getwd()

# A vector of the out/sub_dirs to plot, this vector will be used to create a pattern to prevent
# unwantted results from being imported.
sub_dirs <- c('vary_4_params','vary_beta_q10', 'vary_diff_s') # vary_q10_only', 'vary_beta_only',)

# Factor order for the standalone Hector plots 
hector_filter_order <- c('None', 'atm CO2', 'NPP', 'Tgav', 'atm CO2, NPP', 'atm CO2, Tgav', 'NPP, Tgav', 'atm CO2, NPP, Tgav')
sub_dir_order       <- sub_dirs

# Import the part 2 GCAM paramter mapping file for the vary_4_params, use the temp results to determine the 
# temperature contribtuion from the carbon cylce paramters.
GCAM_mapping <- read_csv("C:/Users/dorh012/Documents/hector-SA-npar/out-2/vary_4_params/A.Hector_GCAM_parameters_mapping.csv")


# 1. Import Data ---------------------------------------------------------------------------------

# Import the filter flag csvs. 
sub_dirs %>% 
  lapply(function(x){
    
    path <- list.files(file.path(BASE, 'out-1', x), 'filter_flag.csv', recursive = F, full.names = TRUE) 
    
    readr::read_csv(file = path) %>%  
      mutate(sub_dir = x)
    
  }) %>% 
  bind_rows -> 
  filter_flag

# Import the temperature data 
all_temp_paths <- list.files(file.path(BASE, 'out-1'), 'C.Tgav_hector_run_cleanup.csv', recursive = T, full.names = TRUE) 
temp_paths     <- all_temp_paths[grepl(pattern = paste(sub_dirs, collapse = '/|'), all_temp_paths)]

temp_paths %>%
  lapply(function(x){

    read.csv(file = x, stringsAsFactors = FALSE) %>%
      mutate(sub_dir = basename(gsub('/C.Tgav_hector_run_cleanup.csv', '', x))) %>% 
      mutate(value = as.numeric(value))

  }) %>%
  bind_rows ->
  temp_data


# 2. Run Count Plot ---------------------------------------------------------------------------------
# Count the number of passing runs 
filter_flag %>%  
  select(-run_name, -run_index, -beta, -q10, -s, -diff) %>% 
  gather(filter, passing, -sub_dir) %>% 
 # na.omit %>% # this is because we are still waiting on Hector results from the part 2! 
  group_by(sub_dir, filter) %>% 
  summarise(count = sum(passing)) %>% 
  ungroup -> 
  run_count


run_count$filter <- factor(run_count$filter, levels = hector_filter_order, ordered = TRUE)
run_count$sub_dir <- factor(run_count$sub_dir, levels = sub_dir_order, ordered = TRUE)

# plot the run count 
ggplot(data = run_count) + 
  geom_col(aes(filter, count, fill = sub_dir), 
           position = 'dodge') + 
  theme(legend.title = element_blank()) + 
  labs(title = 'Run count comparing the number of passing runs', 
       subtitle = 'Hector results from runs with n varying paramters')


# 3. Plot Temperature Data ---------------------------------------------------------------------------------

# Add the filter information to the temperature data.
temp_data %>%  
  full_join(filter_flag, by = c('run_name', 'sub_dir')) %>%  
  gather(filter, passing, `atm CO2`, `NPP`, `Tgav`, `None`, `atm CO2, NPP`, `atm CO2, Tgav`, `atm CO2, NPP, Tgav`) %>% 
  filter(passing) %>% 
  select(-passing) ->
  temp_data_all

temp_data_all$sub_dir <- factor(temp_data_all$sub_dir, levels = sub_dir_order, ordered = TRUE)
temp_data_all$filter  <- factor(temp_data_all$filter, hector_filter_order, ordered = TRUE)

temp_data_all %>% 
  filter(year == 2100) %>% 
  ggplot(data = .) + 
  geom_jitter(aes(sub_dir, value, color = sub_dir)) + 
  facet_wrap('filter') +
  labs(y = '2100 temp') -> 
  temp_plot1


temp_data_all %>% 
  filter(year == 2100) %>% 
  group_by(sub_dir, filter) %>% 
  summarise(range = abs(diff(range(value)))) %>% 
  ungroup() %>% 
  select(filter, sub_dir, range) %>% 
  arrange(filter, sub_dir) -> 
  temp_range_2100

temp_range_2100$sub_dir <- factor(temp_range_2100$sub_dir, levels = sub_dir_order, ordered = TRUE)

temp_range_2100 %>% 
  knitr::kable(.)

# 4. Paramter Space Plots ---------------------------------------------------------------------------------

filter_flag %>%   
  gather(parameter, param_value, beta, q10, s, diff) %>% 
  gather(filter, passing, -run_name, -run_index, -sub_dir, -parameter, -param_value) %>% 
  filter(passing) %>% 
  split(.$parameter) %>% 
  # Map a the layered geom_jitter + geom_boxplot function to parameter data frames.
  map(function(input, subtitle = 'parameter space of the single filters'){
    
    # Save a copy of the paramter name 
    param_name <- unique(input$parameter) 
    
    # Save the run count for each filter category.
    # input %>% 
    #   group_by(filter, sub_dir) %>% 
    #   summarise(max = max(param_value), count = max(count)) -> 
    #   count_tibble
    
    # Plot
    input %>% 
      ggplot(aes(filter, param_value, color = filter)) + 
      geom_jitter() + 
     # geom_text(data = count_tibble, aes(filter, max, label = count), col = 'black', size = 5,  vjust=-.3) +
      geom_boxplot(data = input, aes(filter, param_value), color = 'black', outlier.shape = NA, fill = NA, size = 1) + 
      labs(x = NULL, 
           y = 'parameter value', 
           title = param_name) + 
      theme_bw() + 
      facet_wrap('sub_dir') + 
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))

    
  }) -> 
  parameter_plots

# 5. Part 2 vary all GCAM mapping file plot ---------------------------------------------------------------


GCAM_mapping %>%
  inner_join(temp_data_all %>% 
               select(run_name, sub_dir, year, value), 
             by = 'run_name') -> 
  temp_mapping
  
temp_mapping$filter_name <- factor(temp_mapping$filter_name, hector_filter_order, ordered = TRUE) 

temp_mapping %>%  
  filter(year == 2100) %>%
  ggplot(data = .) + 
  geom_line(aes(filter_name, value, color = sub_dir), 
            position = position_dodge(width = 0.5),
            size = 2) + 
  labs(title = '2100 temp range based on the varying 4 params GCAM input selection')

# 6. Plot Temperature filtered by the varying 4 Dn results ---------------------------------------------

# Add the filter information to the temperature data.
temp_data %>%  
  full_join(filter_flag %>% 
              filter(sub_dir == 'vary_4_params') %>% 
              select(-sub_dir),
            by = c('run_name')) %>%  
  gather(filter, passing, `atm CO2`, `NPP`, `Tgav`, `None`, `atm CO2, NPP`, `atm CO2, Tgav`, `atm CO2, NPP, Tgav`) %>% 
  filter(passing) %>% 
  select(-passing) ->
  temp_data_all


temp_data_all$filter <- factor(temp_data_all$filter, hector_filter_order, ordered = TRUE)

temp_data_all %>% 
  filter(year == 2100) %>% 
  ggplot(data = .) + 
  geom_jitter(aes(sub_dir, value, color = sub_dir)) + 
  facet_wrap('filter') +
  labs(y = '2100 temp', 
       caption = 'categorized by the vary 4 parameters filters')
