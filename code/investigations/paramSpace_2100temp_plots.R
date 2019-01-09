
# Purpose: This script is intended to be an invesitgation into the Hector runs that pass the observation filters based 
# on the Dn metric analysis. 


# 0. Set Up ---------------------------------------------------------------------------------------------
# Make sure that the working directory is equal to the project directory
if( ! 'hector-SA-npar.Rproj' %in% list.files() ){ stop( 'Working dir must be the project location' ) }

# Load libs
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Load the functions 
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))

# Directories
BASE <- getwd() # The working directory should be the project location

# Output 
script_ouput = list()

# 1. Import Data ---------------------------------------------------------------------------------------

# Import the Dn results
readr::read_csv(list.files(file.path(BASE,'out-1', sub_dir), Dn_file, full.names = T)) %>%  
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  select(run_name, variable, passing) %>% 
  spread(variable, passing) -> 
  wide_passing_Dn

# Import the Hector temperature
Hector_tgav  <- read.csv(list.files(file.path(BASE,'out-1', sub_dir), "C.Tgav_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE)  
  
# Import the paramter combination data frame
Hector_param <- read.csv( file.path(BASE, 'out-1', sub_dir, 'A.par4_combinations.csv'), stringsAsFactors = FALSE ) %>%  
  mutate(run_name = paste0('hectorSA-', sprintf( '%04d', 1 : nrow( . ) )) )


# 2. Mapping Tibble ---------------------------------------------------------------------------------------

# Create a tibble that can be use to map the run_name to filter_name and filter count.
categorize_runs(wide_passing_Dn, c('atm CO2', 'Land Flux', 'NPP', 'Tgav')) %>% 
  select(run_name, filter_name) %>% 
  left_join(run_count(wide_passing_Dn, c('atm CO2', 'Land Flux', 'NPP', 'Tgav')), by = 'filter_name') -> 
  mapping_tibble


# 3. Create Plots ----------------------------------------------------------------------------------------

# 3 A All filters, 2100 tgav -----------------------------------------------------------------------------
# Subset the Hector tgav data for the 2100 values and then join with the mapping tibble.
Hector_tgav %>%  
  filter(year == 2100) %>%  
  left_join(mapping_tibble, by = 'run_name') -> 
  Hector_2100_values

# In order to make the plot a little more coherent oroder the filters by the magnitude of the difference. 
Hector_2100_values %>% 
  select(value, filter_name, count) %>%  
  group_by(filter_name) %>% 
  summarise(range_value = abs(max(value) - min(value)), 
            max = max(value),
            count = max(count)) %>% 
  ungroup %>% 
  arrange(desc(range_value)) ->
  Hector_2100_range

# Use the range infromation to order the values to plot
Hector_2100_values$filter_name <- factor(Hector_2100_values$filter_name, Hector_2100_range$filter_name, ordered = T )

# Make a jitter dodge plot of the 2100 year values.
ggplot() + 
  geom_jitter(data = Hector_2100_values, aes(filter_name, value, color = filter_name)) + 
geom_text(data = Hector_2100_range, aes(x = filter_name, y = max, label = count, color = filter_name),
          size = 5, vjust=-.6) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=14)) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav ') -> 
  script_ouput$all$temp_2100

  
# 3 B Only Single Filters ------------------------------------------------------------------------------

Hector_2100_values %>%  
  filter(! grepl(',', filter_name)) -> 
  single_filters


# Make a jitter dodge plot of the 2100 year values.
ggplot() + 
  geom_jitter(data = single_filters, aes(filter_name, value, color = filter_name)) + 
  geom_text(data = filter(Hector_2100_range, filter_name %in% single_filters$filter_name), aes(x = filter_name, y = max, label = count, color = filter_name),
            size = 5, vjust=-.3, col = 'black') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=14)) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav', 
       subtitle = 'Single observational filters only') -> 
  script_ouput$single$temp_2100


# Parameter jitter plots for the tin
Hector_param %>% 
  full_join(single_filters, by = 'run_name') %>% 
  gather(parameter, param_value, beta, q10, s, diff) %>% 
 
  split(.$parameter) %>% 
  map(function(input, subtitle = 'parameter space of the single filters'){
    
    param_name <- unique(input$parameter)
    
    input %>% 
      group_by(filter_name) %>% 
      summarise(max = max(param_value), count = max(count)) -> 
      count_tibble
    
    input %>% 
      ggplot(aes(filter_name, param_value, color = filter_name)) + 
      geom_jitter() + 
      geom_text(data = count_tibble, aes(filter_name, max, label = count), 
                col = 'black', size = 5,  vjust=-.3) +
      labs(x = NULL, 
           y = 'parameter value', 
           title = param_name, 
           subtitle = subtitle) + 
      theme(legend.position = 'none') + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.title = element_blank(), 
            legend.position = 'none', 
            text = element_text(size=14))
      

  }) -> 
  script_ouput$single$param_space
  



# 3 C Containing Co2 ------------------------------------------------------------------------------

Hector_2100_values %>%  
  filter(grepl('atm', filter_name) | grepl('None', filter_name)) -> 
  atm_filters


# Make a jitter dodge plot of the 2100 year values.
ggplot() + 
  geom_jitter(data = atm_filters, aes(filter_name, value, color = filter_name)) + 
  geom_text(data = filter(Hector_2100_range, filter_name %in% atm_filters$filter_name), aes(x = filter_name, y = max, label = count, color = filter_name),
            size = 5, vjust=-.3, col = 'black') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=14)) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav', 
       subtitle = 'Atm CO2 & other observational filters only') -> 
  script_ouput$co2$temp_2100


# Parameter jitter plots for the tin
Hector_param %>% 
  full_join(atm_filters, by = 'run_name') %>% 
  gather(parameter, param_value, beta, q10, s, diff) %>% 
  
  split(.$parameter) %>% 
  map(function(input, subtitle = 'paramter space for filers that use atm CO2'){
    
    param_name <- unique(input$parameter)
    
    input %>% 
      group_by(filter_name) %>% 
      summarise(max = max(param_value), count = max(count)) -> 
      count_tibble
    
    input %>% 
      ggplot(aes(filter_name, param_value, color = filter_name)) + 
      geom_jitter() + 
      geom_text(data = count_tibble, aes(filter_name, max, label = count), 
                col = 'black', size = 5, vjust=-.3) +
      labs(x = NULL, 
           y = 'parameter value', 
           title = param_name, 
           subtitle = subtitle) + 
      theme(legend.position = 'none') + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.title = element_blank(), 
            legend.position = 'none', 
            text = element_text(size=14))
    
    
  }) -> 
  script_ouput$co2$param_space


 
# 3 D Block 3 Filters -----------------------------------------------------------------------------------

Hector_2100_values %>%  
  filter(filter_name %in% c('None',
                            'Land Flux, Tgav', 
                            'Tgav', 
                            'NPP, Tgav', 
                            'atm CO2, NPP, Tgav', 
                            'atm CO2, Tgav', 
                            'Land Flux, NPP, Tgav')) -> 
  block_3_filters
  
# Make a jitter dodge plot of the 2100 year values.
ggplot() + 
  geom_jitter(data = block_3_filters, aes(filter_name, value, color = filter_name)) + 
  geom_text(data = filter(Hector_2100_range, filter_name %in% block_3_filters$filter_name), aes(x = filter_name, y = max, label = count, color = filter_name),
            size = 5, vjust=-.3, col = 'black') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=14)) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav', 
       subtitle = 'Filters with similarly constrained 2100 Tgav') -> 
  script_ouput$subset$temp_2100


# Parameter jitter plots for the tin
Hector_param %>% 
  full_join(block_3_filters, by = 'run_name') %>% 
  gather(parameter, param_value, beta, q10, s, diff) %>% 
  
  split(.$parameter) %>% 
  map(function(input, subtitle = 'parameter space for the similarly constrained 2100 Tgav'){
    
    param_name <- unique(input$parameter)
    
    input %>% 
      group_by(filter_name) %>% 
      summarise(max = max(param_value), count = max(count)) -> 
      count_tibble
    
    input %>% 
      ggplot(aes(filter_name, param_value, color = filter_name)) + 
      geom_jitter() + 
      geom_text(data = count_tibble, aes(filter_name, max, label = count), 
                col = 'black', size = 5, vjust=-.3) +
      labs(x = NULL, 
           y = 'parameter value', 
           title = param_name, 
           subtitle = subtitle) + 
      theme(legend.position = 'none') + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.title = element_blank(), 
            legend.position = 'none', 
            text = element_text(size=14))
    
    
  }) -> 
  script_ouput$subset$param_space


# 3 E Carbon product comparison ---------------------------------------------------------------------

Hector_2100_values %>%  
  filter(!grepl('Tgav', filter_name)) -> 
 carbon_products

# Make a jitter dodge plot of the 2100 year values.
ggplot() + 
  geom_jitter(data = carbon_products, aes(filter_name, value, color = filter_name)) + 
  geom_text(data = filter(Hector_2100_range, filter_name %in% carbon_products$filter_name), aes(x = filter_name, y = max, label = count, color = filter_name),
            size = 5, vjust=-.3, col = 'black') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=14)) + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav', 
       subtitle = 'Carbon produtcs') -> 
  script_ouput$carbon_prod$temp_2100


# Parameter jitter plots for the tin
Hector_param %>% 
  full_join(carbon_products, by = 'run_name') %>% 
  gather(parameter, param_value, beta, q10, s, diff) %>% 
  
  split(.$parameter) %>% 
  map(function(input, subtitle = 'carbon products'){
    
    param_name <- unique(input$parameter)
    
    input %>% 
      group_by(filter_name) %>% 
      summarise(max = max(param_value), count = max(count)) -> 
      count_tibble
    
    input %>% 
      ggplot(aes(filter_name, param_value, color = filter_name)) + 
      geom_jitter() + 
      geom_text(data = count_tibble, aes(filter_name, max, label = count), 
                col = 'black', size = 5, vjust=-.3) +
      labs(x = NULL, 
           y = 'parameter value', 
           title = param_name, 
           subtitle = subtitle) + 
      theme(legend.position = 'none') + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.title = element_blank(), 
            legend.position = 'none', 
            text = element_text(size=14))
    
    
  }) -> 
  script_ouput$carbon_prod$param_space


# 4 Save --------------------------------------------------------------------------

save(script_ouput, file = file.path(BASE, 'out-fig', 'paramSpace_2100temp.rda'))
