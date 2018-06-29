
# Purpose: This script is intended to be an invesitgation into the Hector runs that pass the observation filters based 
# on the Dn metric analysis. 

# TODO This script is not perfect yet, there is still a fair amount of clean up to do... 


# 0. Set Up ---------------------------------------------------------------------------------------------
# Make sure that the working directory is equal to the project directory
if( ! 'hector-SA-npar.Rproj' %in% list.files() ){ stop( 'Working dir must be the project location' ) }

# Load libs
library(dplyr);   library(tidyr)
library(ggplot2); library(purrr)
library(knitr);   library(scatterplot3d)
library(rgcam);   library(ggExtra)

# Directories
BASE    <- getwd() # The working directory should be the project location
sub_dir <- 'rcp26'

# Load the functions 
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))

# Specify the Dn method 
Dn_file <- 'E.all_Dmetric_independent_results.csv'

# Output 
script_output = list()


# 1. Import Data ---------------------------------------------------------------------------------------

# Import the Dn results
readr::read_csv(list.files(file.path(BASE,'int-out', sub_dir), Dn_file, full.names = T)) %>%  
  mutate(passing = if_else(Dn <= Dc, T, F)) %>% 
  select(run_name, variable, passing) %>% 
  spread(variable, passing) -> 
  wide_passing_Dn

# Import the Hector temperature
Hector_tgav  <- read.csv(list.files(file.path(BASE,'int-out', sub_dir), "C.Tgav_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE)  

# Import the Dn input tables 
Dn_Tgav_input <- read.csv( file.path( BASE, 'int-out', sub_dir, 'D.Tgav_Dmetric_input_table.csv' )) 
Dn_CO2_input  <- read.csv( file.path( BASE, 'int-out', sub_dir, 'D.atmCO2_Dmetric_input_table.csv' )) 
Dn_NPP_input  <- read.csv( file.path( BASE, 'int-out', sub_dir, 'D.NPP_Dmetric_input_table.csv' )) 


# Import the paramter combination data frame
Hector_param <- read.csv( file.path(BASE, 'int-out', 'A.par4_combinations.csv'), stringsAsFactors = FALSE ) %>%  
  mutate(run_name = paste0('hectorSA-', sprintf( '%04d', 1 : nrow( . ) )) )

# Load the GCAM output and the GCAM run mapping file
path <- list.files(file.path(BASE, 'sub-out'), 'GCAM_output.proj', full.names = TRUE)
gcam_output <- get(load(path))


# 2. Mapping tibbles & other templates ---------------------------------------------------------------------------------------

# Create a tibble that can be use to map the run_name to filter_name and filter count.
categorize_runs(wide_passing_Dn, c('atm CO2', 'NPP', 'Tgav')) %>% 
  select(run_name, filter_name) %>% 
  left_join(run_count(wide_passing_Dn, c('atm CO2', 'NPP', 'Tgav')), by = 'filter_name') -> 
  mapping_tibble

# Create a consistent color palette, the easiest way to do this is via a hard coded vector. 
color_vector <- c('None' = 'grey', 'NPP' = "#E69F00", 'atm CO2' = "#56B4E9", 'atm CO2, NPP' =  "#009E73", 'Tgav' = "#F0E442", 
                  'NPP, Tgav' =  "#0072B2",  'atm CO2, NPP, Tgav' = "#D55E00", 'atm CO2, Tgav' = "#CC79A7") 

# Universal theme 
UNIVERSTAL_THEME <- theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=16)) 

#######################################
# 3. Create Standalone Hector Plots ----------------------------------------------------------------------------------------

filter_order <- c('None', 'atm CO2', 'NPP', 'Tgav', 'atm CO2, NPP', 'atm CO2, Tgav', 'NPP, Tgav', 'atm CO2, NPP, Tgav')


#######################################
# Figs 1 - 3 observations vs Hector -----------------------------------------------------------------------------

# Fig  1 Hector Temp vs Obs -------------------------------------------------------------------------------------

Dn_Tgav_input %>% 
  # Because we are joining by the matching file here we are restricting the runs 
  # to only the values that passed the temperature filtering process.
  mutate( filter_name = 'Tgav' ) %>%  
  inner_join( mapping_tibble, by = c('run_name', 'filter_name')) -> 
  Tgav_Dn_input_matching

# Parse out a sinlge copy of the observational data from the Tgav_Dn_input tibble. 
Dn_Tgav_input %>% 
  select(year, obs) %>% 
  distinct -> 
  Tgav_obs

# Create a plot of unmathcing Dn, matching Dn, vs observations
ggplot() + 
  geom_line(data = Dn_Tgav_input, aes(year, model, color = 'unmatching Dn', group = run_name)) + 
  geom_line(data = Tgav_Dn_input_matching, aes(year, model, group = run_name, color = 'matching Dn')) + 
  geom_line(data = Tgav_obs, aes(year, obs, color = 'obs'), size = 1.5) + 
  UNIVERSTAL_THEME + 
  labs(x = 'year', 
       y = 'global temp anomoly', 
       title = 'Hector output vs obs') + 
  scale_color_manual(values = c('blue', 'orange', 'grey')) -> 
  script_output$comparison$temp

# Fig  2 Hector atcm CO2 vs Obs ---------------------------------------------------------------------------------

Dn_CO2_input %>% 
  # Because we are joining by the matching file here we are restricting the runs 
  # to only the values that passed the atm CO2 filtering process.
  mutate( filter_name = 'atm CO2' ) %>%  
  inner_join( mapping_tibble, by = c('run_name', 'filter_name')) -> 
  atmCO2_Dn_input_matching

# Parse out a single copy of the observational record 
Dn_CO2_input %>% 
  select(year, obs) %>% 
  distinct -> 
  atmCO2_obs

# Create a plot of unmathcing Dn, matching Dn, vs observations
ggplot() + 
  geom_line(data = Dn_CO2_input, aes(year, model, color = 'unmatching Dn', group = run_name)) + 
  geom_line(data = atmCO2_Dn_input_matching, aes(year, model, group = run_name, color = 'matching Dn')) + 
  geom_line(data = atmCO2_obs, aes(year, obs, color = 'obs'), size = 1.5) + 
  UNIVERSTAL_THEME + 
  labs(x = 'year', 
       y = 'atm CO2', 
       title = 'Hector output vs obs') + 
  scale_color_manual(values = c('blue', 'orange', 'grey')) -> 
  script_output$comparison$CO2

# Fig  3 Hector NPP vs Obs ---------------------------------------------------------------------------------

Dn_NPP_input %>% 
  # Because we are joining by the matching file here we are restricting the runs 
  # to only the values that passed the NPP filtering process.
  mutate( filter_name = 'NPP' ) %>%  
  inner_join( mapping_tibble, by = c('run_name', 'filter_name')) -> 
  NPP_Dn_input_matching

# Parse out a single copy of the NPP observational record.
Dn_NPP_input %>% 
  select(year, obs) %>%  
  distinct -> 
  NPP_obs

# Create a plot of unmathcing Dn, matching Dn, vs observations
ggplot() + 
  geom_line(data = Dn_NPP_input, aes(year, model, color = 'unfiltered runs', group = run_name)) + 
  geom_line(data = NPP_Dn_input_matching, aes(year, model, group = run_name, color = 'matching Dn')) + 
  geom_line(data = NPP_obs, aes(year, obs, color = 'obs'), size = 1.5) + 
  UNIVERSTAL_THEME + 
  labs(x = 'year', 
       y = 'NPP', 
       title = 'Hector output vs obs') + 
  scale_color_manual(values = c('blue', 'orange', 'grey')) -> 
  script_output$comparison$NPP


# Fig  4 Passing Run Counts ---------------------------------------------------------------------------------

# Creat a tibble of the number of runs that pass through each observational filter.
mapping_tibble %>%  
  select(filter_name, count) %>%  
  distinct -> 
  run_count_df

# Add factor levels to the run count data frame
run_count_df$filter_name <- factor(run_count_df$filter_name, names(color_vector), ordered = TRUE)


# Create a bar chart of the number of runs that pass through each filter name
ggplot(run_count_df) + 
  geom_col(aes(filter_name, count, fill = filter_name)) + 
  geom_text(aes(x = filter_name, y = count, label = count), size = 5, vjust = -.6, color = 'black') + 
  # Aesthetics
  scale_fill_manual(values = color_vector) +
  UNIVERSTAL_THEME + 
  labs(x = NULL, 
       y = 'passing Hector runs (Dn <= Dc)', 
       title = 'Passing Runs Count') -> 
  script_output$run_count


# Fig  5 All filters, 2100 tgav -----------------------------------------------------------------------------

# Ths code in this section greates the layered jitter + box plot of the 2100 Tgav values. Before plotting 
# the results the temperature data has to be processed. We are going to want to arrange the plot in order 
# of 2100 spread, this will also take a bit of finageling.

# Subset the Hector tgav data for the 2100 values and then join with the mapping tibble.
Hector_tgav %>%  
  filter(year == 2100) %>%  
  left_join(mapping_tibble, by = 'run_name') -> 
  Hector_2100_values

# Determine the range for the 2100 temperature filters.
Hector_2100_values %>% 
  select(value, filter_name, count) %>%  
  group_by(filter_name) %>% 
  summarise(range_value = abs(max(value) - min(value)), 
            max = max(value),
            count = max(count)) %>% 
  ungroup %>% 
  arrange(desc(range_value)) ->
  Hector_2100_range

# Use the 2100 temp range to order the filters.
Hector_2100_values$filter_name <- factor(Hector_2100_values$filter_name, Hector_2100_range$filter_name, ordered = T )
mapping_tibble$filter_name     <- factor(mapping_tibble$filter_name, Hector_2100_range$filter_name, ordered = T )

# Make a jitter dodge plot of the 2100 year values.
ggplot() + 
  geom_jitter(data = Hector_2100_values, aes(filter_name, value, color = filter_name)) + 
  geom_boxplot(data = Hector_2100_values, aes(filter_name, value), outlier.shape = NA, fill = NA, size = 1) + 
  geom_text(data = Hector_2100_range, aes(x = filter_name, y = max, label = count),
            size = 5, vjust=-.6, color = 'black') + 
  scale_color_manual(values = color_vector) +
  UNIVERSTAL_THEME + 
  labs(x = NULL, 
       y = '2100 Tgav deg C', 
       title = '2100 Tgav ') -> 
  script_output$all$temp_2100


# Summary table for figure 4 

# Start by parsing out the data from the jitter plot layer
script_output$all$temp_2100$layers[[1]]$data %>% 
  group_by(filter_name) %>% 
  summarise(min = min(value), max = max(value), count = unique(count)) %>%  
  ungroup %>% 
  mutate(range = abs(max - min)) %>% 
  select(filter_name, min, max, range, count) %>%  
  kable -> 
  script_output$all$temp_2100_table
  


# Figs 6 - 10 Parmater Spaces -----------------------------------------------------------------------------

# Make layered jitter + box plots to look at the parameter space selected by each observational filtering 
# process.

# Start by adding the parameter information to the mapping tibble so that the data frame contians 
# the paramter values for each run categorized by the observational filters.
paramter_filters_tibble <- full_join(Hector_param, mapping_tibble, by = 'run_name') 

# Format and split the data frames up by the paramter and in prepartion to map the layered 
# jitter + boxplot function to every data frame.
paramter_filters_tibble %>%   
  gather(parameter, param_value, beta, q10, s, diff) %>% 
  split(.$parameter) %>% 
  # Map a the layered geom_jitter + geom_boxplot function to parameter data frames.
  map(function(input, subtitle = 'parameter space of the single filters'){
    
    # Save a copy of the paramter name 
    param_name <- unique(input$parameter) 
    
    # Save the run count for each filter category.
    input %>% 
      group_by(filter_name) %>% 
      summarise(max = max(param_value), count = max(count)) -> 
      count_tibble
    
    # Plot
    input %>% 
      ggplot(aes(filter_name, param_value, color = filter_name)) + 
      geom_jitter() + 
      geom_text(data = count_tibble, aes(filter_name, max, label = count), col = 'black', size = 5,  vjust=-.3) +
      geom_boxplot(data = input, aes(filter_name, param_value), color = 'black', outlier.shape = NA, fill = NA, size = 1) + 
      labs(x = NULL, 
           y = 'parameter value', 
           title = param_name, 
           subtitle = subtitle) + 
      theme_bw() + 
      theme(legend.position = 'none') + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.title = element_blank(), 
            legend.position = 'none', 
            text = element_text(size=16)) + 
      scale_color_manual(values = color_vector)
    
  }) -> 
  script_output$param


# Follow up plots 
# Since we know that q10 and s are complete spaned what does the s vs beta parmeter space look like? 

ggplot(paramter_filters_tibble) + 
  geom_point(aes(beta, s, color = filter_name), alpha = 0.9) + 
  stat_ellipse(data = (filter(paramter_filters_tibble, filter_name != 'None')), aes(beta, s, fill = filter_name), geom = "polygon",
               type = "norm", alpha = 0.3) + 
  # Aesthics
  UNIVERSTAL_THEME + 
  theme(legend.position = 'right') +
  coord_cartesian(xlim = c(0, 1), ylim = c(1,7)) + 
  scale_color_manual(values = color_vector) + 
  scale_fill_manual(values = color_vector[ names(color_vector) != 'None']) + 
  labs(title = 'beta vs s parameter space') -> 
  script_output$param$'beta vs s'
  

# Fig  11 Passing Tgav Plot -------------------------------------------------------------------------------
# We want to plot the full temperature time series color coordinated by observationa filter. Since there is 
# over lap in some of the runs we are going to need to make some sort of markers for the 2100 values.

# Start by adding the filter name to the Hector Tgav results.
Hector_tgav %>%  
  full_join(mapping_tibble, 'run_name') -> 
  Tgav_filters

# This vector will be used to plot the results in a specific order, ORDER MATTERS!
# So does content 
filters_to_plot <- c('None', 'atm CO2', 'atm CO2, NPP, Tgav') 


# Make a tibble of the end markers. Make sure that the end_markers tibble is ranked by filter name.
Hector_2100_values %>% 
  filter(filter_name %in% filters_to_plot) %>% 
  group_by(filter_name) %>% 
  summarise(min = min(value), max = max(value)) %>% 
  ungroup %>% 
  mutate(year = seq(from = 2105, by = 5, length = nrow(.))) %>% 
  gather(extreme, value, min, max) %>% 
  arrange(filter_name) -> 
  end_markers

end_markers$filter_name <- factor(end_markers$filter_name, names(color_vector), ordered = TRUE)
  

# Figure Code
my_plot <- ggplot()  # The empty plot                                           

# Add the data in filters_to_plot order
for( filter in 1:length(filters_to_plot) ){
  
  # Subset the data by filter in order to 
  filtered_Tgav_data <- filter(Tgav_filters, filter_name == filters_to_plot[[ filter ]])
  filtered_end_pts   <- filter(end_markers, filter_name == filters_to_plot[[ filter ]])
  
  my_plot + 
    geom_line(data = filtered_Tgav_data, aes(year, value, color = filter_name, group = run_name))  + 
    geom_point(data = filtered_end_pts, aes(year, value, color = filter_name), size = 1.5) -> 
    my_plot
  
}

# Alter the asetics of the plot 
my_plot +
  UNIVERSTAL_THEME +
  theme(legend.position = 'bottom') +
  labs(y = 'deg C') +
  scale_color_manual(values = color_vector[ names(color_vector) %in% filters_to_plot]) -> 
  script_output$'temp by filter'

########################################
# 4. Create GCAM Plots -----------------
########################################
# 4 A Format GCAM Output ---------------------------------------------------------------------------------------

# Most of the GCAM output prcossesing should have occured druing the part 2 level C script but somtimes we 
# are interested in aggregrate or specfic regions / sectors. 

# Copy the gcam output as to plot 
to_plot <- gcam_output

# Calculate the Global CO2 emissions
gcam_output$`CO2 emissions by region` %>% 
  group_by(run_name, year, filter_name, units, policy) %>%
  summarise(value = sum(value)) %>%
  ungroup ->
  to_plot$'Global CO2 emissions'


# Calculate the total primary energy consumption in the US. 
gcam_output$'Energy consumption by sector'%>% 
  filter(region == "USA") %>% 
  group_by(run_name, year,filter_name, units, policy) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  to_plot$'Total energy consumption in USA'


# 4 B GCAM Plots ------------------------------------------------------------------------------------------------

# Make a list of the queries and observational filters to plot
plot_queries        <- list('CO2 concentrations', 'Global mean temperature', 'Climate forcing', 'Total energy consumption in USA', 'Global CO2 emissions') 
names(plot_queries) <- plot_queries

plot_filters <- c('None', 'atm CO2', 'atm CO2, NPP, Tgav')

# Plot the reference runs only
# Map the ploting code to all of the query tibbles.
map(plot_queries, function(query_name, 
                           filters = plot_filters,
                           policy_name = 'nop', 
                           data = to_plot, 
                           subtitle = 'reference runs'){
  
  # Subset the data and arrange so that the color order is correct
  data[[query_name]] %>% 
    filter(filter_name %in% filters) %>% 
    filter(year >= 2015 & policy == policy_name) -> 
    to_plot
    
  # Create a tibble of of 2100 manx and mins to plot past year 2100 to represent 
  # the spread.
  to_plot %>% 
    filter(year == 2100) %>% 
    filter(filter_name %in% to_plot$filter_name) %>%
    select(-year) %>% 
    bind_cols(year = seq(from = 2101.5, by = 1, length.out = nrow(.))) ->
    points_tibble

  # # Parse information variable and unit infromation to use to lable the plots
  units  <- unique(to_plot$units)
  q_name <- query_name

  # Create the line plot
  ggplot(to_plot) +
    geom_line(aes(year, value, color = filter_name, group = run_name), size = 1.5) +
    geom_point(data = points_tibble, aes(year, value, color = filter_name), size = 2) +
    scale_color_manual(values = color_vector[ names(color_vector) %in% unique(to_plot$filter_name) ]) +
    UNIVERSTAL_THEME +
    theme(legend.position = 'bottom') +
    labs(title = query_name,
         subtitle = subtitle,
         y = units)
      
  }) -> 
  script_output$gcam$reference_only


# Plot the target runs only 
# Map the polting code to all fo the query tibbles. 
map(plot_queries, function(query_name, 
                           filters = 'Tgav',
                           policy_name = '2deg', 
                           data = to_plot, 
                           subtitle = 'policy runs'){
  
  # Subset the data and arrange so that the color order is correct
  data[[query_name]] %>% 
    filter(filter_name %in% filters) %>% 
    filter(year >= 2015 & policy == policy_name) -> 
    to_plot
  
  # Create a tibble of of 2100 manx and mins to plot past year 2100 to represent 
  # the spread.
  to_plot %>% 
    filter(year == 2100) %>% 
    filter(filter_name %in% to_plot$filter_name) %>%
    select(-year) %>% 
    bind_cols(year = seq(from = 2101.5, by = 1, length.out = nrow(.))) ->
    points_tibble
  
  # # Parse information variable and unit infromation to use to lable the plots
  units  <- unique(to_plot$units)
  q_name <- query_name
  
  # Create the line plot
  ggplot(to_plot) +
    geom_line(aes(year, value, color = filter_name, group = run_name), size = 1.5) +
    geom_point(data = points_tibble, aes(year, value, color = filter_name), size = 2) +
    scale_color_manual(values = color_vector[ names(color_vector) %in% unique(to_plot$filter_name) ]) +
    UNIVERSTAL_THEME +
    theme(legend.position = 'bottom') +
    labs(title = query_name,
         subtitle = subtitle,
         y = units)
  
}) -> 
  script_output$gcam$policy_only



# Plot the difference between the policy and the refernce runs 
# Plot the policy runs only 
# Map the polting code to all fo the query tibbles. 
map(plot_queries, function(query_name, 
                           filters = 'Tgav',
                           policy_name = '2deg', 
                           data = to_plot, 
                           subtitle = 'target runs'){
  
  # Subset the data and arrange so that the color order is correct
  data[[query_name]] %>% 
    filter(filter_name %in% filters) %>% 
    filter(year >= 2010 & policy == policy_name) -> 
    to_plot
  
  # Create a tibble of of 2100 manx and mins to plot past year 2100 to represent 
  # the spread.
  to_plot %>% 
    filter(year == 2100) %>% 
    filter(filter_name %in% to_plot$filter_name) %>%
    select(-year) %>% 
    bind_cols(year = seq(from = 2101.5, by = 1, length.out = nrow(.))) ->
    points_tibble
  
  # # Parse information variable and unit infromation to use to lable the plots
  units  <- unique(to_plot$units)
  q_name <- query_name
  
  # Create the line plot
  ggplot(to_plot) +
    geom_line(aes(year, value, color = filter_name, group = run_name), size = 1.5) +
    geom_point(data = points_tibble, aes(year, value, color = filter_name), size = 2) +
    scale_color_manual(values = color_vector[ names(color_vector) %in% unique(to_plot$filter_name) ]) +
    UNIVERSTAL_THEME +
    theme(legend.position = 'bottom') +
    labs(title = query_name,
         subtitle = subtitle,
         y = units)
  
}) -> 
  script_output$gcam$target_only


# Plot the policy results
map(plot_queries, function(query_name, 
                           filters = 'Tgav',
                           policy_name = 'policy', 
                           data = to_plot, 
                           subtitle = 'policy runs (target - reference)'){
    
    # Subset the data and arrange so that the color order is correct
    data[[query_name]] %>% 
      filter(filter_name %in% filters) %>% 
      filter(year >= 2015 & policy == policy_name) -> 
      to_plot
    
  if(nrow(to_plot) > 1){
    
    # Subset the data and arrange so that the color order is correct
    data[[query_name]] %>% 
      filter(filter_name %in% filters) %>% 
      filter(year >= 2015 & policy == policy_name) -> 
      to_plot
    
    # Create a tibble of of 2100 manx and mins to plot past year 2100 to represent 
    # the spread.
    to_plot %>% 
      filter(year == 2100) %>% 
      filter(filter_name %in% to_plot$filter_name) %>%
      select(-year) %>% 
      bind_cols(year = seq(from = 2101.5, by = 1, length.out = nrow(.))) ->
      points_tibble
    
    # # Parse information variable and unit infromation to use to lable the plots
    units  <- unique(to_plot$units)
    q_name <- query_name
    
    # Create the line plot
    ggplot(to_plot) +
      geom_line(aes(year, value, color = filter_name, group = run_name), size = 1.5) +
      geom_point(data = points_tibble, aes(year, value, color = filter_name), size = 2) +
      scale_color_manual(values = color_vector[ names(color_vector) %in% unique(to_plot$filter_name) ]) +
      UNIVERSTAL_THEME +
      theme(legend.position = 'bottom') +
      labs(title = query_name,
           subtitle = subtitle,
           y = units)
    
  } else {
    
    # If there are no policy results to plot return a NULL plot
    NULL
    
  }

    
    
  }) -> 
  script_output$gcam$policy


# 4 Save --------------------------------------------------------------------------

save(script_output, file = file.path(BASE, 'diag-out', 'CMS_paper_plots.rda'))
