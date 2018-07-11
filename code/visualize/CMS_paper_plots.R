
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
BASE    <- getwd() 
sub_dir <- 'rcp26' # The name of the int-out/sub_dir to pull the standalone Hector results from

# Load the functions 
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))

# Specify the Dn method and the GCAM results to import 
Dn_file   <- 'E.all_Dmetric_independent_results.csv'
GCAM_list <- list('GCAM_min.rda', 'GCAM_max.rda', 'GCAM_median.rda') 

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
Hector_atmC  <- read.csv(list.files(file.path(BASE,'int-out', sub_dir), "C.Ca_hector_run_cleanup.csv", full.names = T), stringsAsFactors = FALSE)  


# Import the Dn input tables 
Dn_Tgav_input <- read.csv( file.path( BASE, 'int-out', sub_dir, 'D.Tgav_Dmetric_input_table.csv' )) 
Dn_CO2_input  <- read.csv( file.path( BASE, 'int-out', sub_dir, 'D.atmCO2_Dmetric_input_table.csv' )) 
Dn_NPP_input  <- read.csv( file.path( BASE, 'int-out', sub_dir, 'D.NPP_Dmetric_input_table.csv' )) 


# Import the paramter combination data frame
# TODO will need to remove the run_name mutate call. 
Hector_param <- read.csv( file.path(BASE, 'int-out', 'A.par4_combinations.csv'), stringsAsFactors = FALSE ) %>%  
  mutate(run_name = paste0('hectorSA-', sprintf( '%04d', 1 : nrow( . ) )) )


# Import the GCAM data and format as a flat list and then concatenate based on tibble name.
flatten_list <- flatten( map(GCAM_list, function(name){ get(load( file.path(BASE, 'sub-out', name) )) }) )

tibble_names        <- unique(names(flatten_list))
names(tibble_names) <- tibble_names

gcam_rslt <- map(tibble_names, function(name){  bind_rows( flatten_list[names(flatten_list) == name] ) })


# 2. Mapping Information ------------------------------------------------------------------------------------------------------

# Create a tibble that can be use to map the run_name to filter_name and filter count.
categorize_runs(wide_passing_Dn, c('atm CO2', 'NPP', 'Tgav')) %>% 
  select(run_name, filter_name) %>% 
  left_join(run_count(wide_passing_Dn, c('atm CO2', 'NPP', 'Tgav')), by = 'filter_name') -> 
  mapping_tibble

# Create a consistent color palette, the easiest way to do this is via a hard coded vector. 
color_vector <- c('None' = 'grey', 'NPP' = "#E69F00", 'atm CO2' = "#56B4E9", 'atm CO2, NPP' =  "#009E73", 
                  'Tgav' = "#F0E442", 'NPP, Tgav' =  "#0072B2", 'atm CO2, NPP, Tgav' = "#D55E00", 'atm CO2, Tgav' = "#CC79A7") 

# Define a universal theme 
theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.title = element_blank(), 
        legend.position = 'none', 
        text = element_text(size=16)) -> 
  UNIVERSTAL_THEME 


# Factor order for the standalone Hector plots 
hector_filter_order <- c('None', 'atm CO2', 'NPP', 'Tgav', 'atm CO2, NPP', 'atm CO2, Tgav', 'NPP, Tgav', 'atm CO2, NPP, Tgav')


############################################################################################################################
# 3. Create Standalone Hector Plots ----------------------------------------------------------------------------------------
############################################################################################################################
# Fig 1 Hector Temp vs Obs -------------------------------------------------------------------------------------

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

# Fig 2 Hector atcm CO2 vs Obs ---------------------------------------------------------------------------------

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

# Fig 3 Hector NPP vs Obs ---------------------------------------------------------------------------------

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


# Fig 4 Passing Run Counts ---------------------------------------------------------------------------------

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


# Fig 5 2100 values --------------------------------------------------------------------------------------------------------

# plot_2100_value function that plots the year 2100 jitter plots and add median black line. 
plot_2100_value <- function(input, title = NULL, subtitle = NULL, ylab = NULL){
  
  # Determine the range for the 2100 temperature filters.
  input %>% 
    select(value, filter_name, count) %>%  
    group_by(filter_name) %>% 
    summarise(range_value = abs(max(value) - min(value)), 
              max = max(value),
              count = max(count)) %>% 
    ungroup %>% 
    arrange(desc(range_value)) ->
    Hector_2100_range
  
  # Use the 2100 temp range to order the filters.
  input$filter_name <- factor(input$filter_name, Hector_2100_range$filter_name, ordered = T )
  mapping_tibble$filter_name <- factor(mapping_tibble$filter_name, Hector_2100_range$filter_name, ordered = T )
  
  
  # Make a jitter dodge plot of the 2100 year values.
  ggplot() + 
    geom_jitter(data = input, aes(filter_name, value, color = filter_name)) + 
    geom_boxplot(data = input, aes(filter_name, value), outlier.shape = NA, fill = NA, size = 1) + 
    geom_text(data = Hector_2100_range, aes(x = filter_name, y = max, label = count),
              size = 5, vjust=-.6, color = 'black') + 
    scale_color_manual(values = color_vector) +
    UNIVERSTAL_THEME + 
    labs(x = NULL, 
         y = ylab, 
         title = title, 
         subtitle = subtitle) -> 
    plot 
  
  # Pull out the boxplot layer
  dat <- ggplot_build(plot)$data[[2]]
  
  # Add the median line
  plot +
    geom_segment(data=dat, aes(x = xmin, xend = xmax, y=middle, yend=middle), colour="black", size=2) -> 
    plot 
  
  # Remove the median box plot
  plot$layers[[2]] <- NULL
  
  # Return plot
  plot
  
}

# Tgav  
# Subset the Hector tgav data for the 2100 values and then join with the mapping tibble.
Hector_tgav %>%  
  filter(year == 2100) %>%  
  left_join(mapping_tibble, by = 'run_name') -> 
  Hector_2100_values

script_output$'2100 value'$temp <- plot_2100_value(Hector_2100_values, ylab = 'deg C', title = 'Tgav 2100 values')

# atm CO2 
# Subset the Hector CO2 data for the 2100 values
Hector_atmC %>%  
  filter(year == 2100) %>%  
  left_join(mapping_tibble, by = 'run_name') -> 
  Hector_2100_atmC

script_output$'2100 value'$CO2 <- plot_2100_value(Hector_2100_atmC, ylab = 'ppmv CO2', title = 'atm CO2 2100 values')


# Fig 6 - 10 Parmater Spaces -----------------------------------------------------------------------------

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


# # Parameter space follow up plot 
# # Since we know that q10 and s are complete spaned what does the s vs beta parmeter space look like? 
# 
# ggplot(paramter_filters_tibble) + 
#   geom_point(aes(beta, s, color = filter_name), alpha = 0.9) + 
#   stat_ellipse(data = (filter(paramter_filters_tibble, filter_name != 'None')), aes(beta, s, fill = filter_name), geom = "polygon",
#                type = "norm", alpha = 0.3) + 
#   # Aesthics
#   UNIVERSTAL_THEME + 
#   theme(legend.position = 'right') +
#   coord_cartesian(xlim = c(0, 1), ylim = c(1,7)) + 
#   scale_color_manual(values = color_vector) + 
#   scale_fill_manual(values = color_vector[ names(color_vector) != 'None']) + 
#   labs(title = 'beta vs s parameter space') -> 
#   script_output$param$'beta vs s'
  

# Fig 11 Passing Tgav Plot -------------------------------------------------------------------------------------------------
# We want to plot the full temperature time series color coordinated by observational filter. Since there is 
# over lap in some of the runs we are going to need to make some sort of markers for the 2100 values.

# Start by adding the filter name to the Hector Tgav results.
Hector_tgav %>%  
  full_join(mapping_tibble, 'run_name') -> 
  Tgav_filters

# This vector will be used to plot the results in a specific order, ORDER MATTERS!
# So does content 
filters_to_plot <- c('None', 'atm CO2', 'atm CO2, NPP, Tgav') 


# Make a tibble of the end markers. Make sure that the end_markers tibble is ranked by filter name.
Hector_tgav %>%
  left_join(mapping_tibble, by = 'run_name') %>% 
  filter(year == 2100) %>% 
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
  script_output$'Tgav full time series colored by filter plot'

############################################################################################################################
# 4. Create GCAM Plots -----------------
############################################################################################################################
# A Process GCAM ouput -----------------------------------------------------------------------------------------

# Most of the GCAM output prcossesing should have occured druing  the part 2 of the level C script but we are intrested in 
# ploting some aggergated values of energy consumptionand emissions. Subset / aggegrate the GCAM data to create the 
# tibbles we want to visualize. 

# Calculate the Global CO2 emissions
gcam_rslt$`CO2 emissions by region` %>% 
  group_by(run_name, year, filter_name, units, policy, keep) %>%
  summarise(value = sum(value)) %>%
  ungroup ->
  gcam_rslt$'Global CO2 emissions'

# Calculate the total primary energy consumption in the US. 
gcam_rslt$'Energy consumption by sector'%>% 
  filter(region == "USA") %>% 
  group_by(run_name, year,filter_name, units, policy, keep) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  gcam_rslt$'Total energy consumption in USA'

# Calculate the `Primary Energy Consumption for bioenergy, fossile fuels, and renewables. 
# First create a fuel type mapping tibble that will be used to categorize the fuels, then 
# aggregate. 
tibble( fuel =  unique(gcam_rslt$`Primary Energy Consumption (Direct Equivalent)`$fuel)) %>% 
  mutate(fuel_class = if_else(grepl("[B|b]iomass", fuel), "bioenergy", "NA")) %>% 
  mutate(fuel_class = if_else(fuel %in% c("Coal", "Oil", "Natural Gas"), "fossil fuel", fuel_class)) %>% 
  mutate(fuel_class = if_else(fuel %in% c("Geothermal", "Hydro", "Nuclear", "Solar", "Wind"), "renewable", fuel_class)) %>% 
  filter_all(all_vars(. != "NA")) -> 
  fuel_mapping

gcam_rslt$`Primary Energy Consumption (Direct Equivalent)` %>% 
  filter(region == 'USA') %>% 
  full_join(fuel_mapping, by = 'fuel') %>% 
  group_by(run_name, policy, year, filter_name, fuel_class, keep) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  # Remove adata for any uncategorized fuels
  na.omit %>%  
  mutate(units = NA)  ->
  gcam_rslt$'USA categorized primary energy consumption'

# B GCAM Plots ------------------------------------------------------------------------------------------------

# plot_gcam_rslt is a function that creates a line/point plot of the GCAM results for at single query / policy 
# output. The results are colored by filter_name and points are based on keep (the reason the parameter set 
# was selecte to be used in GCAM).
plot_gcam_rslt <- function(input, query, filters, plot.policy, subtitle){
  
  # Parse out the tibble from the input list to plot, then 
  # subset the datat based on function arguments. 
  input[[query]] %>% 
    filter(filter_name %in% filters) %>% 
    filter(year >= 2015 , policy == plot.policy) -> 
    data
  
  if(length(unique(data$policy)) != 1) stop('Problem with the number of entries in the policy data frame')  
  
  if(nrow(data) < 1) { 
    
    NULL
    
    } else {
    
    # Save some information to use to label  the plots
    units  <- unique(data$units)
    
    # Plot
    ggplot(data) +
      geom_line(aes(year, value, color = filter_name, group = run_name), size = 1) +
      geom_point(aes(year, value, color = filter_name, group = run_name, shape = keep), size = 3) +
      scale_color_manual(values = color_vector[ names(color_vector) %in% unique(data$filter_name) ]) +
      UNIVERSTAL_THEME +
      theme(legend.position = 'bottom') +
      labs(title = query,
           subtitle = subtitle,
           y = units)  + 
      facet_wrap('filter_name', ncol = 3)
  }
  
}



# Information I would like to plot 
plot_queries        <- list('CO2 concentrations', 'Global mean temperature', 'Climate forcing','Total energy consumption in USA',
                      'Global CO2 emissions', 'CO2 prices', 'USA categorized primary energy consumption') 
names(plot_queries) <- plot_queries

plot_filters <- unique(gcam_rslt$`CO2 concentrations`$filter_name)


# Generate the basic GCAM plots for all of the different policy options. s
map( plot_queries, plot_gcam_rslt, input = gcam_rslt, filters = plot_filters, 
    plot.policy = 'nop', subtitle = 'reference run (no policy)' ) -> 
  script_output$gcam_reference 

map( plot_queries, plot_gcam_rslt, input = gcam_rslt, filters = plot_filters, 
     plot.policy = 'RF-2p6', subtitle = 'target' ) -> 
  script_output$gcam_target

map( plot_queries, plot_gcam_rslt, input = gcam_rslt, filters = plot_filters, 
     plot.policy = 'policy', subtitle = 'policy (target - ref)' ) -> 
  script_output$gcam_policy


# Modify  the primary energy consumption plots, because the categorized fuels are 
# all plotted together they will need to be modified. 
script_output$gcam_policy$`USA categorized primary energy consumption` +
  facet_grid(fuel_class ~ filter_name, scales = 'free') + 
  labs(y = NULL) -> 
  script_output$gcam_policy$`USA categorized primary energy consumption`

script_output$gcam_reference$`USA categorized primary energy consumption` +
  facet_grid(fuel_class ~ filter_name, scales = 'free') + 
  labs(y = NULL) -> 
  script_output$gcam_reference$`USA categorized primary energy consumption`

script_output$gcam_target$`USA categorized primary energy consumption` +
  facet_grid(fuel_class ~ filter_name, scales = 'free') + 
  labs(y = NULL) -> 
  script_output$gcam_target$`USA categorized primary energy consumption`

# 4 Save --------------------------------------------------------------------------

# TODO I will want some method that will allow me to pretty easily save the plots in rda format, I will also want the ability 
# to save the figures as png files. I will also probably want to enable th user with the ability to decide if they want to 
# save in which format or not save. 


#save(script_output, file = file.path(BASE, 'diag-out', 'CMS_paper_plots.rda'))
