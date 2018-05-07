# Purpose: Figures for https://github.com/JGCRI/hector-SA-npar/issues/7#event-1509615854 

# Note this script can take a while to run / plot. To decrease plotting speed only plot a small 


# Part 1 The Hector Results Figures: Plots the results of the stand alone hector runs.
# Part 2 Run and Paramter Figures: Plots looking at the parameter sets / runs that pass through the obs filtering.
# Part 3 The GCAM Results Figures: Plots looking at the GCAM results from the Hector params selected as inputs. 
                                # The current method picks extreem values 
# Part 4 Get the numbers for CH


# Set Up --------------------------------------------------------------------------------

# Load the libs
library(ggplot2); library(dplyr)
library(tidyr);   library(purrr)
library(caTools); library(rgcam)
library(gridExtra); 


# Directory and script set up 
BASE          <- getwd() # the base dir
rcpXX         <- 'rcp26' # the rcp sub directory to pull the data from  


# Visual constants to make sure that all figures have a constant 
# background and color scheme. 

FIGURE_THEME <- theme_bw() + theme(text = element_text(size=16)) 


# Use cbPalette in all of the figures to make sure that the color pallate is consistent. 
cbPalette    <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
 
# Create an empty list to store all the figures in and select how the outpus should be saved.
fig_list <- list()
save_rda <- TRUE
save_png <- FALSE


###############################################################################################################
# Part 1 the Hector Results Figures ---------------------------------------------------------------------------
# The plots in this section look at the results from the stand alone Hector runs. These plots can take a 
# while to plot and save because of the large numebr of runs.
## Import and Format Hector Data ------------------------------------------------------------------------------

# Import the cleaned up hector results, format into long format. Do not calcualte the moving 
# average now because it will take too long. 
hector_all <- readr::read_csv(file.path(BASE, 'int-out', rcpXX, "C.hector_run_cleanup.csv")) 

# If you are intested in visualizing the several times I would suggest subsetting the number 
# of hector runs you are working with in order to decrease the plotting time. 
# hector_all <- hector_all[300,]

# Format into long format
hector_all %>% 
  gather(year, value, -run_name, -variable, -units) %>%  
  mutate(year = as.integer(year)) ->
  hector 


# The filtering by obs window results.
flags <- readr::read_csv(file.path(BASE, 'int-out', rcpXX, 'filter_flag.csv')) 

# add_filter_info: is a function that formats the the falgs data frame into long format and 
# classifies what observational filters the hector run passes through. 
add_filter_info <- function(data, flags, fname){
  
  if(!is.null(flags)) {
    
    data %>% 
      filter_at(flags, all_vars(. == 1)) %>% 
      gather(parameter, param_value, beta, q10, s, diff) %>% 
      mutate(filter = fname, filter_flag = paste(flags, collapse = " & ")) 
    
  } else {
    
    data %>% 
      gather(parameter, param_value, beta, q10, s, diff) %>% 
      mutate(filter = fname, filter_flag = fname) 
    
  }
  
}

# Use the add filter info to classify the runs by filter we are interested in this analysis. 
bind_rows(add_filter_info(flags, NULL, "None"),
          add_filter_info(flags, "tempature_flag", "Temp"), 
          add_filter_info(flags, c("tempature_flag", "landflux_flag"), "Temp, Land Flux"), 
          add_filter_info(flags, c("tempature_flag", "growth_flag", "landflux_flag"), "Temp, Land Flux, Growth")) -> 
  filtered_flags



# Filter color codes 
filter_name   <- unique(filtered_flags$filter)
filter_colors <- tibble(filter = filter_name, 
                        color = c("grey", cbPalette[[2]], cbPalette[[3]], cbPalette[[4]]))



## Functions ----------------------------------------------------------------------------------------------

# remove_layer: is a function that removes a single layer from 
# a ggplot fiugre at a time
remove_layer <- function(complete, num){
  complete$layers[[num]] <- NULL
  complete
}

# power_point_annimpation_figs: is  a function that splits a 
# laywerd plot up into mulitple plots so that it can be used in power 
# point animations. 
power_point_animation_figs <- function(complete_figure){
  
  complete_figure  + 
    labs(title = NULL) + 
    theme(panel.grid = NULL) + 
    guides(color = FALSE ) -> 
    layers_4
  
  remove_layer(layers_4, 4) + 
    labs(title = NULL) + 
    theme(panel.grid = NULL) + 
    guides(color = FALSE ) -> 
    layers_3
  
  remove_layer(layers_3, 3) + 
    labs(title = NULL) + 
    theme(panel.grid = NULL) -> 
    layers_2
  
  remove_layer(layers_2, 2) + 
    guides(color = FALSE )  -> 
    layers_1
  
  list(layer_1 = layers_1, layer_2 = layers_2, layer_3 = layers_3, layer_4 = layers_4)
  
}

# The get_Hector_ma function gets the moving average for a specific Hector Variable  
get_Hector_ma <- function(data, variable, windowYrs = 15){  

  data %>%  
    filter(variable == variable) %>%  
    split(., .$run_name) %>%  
    map(function(x){ x$value <- caTools::runmean(x$value, windowYrs, 
                                        alg = c( 'C' ),   
                                        endrule = c( 'mean' ),   
                                        align = c( 'center')); x } ) %>%    
    bind_rows %>%   
    arrange(run_name, year)  
}



## Fig1.Hector_temp: all temp and temp vs obs ---------------------------------------------------------------------------------

# All 5000 Hector runs - mean and spread for temperature 

# Subset the Hector temperature and then calculate the moving average
hector_temp <- filter(hector, variable == "Tgav") 

# Find the mean and spread of Hector temp.
hector_temp %>% 
  group_by(year, variable, units) %>% 
  summarise(mean = mean(value), sd = sd(value)) %>%  
  ungroup -> 
  hector_mean


# Plot Hector temp 
ggplot(hector_temp) + 
  # Plot all of the raw hector temp values
  geom_line(aes(year, value, group = run_name, color = "Hector")) + 
  # Add the Hector mean +/- sd ribbon 
  geom_line(data = hector_mean, aes(year, mean, color = "Mean"), size = 1.5) + 
  geom_ribbon(data = hector_mean, 
              aes(x = year, ymin = mean - sd, ymax = mean + sd, color = "Mean", fill = "Mean"), 
              alpha = 0.5) -> 
  plot_layer

# Format the color and background
plot_layer + 
  scale_color_manual(values = c("grey", cbPalette[8])) + 
  scale_fill_manual(values = cbPalette[8]) + 
  guides(fill = FALSE, colour = FALSE) + 
  FIGURE_THEME -> 
  plot_aesthetics 

# Add the lables 
plot_aesthetics + 
  labs(title = "Temp from all 50,00 Hector runs", 
       subtitle = "Ribbon = Hector Run Mean + sd",
       y = "deg C") -> 
  fig_list$"Fig1.Hector_temp"



# Now create the Hector vs obs moving average plot. 

passing_runs <- filter(flags, tempature_flag == "1")  

hector_temp[hector_temp$run_name %in% passing_runs$run_name, ] %>%   
  get_Hector_ma(., variable == "Tgav") ->   
  passing_temp  

# Remove the reference period temp   
passing_temp %>%    
  filter(year %in% 1951 : 1990) %>%    
  group_by(run_name) %>%   
  summarise(ref_value = mean(value)) %>%   
  ungroup ->   
  ref_temps  

passing_temp %>%   
  left_join(ref_temps) %>%   
  mutate(value = value - ref_value) %>%  
  select(names(passing_temp)) ->   
  passing_temp_refRemoved  

# Store a vector of the hector years. This vecotr will be used to increase the number   
# of years in the observation window so that the we can plot the full Hector run results.   
hector_years <- unique(passing_temp_refRemoved$year)  

# Import and complete the temp observation  
incomplete_obs <- readr::read_csv(file.path(BASE, 'int-out', 'observations', 'D.temperature_obervation_ma.csv'))   

incomplete_obs <- read.csv( './int-out/observations/D.temperature_obervation_ma.csv', stringsAsFactors = F )   

tibble(year = hector_years, remove = 1) %>%   
  left_join(incomplete_obs, by = "year") %>%  
  select(year, min, max) ->   
  obs  

# Start making the plots    
ggplot(passing_temp_refRemoved) +   
  geom_line(aes(year, value, group = run_name, color = "Hector")) +  
  geom_ribbon(data = obs, aes(year, ymin = min, ymax = max, color = "Observations", fill = "Observations"),   
              alpha  = 0.7) ->  
  plot_layer  

# Format the color and background  
plot_layer +   
  scale_color_manual(values = c("grey", cbPalette[3])) +   
  scale_fill_manual(values = cbPalette[3]) +   
  guides(fill = FALSE, colour = FALSE) +   
  FIGURE_THEME ->   
  plot_aesthetics   

# Add the lables   
plot_aesthetics +   
  labs(title = "Hector Temp vs Temp Observation Window",   
      caption = "15 year moving average",  
      y = "deg C") ->   
  fig_list$"Fig1B.Hector_temp_obs"



## Fig2.Hector_temp_layers: animated layers of the Hector temp -------------------------------------------------------------------------------

# Plot of Hector temperature colored by the observation filter combinations they pass through. 

# Coombine the filter and hector results data into a single data frame and then split 
# the data frame by the observational filters to make is easy to layer the plots. 
filtered_flags %>% 
  select(run_name, filter) %>%  
  distinct %>% 
  full_join(hector_temp, by = "run_name") %>% 
  split(., .$filter) -> 
  filtered_hector_temp

# Create a complete figure with all of hector runs by filter in separate layers, this will make 
# is easy to remove layers to create the series of plots to use in the power point animation. 
ggplot(data = filtered_hector_temp$None) + 
  geom_line(aes(year, value, color = "All combinations", group = run_name)) + 
  geom_line(data =  filtered_hector_temp$Temp, aes(year, value, color = "Temp", group = run_name)) + 
  geom_line(data = filtered_hector_temp$`Temp, Land Flux`, 
            aes(year, value, color = "Temp, Land Flux", group = run_name)) + 
  geom_line(data = filtered_hector_temp$`Temp, Land Flux, Growth`, 
            aes(year, value, color = "Temp, Land Flux, Growth", group = run_name))  + 
  scale_color_manual(values = filter_colors$color) + 
  FIGURE_THEME + 
  labs(title = "Hector Tgav", 
       y = paste0(unique(filtered_hector_temp$None$variable), " ", unique(filtered_hector_temp$None$units))) +
  theme(legend.title=element_blank()) -> 
  complete_figure_temp

# Use the power_point_animation_figs function to separate the complete figure by layer to get the 
# different plots CH would like to make the animimated graphic. 

fig_list$"Fig2.Hector_temp_layers" <- power_point_animation_figs(complete_figure_temp)


 
## Fig3.Hector_atmGrowth_layers: animated layers of the Hector atm CO2 growth  ----------------------------------

# This chunk does the same thing as the figure 2 section of code. The hector results for CO2 growth that pass through 
# the different filter combinations plotted in a manner so that they look like they are being stacked on top 
# of one another. Because atm CO2 growth is not a Hector output we start by calculating it. 


# Calculate the atm growth from Hector atoms_c for each hector run. This step 
# may take a while. 
hector %>% 
  filter(variable == 'atmos_c') %>% 
  arrange(run_name, year) %>% 
  split(., .$run_name) %>% 
  purrr::map(., function(.){mutate(., value = c(0, diff(value))) %>% 
      filter(year != max(year) , year != min(year))}) %>%
  bind_rows -> 
  hector_ca_growth
  
# Add the filter infromation to the growth data frame and 
# split by filter applied for use in the layering results figure. 
filtered_flags %>% 
  select(run_name, filter) %>%  
  distinct %>% 
  full_join(hector_ca_growth, by = "run_name") %>% 
  split(., .$filter) -> 
  filtered_hector_ca_growth

# First create the complete plot, aka the plot with all of the layers and then 
# use the power_point_animation_figs function to get the layers for the power point.
ggplot(data = filtered_hector_ca_growth$None) + 
  geom_line(aes(year, value, color = "All combinations", group = run_name)) + 
  geom_line(data =  filtered_hector_ca_growth$Temp, aes(year, value, color = "Temp", group = run_name)) + 
  geom_line(data = filtered_hector_ca_growth$`Temp, Land Flux`, 
            aes(year, value, color = "Temp, Land Flux", group = run_name)) + 
  geom_line(data = filtered_hector_ca_growth$`Temp, Land Flux, Growth`, 
            aes(year, value, color = "Temp, Land Flux, Growth", group = run_name))  + 
  scale_color_manual(values = filter_colors$color) + 
  FIGURE_THEME + 
  labs(title = "Hector CO2 Growth", 
       y = paste0(unique(filtered_hector_ca_growth$None$variable)," ", unique(filtered_hector_ca_growth$None$units))) + 
  theme(legend.title=element_blank()) -> 
  complete_figure

fig_list$"Fig3.Hector_atmGrowth_layers" <- power_point_animation_figs(complete_figure)


 
## Fig4.Hector_atmGrowth_layers: animated layers of the Hector landflux ------------------------------------------------------------------------------

# This chunk does the same thing as the figure 2 section of code. The hector results for land flux pass through 
# the different filter combinations plotted in a manner so that they look like they are being stacked on top 
# of one another.


# Add the filter infromation to the land flux data frame and format 
# for the layered plotting method. 
hector_landFlux <- filter(hector, variable == "atm_land_flux") 

filtered_flags %>% 
  select(run_name, filter) %>%  
  distinct %>% 
  full_join(hector_landFlux, by = "run_name") %>% 
  split(., .$filter) -> 
  filtered_hector_landFlux


# First create the complete plot, aka the plot with all of the layers and then 
# use the power_point_animation_figs function to get the layers for the power point.
ggplot(data = filtered_hector_landFlux$None) + 
  geom_line(aes(year, value, color = "All combinations", group = run_name)) + 
  geom_line(data =  filtered_hector_landFlux$Temp, aes(year, value, color = "Temp", group = run_name)) + 
  geom_line(data = filtered_hector_landFlux$`Temp, Land Flux`, 
            aes(year, value, color = "Temp, Land Flux", group = run_name)) + 
  geom_line(data = filtered_hector_landFlux$`Temp, Land Flux, Growth`, 
            aes(year, value, color = "Temp, Land Flux, Growth", group = run_name))  + 
  scale_color_manual(values = filter_colors$color) + 
  FIGURE_THEME + 
  labs(title = "Hector CO2 Growth", 
       y = paste0(unique(filtered_hector_landFlux$None$variable), " ", unique(filtered_hector_landFlux$None$units))) + 
  theme(legend.title=element_blank()) -> 
  complete_figure

Fig4.Hector_atmGrowth_layers <- power_point_animation_figs(complete_figure)


###############################################################################################################
# Part 2 Run and Paramter Figures -----------------------------------------------------------------------------
## Fig5.run_count -------------------------------------------------------------------------------------------
# Bar chart of hector runs with filtered parameters

# Count the number of runs that pass through each filter method. 
filtered_flags %>% 
  group_by(filter) %>% 
  summarise(count = n_distinct(run_index)) %>%  
  ungroup  -> 
  count_df

# Create a bar plot of the number of runs that pass each filtering 
# method with text of the count displayed above each bar. 
count_df %>% 
  ggplot(aes(filter, count, fill = filter)) + 
  geom_col() + 
  geom_text(aes(label = count), position = position_dodge(0.9), 
            vjust=-.2) +
  labs(title = "Hector Runs that pass observation filtering", 
       y = "Number of Runs", 
       x = "Observation Filter Used") + 
  scale_fill_manual(values = filter_colors$color) +
  guides(fill = FALSE) + 
  FIGURE_THEME -> 
  fig_list$"Fig5.run_count"


## Fig6.2100_param -----------------------------------------------------------------------------------

# Plot the parameter value vs the 2100 Hector results value for Tgav and Ca with 
# confidence ellipses based on the observation distributions. 

# CH idea -- linear regressions for the different param values vs 2100. 

# Select the Tgav and Ca values for year 2100 and then add filter information.
hector %>% 
  filter(year == 2100) %>% 
  filter(variable %in% c("Tgav", "Ca")) %>% 
  full_join(filtered_flags, by = "run_name") ->
  hector_2100_values 

# Split the hector 2100 values by parameter and variable in preperation of plotting. 
to_plot_list <- split(hector_2100_values, 
                      interaction(hector_2100_values$parameter, hector_2100_values$variable, sep = "_")) 
  
# Make the scatter plot of the 2100 value vs paramter value for Tgav and Ca. Use map to apply the 
# plotting funtion. 
fig_list$"Fig6.2100_param" <- purrr::map(.x = to_plot_list, function(.x, caption = NULL){
  
  # Store paramter name, variable name, and varaible units as vectors to 
  # use latter on to label the plot. 
  param_name <- unique(.x$parameter)
  var_name   <- unique(.x$variable)
  var_units  <- unique(.x$units)
  
  # Create a layer without the unfiltered observations, this will be 
  # the data used to create the ellipse. 
  filtered_only <- filter(.x, filter != "None")
  
  # Plot the paramter vs variable values a scatter plot colored by filter method. 
  ggplot(.x, aes(param_value, value, color = filter, fill = filter)) + 
    geom_point() + 
    # Add the ellipse layer, here we are using the filtered only observations because 
    # we are not interested in creating a unfitlered ellipse. 
    stat_ellipse(data = filtered_only, aes(param_value, value, color = filter), geom = "polygon",
                 type = "norm", alpha = 0.4) +
    # Add the labels
    labs(title = paste("2100", var_name, "vs", param_name, collapse = " "), 
         caption = caption, 
         y = paste(var_name, var_units, collapse = " "), 
         x = param_name) + 
    # Format so that the figure matches the script figure 
    # themes and color codes.
    scale_color_manual(values = filter_colors$color) + 
    scale_fill_manual(values = filter_colors$color) + 
    FIGURE_THEME 
  
  })
  
  
## Fig7.parameter_space -------------------------------------------------------------------------------------

# Split the parameter data frame by paramter so that we can make an indiviual plot of 
# how the filter changes the paramter space for each paramter. 

list_params_data <- split(filtered_flags, filtered_flags$parameter)

# Paramter space jitter scatter plots. 
fig_list$"Fig7.parameter_space" <- map(list_params_data, function(data = .){
  
  # Store the paramter name for latter
  param_name <- unique(data$parameter)
  
  # Store the max param value to for height when labeling
  height <- max(data$param_value)
  
  # Create a data frame of coutns 
  data %>%
    select(parameter, filter, run_index) %>%
    group_by(filter) %>%  
    summarise(count = n_distinct(run_index)) %>% 
    ungroup  ->
    count_df
  
  # Jitter scatter plot with run count
  data %>%
    ggplot(aes(x = filter, y = param_value, color = filter)) +
    geom_point(position=position_jitterdodge(dodge.width = 0.9)) +
    annotate("text", x=count_df$filter[1], y=height, label= count_df$count[1],  vjust=-.4) +
    annotate("text", x=count_df$filter[2], y=height, label= count_df$count[2],  vjust=-.4) +
    annotate("text", x=count_df$filter[3], y=height, label= count_df$count[3],  vjust=-.4) +
    annotate("text", x=count_df$filter[4], y=height, label= count_df$count[4],  vjust=-.4) +
    labs(title = paste0(param_name, " Parameter Space"), 
         y = paste0(param_name, " value"), 
         x = NULL) + 
    FIGURE_THEME + 
    scale_color_manual(values = filter_colors$color) + 
    theme(axis.text.x = element_blank())
    

})





##############################################################################################################
# Part 3 the GCAM Results Figures ----------------------------------------------------------------------------
# Plot the GCAM results from the runs using the selected carbon cycle parameter results. We currently select 
# paramter sets based on extreeme year 2100 temperatuer values for each filtered group. This methodology 
# may change latter. 
## Functions -------------------------------------------------------------------------------------------------

# gcam_lalyer_plot: is a funciton that used internally in the gcam_results_plot 
# to isolate the layers of the to allow for layering in a power point presentation. 
gcam_lalyer_plot <- function(completePlot){
  
  # Remove the legends so that the plot dimensions remain constant.
  completePlot + 
    guides(color = FALSE, shape = FALSE) -> 
    no_legened_completePlot
  
  # Remove the tmep, flux, growth filtered layer
  partial_fig_2 <- remove_layer(no_legened_completePlot, 6)
 # partial_fig_2 <- remove_layer(partial_fig_2, 8)
  fig_2         <- remove_layer(partial_fig_2, 5)
  
  # Remove the temp, flux filter layer 
  partial_fig_1 <- remove_layer(fig_2, 4)
#  partial_fig_1 <- remove_layer(partial_fig_1, 5)
  fig_1         <- remove_layer(partial_fig_1, 3)
  
  
  list(layer_1 = fig_1, layer_2 = fig_2, layer_3 = no_legened_completePlot)
  
  
}



# gcam_results_plot: is a function that will create a line plot of the gcam results 
# colored by filter applied and a geom shape that reflects the 2100 Hector temp. This 
# function returns a list of plots, a complete plot with a legend and then the layered
# version to be used in the power point animation. 

gcam_results_plot <- function(input_data, title = NULL, caption = NULL, subtitle = NULL){
  
  # Save information for the label 
  units <- unique(input_data$units)
  
  # Use years greater than 2000 and also split by the filter applied and 
  # build the plot layer by layer. This is to make is possible to split the 
  # plot into something that can be layered on itself. 
  input_data %>%  
    filter(year >= 2000) %>% 
    filter(policy == "2p6") %>% 
    split(., .$filter) -> 
    input_data_list 
    
  
  # # Now we are going to have to create the data frame for the "no policy run"
  # # ids. For now the run names are going to be hard coded into the data frame, 
  # # we may want to change this in the future. 
  # tibble(run_name = c("hectorSA-2730", "hectorSA-0378")) %>% 
  #   mutate(policy_keep = "no policy run") %>% 
  #   full_join(input_data, by = "run_name") %>% 
  #   filter(year == 2100) %>% 
  #   filter(policy == "2deg") %>% 
  #   select(policy_keep, value, year, filter, keep) %>% 
  #   # Use ifelse here instead of if_else becasue if_else requires 
  #   # that a tibble column only contains one type of values. 
  #   mutate(value = ifelse(is.na(policy_keep), NA, value)) %>% 
  #   mutate(year = 2105, 
  #          policy = 'no policy run') %>%
  #   select(policy, value, year, filter, keep) %>%  
  #   split(., .$filter) -> 
  #   no_policy_id
  
  

  # Plot the gcam data
  # 
  # Temp filtered layer
  ggplot(data = input_data_list$Temp) + 
    geom_line(aes(year, value, color = "Temp", group = run_name), size = 1) + 
    geom_point(data = input_data_list$Temp, aes(year, value, color = "Temp", shape = keep), size = 1.5) + 
  #  geom_point(data = no_policy_id$Temp, aes(year, value, shape = policy)) + 
    
    # Add the temp and land flux filtered data
    geom_line(data = input_data_list$`Temp, Land Flux`, aes(year, value, color = "Temp, Land Flux", group = run_name), size = 1) + 
    geom_point(data = input_data_list$`Temp, Land Flux`, aes(year, value, color =  "Temp, Land Flux", shape = keep), size = 1.5) + 
  #  geom_point(data = no_policy_id$`Temp, Land Flux`, aes(year, value, shape = policy)) + 
    
    # Add the temp, land flux, and CO2 growth fitlered data to the plot. 
    geom_line(data = input_data_list$`Temp, Land Flux, Growth`, aes(year, value, color = "Temp, Land Flux, Growth", group = run_name), size = 1) + 
    geom_point(data = input_data_list$`Temp, Land Flux, Growth`, aes(year, value, color =  "Temp, Land Flux, Growth", shape = keep), size = 1.5) ->
   # geom_point(data = no_policy_id$`Temp, Land Flux, Growth`, aes(year, value, shape = policy)) -> 
    plot_layer
  
  # Determine what colors to use on the plot
  use_colors <- filter_colors$color[filter_colors$filter %in% unique(data$Tgav$filter)]
  
  # Foramt the graphics of the data
  plot_layer + 
    scale_color_manual(values = use_colors) +
    FIGURE_THEME + 
    labs(title = title,
         caption = caption,
         subtitle = subtitle,
         y = units) + 
    theme(legend.title = element_blank()) -> 
    complete_plot
  
  # Split complete plot up into layers that will be used in the power. 
  plot_layers <- gcam_lalyer_plot(complete_plot)
  
  
  list(complete_plot = complete_plot, layers = plot_layers)
  
}  




## Import and format data ---------------------------------------------------------

# Import the csv file used to generate the ini files and add the filter info. 
readr::read_csv(file.path(BASE, 'int-out', rcpXX, '2A.selected_parameter_sets.csv')) %>% 
  full_join(filtered_flags %>% 
              select(filter_flag, filter) %>% 
              distinct, by = "filter_flag") %>% 
  na.omit %>% 
  mutate(keep = gsub(" Tgav", "", keep)) -> 
  selected_params


# Import the database.proj 
path    <- file.path(BASE, "int-out", "rcp26", "proj_merge_RFtarget.proj")
db_proj <- get(load(path))

# Vector of quries to plot
query_list  <- c("Global mean temperature", "Climate forcing", "CO2 prices", 
                 "CO2 emissions by region", "CO2 concentrations", 
                 "Primary Energy Consumption (Direct Equivalent)", 
                 "Prices for all markets", "GHG emissions by region")
query_names <- c("Tgav", "forcing", "prices", "CO2_emissions", "CO2_con", "Primary_Energy_Consumption",
                 "Prices_for_markets", "GHG_emissions")


# Extract the queries of interest from the data base, format so that 
# the run name will match up with the standard hectorSA-## run name and 
# add info from the selected params data frame
query_data <- map(query_list, function(query = .){ 
  
  getQuery(db_proj, query) %>% 
    # Use the scenario name to create the run_name and policy columns. 
    separate(scenario, c("run_name", "policy")) %>%  
    mutate(run_name = paste0("hectorSA-", run_name)) %>% 
    # Remove the refrence and basline runs 
    filter(run_name != "hectorSA-refr" ) %>% #, policy == "2p6") %>% 
    # Add the filter information
    full_join(selected_params, by = "run_name") %>%
    rename(units = Units)  %>% 
    filter_at(c("run_name", "filter", "keep"), all_vars(!is.na(.))) %>% 
    filter(keep %in% c("min", "max"))
  
  })

# Add the query name to the query data data list
data <- setNames(query_data, query_names)



## Fig8.GCAM_param_selection: Transition Figures ------------------------------------------------------

# The transion to hector GCAM results includes a table of the paramter values and 
# the temperature plot with points reflecting the selected runs.


# Format the selected params data frame so that there is an entry of the paramter 
# values separated by a ",".
selected_params %>% 
  filter(keep %in% c("min", "max")) %>% 
  mutate_at(c("beta", "q10", "s", "diff"), function(.)(. = signif(., digits = 3))) %>% 
  select(run_name, keep, filter, beta, q10, s, diff) %>% 
  group_by(run_name, keep, filter) %>% 
  do( ., mutate(., param_combo = paste0(beta,", ", q10, ", ", s, ", ", diff))) %>% 
  ungroup %>%  
  left_join(filter_colors, "filter") -> 
  selected_runs_formatted



# Now add point information to the selected runs. Where year = 2105 and
# the value = value at year 2100. This data frame will be used to add 
# points to temperature line plot to show how we picked the hector paramter 
# sets to use in gcam-hector.
hector %>% 
  filter(year == 2100) %>% 
  filter(variable == "Tgav") %>% 
  left_join(selected_runs_formatted, by = "run_name") %>% 
  na.omit %>% 
  filter(keep %in% c("min", "max")) %>% 
  select(run_name, year, value, keep, filter) %>%  
  mutate(year = 2105) %>%  
  mutate(year = if_else(filter == "Temp", 2108, year)) ->
  selected_runs_points

# # Now we are going to have to create the data frame for the "no policy run"
# # ids. For now the run names are going to be hard coded into the data frame, 
# # we may want to change this in the future. 
# tibble(run_name = c("hectorSA-2730", "hectorSA-0378")) %>% 
#   mutate(policy_keep = "no policy run") %>% 
#   full_join(selected_runs_points, by = "run_name") %>% 
#   select(policy_keep, value, year, filter, keep) %>% 
#   # Use ifelse here instead of if_else becasue if_else requires 
#   # that a tibble column only contains one type of values. 
#   mutate(value = ifelse(is.na(policy_keep), NA, value)) %>% 
#   mutate(year = 2112, 
#          policy = 'no policy run')  -> 
#   no_policy_id



# Add selected run points or markers to hector temparure figure. Other ideas that 
# might be worth considering include, marking the full line with the symbol or a 
# soild black line just so that it stands out. 
complete_figure_temp + 
  geom_point(data = selected_runs_points,  aes(year, value, color = filter, shape = keep), 
             size = 2) ->
 # geom_point(data = no_policy_id,  aes(year, value, shape = policy), 
  #           size = 2) -> 
  Fig8.GCAM_param_selection



# For the table 
selected_runs_formatted %>%  
  select(keep, filter, param_combo) %>% 
  spread(filter, param_combo) -> 
  table




## Fig9 - 16: GCAM Results Figs  ------------------------------------------------------
# GCAM figures - under 2p6 overshoot, the year 2100 temp indicators will have to be off set. 


use_subtitle <- "policy rcp 2.6"

fig_list$"Fig9.GCAM_temp"       <- gcam_results_plot(data$Tgav, title = "Global Temperature", subtitle = use_subtitle)
fig_list$"Fig10.GCAM_forcing"   <- gcam_results_plot(data$forcing, title = "Climate Forcing", subtitle = use_subtitle)
fig_list$"Fig11.GCAM_co2_price" <- gcam_results_plot(data$prices, title = "Carbon Price", subtitle = use_subtitle)
fig_list$"Fig12.GCAM_co2_conc"  <- gcam_results_plot(data$CO2_con, title = "CO2 Concentration", subtitle = use_subtitle)


# GCAM reports CO2 emissions by region, we will need to aggregate up to Global emissions before 
# making the gcam results.
data$CO2_emissions %>% 
  group_by(run_name, year, keep, filter, units, policy) %>% 
  summarise(value = sum(value)) %>%  
  ungroup -> 
  global_CO2_emissions

fig_list$"Fig13.GCAM_co2_emissions" <- gcam_results_plot(global_CO2_emissions, title = "Gloabl CO2 Emissions",  subtitle = use_subtitle)


# Total primary energy consumption in the US. Had to aggergate the total amount. 
data$Primary_Energy_Consumption %>% 
  filter(region == "USA") %>% 
  group_by(run_name, year, beta, q10, s, diff, filter, units, keep, policy) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  USA_energy_consumption 

fig_list$"Fig14.GCAM_energy_consumption" <- gcam_results_plot(USA_energy_consumption, title = "Total Primary Energy Consumption in USA",  subtitle = use_subtitle)


data$GHG_emissions %>%  
  filter(ghg %in% c("CH4", "N2O")) %>%  
  group_by(year, filter, keep, ghg, units, policy, run_name) %>%  
  summarise(value = sum(value)) %>%  
  ungroup  -> 
  global_CH4_N2O

fig_list$"Fig15.GCAM_CH4_emissions" <- gcam_results_plot(filter(global_CH4_N2O, ghg == "CH4"), title = "Global CH4 Emissions")
fig_list$"Fig16.GCAM_N2O_emissions" <- gcam_results_plot(filter(global_CH4_N2O, ghg == "N2O"), title = "Global N2O Emissions")


## Fig17.GCAM_energy_consumption: Difference between fuel type -------------------------------------------------


 tibble( fuel =  unique(data$Primary_Energy_Consumption$fuel)) %>% 
  mutate(fuel_class = if_else(grepl("[B|b]iomass", fuel), "bioenergy", "NA")) %>% 
  mutate(fuel_class = if_else(fuel %in% c("Coal", "Oil", "Natural Gas"), "fossil fuel", fuel_class)) %>% 
  mutate(fuel_class = if_else(fuel %in% c("Geothermal", "Hydro", "Nuclear", "Solar", "Wind"), "renewable", fuel_class)) %>% 
  na.omit -> 
  fuel_mapping

data$Primary_Energy_Consumption %>% 
  filter(region == "USA", year >= 2000)  %>% 
  spread(policy, value) %>% 
  mutate(value = `2p6` - nop) %>% 
  select(units, fuel, year, filter, value, keep, region) %>% 
  left_join(fuel_mapping, by = "fuel") %>% 
  group_by(year, filter, fuel_class, keep, region) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  agg_primary_energy_consumption

agg_primary_energy_consumption %>% 
# filter(filter == "Temp, Land Flux, Growth") %>%
  filter(filter == "Temp") %>%
  rename(fuel = fuel_class) %>% 
  ggplot(aes(year, value, color = fuel, group = interaction(keep, fuel, filter))) + 
  geom_line() + 
  geom_point(aes(year, value, shape = keep, color = fuel, group = interaction(keep, fuel, filter))) + 
  facet_wrap("filter") + 
  FIGURE_THEME + 
  theme(legend.title=element_blank()) -> 
  fig_list$"Fig17.GCAM_energy_consumption"
  


# data$Primary_Energy_Consumption %>% 
#   filter(region == "USA" & filter == "Temp, Land Flux, Growth") %>% 
#   filter(keep %in% c("min", "max")) %>% 
#   select(year, value, filter, keep, fuel) %>%  
#   spread(keep, value) %>%  
#   mutate(dif = max - min) -> 
#   difference_fuel
# 
# 
# difference_fuel %>% 
#   filter(year >= 2000) %>% 
#   ggplot(aes(year, dif, color = fuel)) + 
#   geom_line(size = 1.5)  +
#   labs(y = "EJ", 
#        title = "Difference in Primary Energy Consumption in the USA\n from 2100 max and min Tgav Temp, Land Flux, Growth filtering") + 
#   FIGURE_THEME -> 
#   script_output[["gcam_diff_fuel"]]


## Fig16: Prices for markets --------------------------------------------------------------
# 
# readr::read_csv(file.path(BASE, "input", "GCAM_region_names.csv"), comment = "#") %>% 
#   pull(region) -> 
#   gcam_region
# 
# 
# market_data <- data$Prices_for_markets
# 
# for(contry in 1:length(gcam_region)){
# 
#   one_country <- gcam_region[contry]  
#   market_data <- mutate(market_data, market = gsub(pattern = one_country, replacement  = "", x = market))
#   
# }
# 
# 
# names(market_data)
# market_data$market %>% unique %>% 
#   tibble::tibble(market = .) %>% 
#   write.csv(., file = file.path(BASE, "market_list.csv"))
# 
# market_data %>% 
#   filter(grepl("energy", market))
# 
# 

# Figure 17: 
##############################################################################################################
# Part 4 Get the numbers for CH --------------------------------------------------------------

# Year 2100 values 
#
# CH asked for year 2100 values on the gcam hector plots. And she also asked for the timing 
# of the peaks in the CO2 emissions plot. This information is supposed to be used 
# for as talking points. 

yr2100_names <- c("filter", "year", "value", "keep", "filter", "variable", "units")

data$Tgav %>% 
  filter(year == 2100) %>%  
  mutate(variable = "Tgav") %>% 
  select(yr2100_names) ->
  temp2100_values

data$forcing %>% 
  filter(year == 2100) %>%  
  mutate(variable = "forcing") %>% 
  select(yr2100_names) ->
  forcing2100_values

data$CO2_con %>% 
  filter(year == 2100) %>%  
  mutate(variable = "CO2_con") %>% 
  select(yr2100_names) ->
  CO2con2100_values

data$prices %>% 
  filter(year == 2100) %>%  
  mutate(variable = "prices") %>% 
  select(yr2100_names) ->
  prices2100_values


global_CO2_emissions %>% 
  filter(year == 2100) %>%  
  mutate(variable = "CO2_emissions") %>% 
  select(yr2100_names) ->
  CO2emissions2100_values

USA_energy_consumption %>% 
  filter(year == 2100) %>%  
  mutate(variable = "primary_energy") %>% 
  select(yr2100_names) ->
  primaryEnergy2100_values


bind_rows(temp2100_values, forcing2100_values, CO2con2100_values, prices2100_values,
          CO2emissions2100_values, primaryEnergy2100_values) %>%  
  select(year, variable, keep, filter, value, units) %>% 
  arrange(year, variable, keep, filter) -> 
  yr2100_df


# Timing and value of the peak 
global_CO2_emissions %>% 
  group_by(filter, keep) %>% 
  filter(value == max(value)) %>%  
  mutate(variable = "global CO2 emissions") %>% 
  select(filter, keep, year, value, variable, units) %>% 
  arrange(filter, keep) -> 
  co2Emissions_peak




##############################################################################################################
# Save -------------------------------------------------------------------------------------------------------

# Save all of the figures as .rda objects
fig_path <- file.path(BASE, "diag-out","UIUC_figures")
dir.create(fig_path, showWarnings = FALSE)
git s

if(save_rda) {
  
  for(index in 1:length(fig_list)){
    
    name   <- paste0(names(fig_list)[index], ".rda")
    object <- fig_list[index]
    
    save(object, file = file.path(fig_path, name))
    
  }
}



if(save_png){
  
  
  message("Have not set up the save png code yet")
  
}


# End 


