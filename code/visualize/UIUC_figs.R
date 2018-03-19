# Purpose: Figures for https://github.com/JGCRI/hector-SA-npar/issues/7#event-1509615854 

# Set Up --------------------------------------------------------------------------------

# Load the libs
library(ggplot2); library(dplyr)
library(tidyr);   library(purrr)
library(caTools); library(rgcam)
library(gridExtra)


# Directory and script set up 
BASE          <- getwd() # the base dir
rcpXX         <- 'rcp26' # the rcp sub directory to pull the data from  
script_output <- list()  # list to save the script figures in 


# Visual constants to make sure that all figures have a constant 
# background and color scheme. 
FIGURE_THEME <- theme_bw()  # Make the figures have a constant ggplot theme
cbPalette    <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # The color pallate to use  


# Part 1 the Hector Results Figures -----
## Import and Format Hector Data ------------------------------------------------------------------------------

# Import the cleaned up hector results, format into long format. Do not calcualte the moving 
# average now because it will take too long. 
hector_all <- readr::read_csv(file.path(BASE, 'int-out', rcpXX, "C.hector_run_cleanup.csv")) 

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





## Figure 1: all temp ---------------------------------------------------------------------------------

# All 50,00 Hector runs - mean and spread for temperature 

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
  script_output[["all_temp"]]


## Figure 2: temp filtering layers -------------------------------------------------------------------------------

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

script_output[["temp_layers"]] <- power_point_animation_figs(complete_figure_temp)


 
## Figure 3: atom_co2 growth vs filtered runs -------------------------------------------------------------------------------

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

script_output[["caGrowth_layers"]] <- power_point_animation_figs(complete_figure)


 
## Figure 4: land_flux vs filter ------------------------------------------------------------------------------

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

script_output[["landFlux_layers"]] <- power_point_animation_figs(complete_figure)


## Figure 5: Run Count  -------------------------------------------------------------

# bar chart of hector runs with filtered parameters

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
  script_output[["passing_hector_runs"]]


## Figure 6: Param vs 2100 -------------------------------------------------------

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
script_output[["parameter_2100value"]] <- purrr::map(.x = to_plot_list, function(.x, caption = NULL){
  
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
  
  
## Figure 7: Param Space --------------------------------------------------------------------

# Split the parameter data frame by paramter so that we can make an indiviual plot of 
# how the filter changes the paramter space for each paramter. 

list_params_data <- split(filtered_flags, filtered_flags$parameter)

# Paramter space jitter scatter plots. 
script_output[["param_space"]] <- map(list_params_data, function(data = .){
  
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





# Part 2 the GCAM Results Figures -----
## Functions ---------------------------------------------------------------------

# gcam_results_plot: is a function that will create a line plot of the gcam results 
# colored by fitler method with year 2100 values id shapes. 

gcam_results_plot <- function(input_data, title = NULL, caption = NULL, subtitle = NULL){
  
  # Save information for the label 
  units <- unique(input_data$units)
  
  # # Create the year 2100 value indicator
  # input_data %>% 
  #   filter(year == 2100) %>% 
  #   select(year, value, filter, keep) %>% 
  #   mutate(year = 2101) %>% 
  #   mutate(year = if_else(filter == "Temp" & keep == "max", 2103, year)) -> 
  #   yr2100_id
  
  input_data <- filter(input_data, year >= 2000)
  
  # Plot the gcam data
  ggplot(data = input_data) + 
    geom_line(aes(year, value, color = filter, group = run_name), size = 1) + 
    # Add the 2100 temperature value id 
    # geom_point(data = yr2100_id, aes(year, value, color = filter, shape = keep), size = 1.5) -> 
    geom_point(data = input_data, aes(year, value, color = filter, shape = keep), size = 1.5) -> 
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
    theme(legend.title = element_blank())
  
  
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
path    <- file.path(BASE, "int-out", "rcp26", "gcam_db", "proj_merge2.proj")
db_proj <- get(load(path))

# Vector of quries to plot
query_list  <- c("Global mean temperature", "Climate forcing", "CO2 prices", 
                 "CO2 emissions by region", "CO2 concentrations", 
                 "Primary Energy Consumption (Direct Equivalent)")
query_names <- c("Tgav", "forcing", "prices", "CO2_emissions", "CO2_con", "Primary_Energy_Consumption")


# Extract the queries of interest from the data base, format so that 
# the run name will match up with the standard hectorSA-## run name and 
# add info from the selected params data frame
query_data <- map(query_list, function(query = .){ 
  
  getQuery(db_proj, query) %>% 
    # Use the scenario name to create the run_name and policy columns. 
    separate(scenario, c("run_name", "policy")) %>%  
    mutate(run_name = paste0("hectorSA-", run_name)) %>% 
    # Remove the refrence and basline runs 
    filter(run_name != "hectorSA-refr" , policy == "2p6") %>% 
    # Add the filter information
    full_join(selected_params, by = "run_name") %>%
    rename(units = Units) %>% 
    filter_at(c("run_name", "filter", "keep"), all_vars(!is.na(.)))
  
  })

# Add the query name to the query data data list
data <- setNames(query_data, query_names)



## Figure 8: Transition Figures ------------------------------------------------------

# The transion to hector GCAM results includes a table of the paramter values and 
# the temperature plot with points reflecting the selected runs.


# Format the selected params data frame so that there is an entry of the paramter 
# values separated by a ",".
selected_params %>% 
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
  select(year, value, keep, filter) %>%  
  mutate(year = 2105) %>%  
  mutate(year = if_else(filter == "Temp", 2110, year)) ->
  selected_runs_points


# Add selected run points or markers to hector temparure figure. Other ideas that 
# might be worth considering include, marking the full line with the symbol or a 
# soild black line just so that it stands out. 
complete_figure_temp + 
  geom_point(data = selected_runs_points,  aes(year, value, color = filter, shape = keep), 
             size = 2) -> 
  script_output[['hector_gcam_selection']]



# For the table 
selected_runs_formatted %>%  
  select(keep, filter, param_combo) %>% 
  spread(filter, param_combo) -> 
  table




## Figures 9 - 14: GCAM Global mean temperature ------------------------------------------------------
# GCAM figures - under 2p6 overshoot, the year 2100 temp indicators will have to be off set. 


use_subtitle <- "policy rcp 2.6"

script_output[["gcam_temp"]]    <- gcam_results_plot(data$Tgav, title = "Global Temperature", subtitle = use_subtitle)
script_output[["gcam_forcing"]] <- gcam_results_plot(data$forcing, title = "Climate Forcing", subtitle = use_subtitle)
script_output[["gcam_prices"]]  <- gcam_results_plot(data$prices, title = "Carbon Price", subtitle = use_subtitle)
script_output[["gcam_CO2con"]]  <- gcam_results_plot(data$CO2_con, title = "CO2 Concentration", subtitle = use_subtitle)


# GCAM reports CO2 emissions by region, we will need to aggregate up to Global emissions before 
# making the gcam results.
data$CO2_emissions %>% 
  group_by(run_name, year, keep, filter, units) %>% 
  summarise(value = sum(value)) %>%  
  ungroup -> 
  global_CO2_emissions

script_output[["gcam_CO2emissions"]] <- gcam_results_plot(global_CO2_emissions, title = "Gloabl CO2 Emissions",  subtitle = use_subtitle)


# Total primary energy consumption in the US. Had to aggergate the total amount. 
data$Primary_Energy_Consumption %>% 
  filter(region == "USA") %>% 
  group_by(run_name, year, beta, q10, s, diff, filter, units, keep) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  USA_energy_consumption 

script_output[["gcam_primaryEnergy"]] <- gcam_results_plot(USA_energy_consumption, title = "Total Primary Energy Consumption in USA",  subtitle = use_subtitle)


# Figure 15: Difference between fuel type -------------------------------------------------
data$Primary_Energy_Consumption %>% 
  filter(region == "USA" & filter == "Temp, Land Flux, Growth") %>% 
  filter(keep %in% c("min", "max")) %>% 
  select(year, value, filter, keep, fuel) %>%  
  spread(keep, value) %>%  
  mutate(dif = max - min) -> 
  difference_fuel


difference_fuel %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(year, dif, color = fuel)) + 
  geom_line(size = 1.5)  +
  labs(y = "EJ", 
       title = "Difference in Primary Energy Consumption in the USA\n from 2100 max and min Tgav Temp, Land Flux, Growth filtering")




# Save ------------------------------------------------------------------------------------

# Save the script output as Rdata object
save(script_output, file = file.path(BASE, "diag-out", paste0("figs_", rcpXX, "_UIUC.rda")))


# Save as pngs 
png_dir <- file.path(BASE, "diag-out", "UIUC_pngs")
dir.create(png_dir)

# Save all of the flat lists first
ggsave(script_output$all_temp, file = file.path(png_dir, "Hector_temp.png"))
ggsave(script_output$passing_hector_runs, file = file.path(png_dir, "passing_run_count.png"))
ggsave(script_output$hector_gcam_selection,file = file.path(png_dir, "hector_gcam_selection.png"))
ggsave(script_output$gcam_temp, file = file.path(png_dir, "hector_gcam_temp.png"))
ggsave(script_output$gcam_forcing, file = file.path(png_dir, "hector_gcam_forcing.png"))
ggsave(script_output$gcam_prices, file = file.path(png_dir, "hector_gcam_prices.png"))
ggsave(script_output$gcam_CO2con, file = file.path(png_dir, "hector_gcam_CO2_concentration.png"))
ggsave(script_output$gcam_CO2emissions, file = file.path(png_dir, "hector_gcam_CO2_emissions.png"))
ggsave(script_output$gcam_primaryEnergy, file = file.path(png_dir, "hector_gcam_USA_primaryEnergy.png"))


# Now save the nested figures

# The paramter space figures
for(i in 1:length(script_output$param_space)){
  
  fig_n <- names(script_output$param_space)[i]
  ggsave(script_output$param_space[[i]], file = file.path(png_dir,paste0("paramSpace_", fig_n,".png")))
  
}

# The paramter value vs varaible 2100 value
for(i in 1:length(script_output$parameter_2100value)){
  
  fig_n <- names(script_output$parameter_2100value)[i]
  ggsave(script_output$parameter_2100value[[i]], file = file.path(png_dir,paste0("param_2100value_", fig_n,".png")))
  
}


# The temperature layers
for(i in 1:length(script_output$temp_layers)){
  
  fig_n <- names(script_output$temp_layers)[i]
  ggsave(script_output$temp_layers[[i]], file = file.path(png_dir,paste0("temp_", fig_n,".png")))
  
}

# The atm CO2 growth layers
for(i in 1:length(script_output$caGrowth_layers)){
  
  fig_n <- names(script_output$caGrowth_layers)[i]
  ggsave(script_output$caGrowth_layers[[i]], file = file.path(png_dir,paste0("co2Growth_", fig_n,".png")))
  
}

# The land flux layers
for(i in 1:length(script_output$landFlux_layers)){
  
  fig_n <- names(script_output$landFlux_layers)[i]
  ggsave(script_output$landFlux_layers[[i]], file = file.path(png_dir,paste0("landFlux_", fig_n,".png")))
  
}



# End 









