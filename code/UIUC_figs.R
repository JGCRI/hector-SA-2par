# Purpose: Figures for https://github.com/JGCRI/hector-SA-npar/issues/7#event-1509615854 

# Notes, this was the first attempt at making the figures for this GitHub issue however 
# some of the figures were not what CH had intended them to be and therefore the code 
# and figures that will be comitted in the next round will be considerably different from 
# these figures. Also since this version is a rough draft of the UICU figures the code
# is not well documented. 

# Set Up --------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(caTools)
library(rgcam)

BASE <- getwd()

rcpXX <- 'rcp26'

script_output <- list()


#theme_bw() + 
#  theme(panel.grid = element_blank()) -> 
FIGURE_THEME <- theme_bw()

# The color pallate to use  
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Length of years to use in the moving average
windowYrs <- 15 


# Functions ------------------------------------------------------------------------------------------

# The get_Hector_ma function gets the moving average for a specific Hector Variable
get_Hector_ma <- function(data, variable, windowYrs = 15){

  data %>%
    filter(variable == variable) %>%
    split(., .$run_name) %>%
    map(function(x){ x$value <- runmean(x$value, windowYrs, 
                                        alg = c( 'C' ), 
                                        endrule = c( 'mean' ), 
                                        align = c( 'center')); x } ) %>%  
    bind_rows %>% 
    arrange(run_name, year)

}


# Import and Format Hector Data ------------------------------------------------------------------------------

# The filtering by obs window results.
flags <- readr::read_csv(file.path(BASE, 'int-out', rcpXX, 'filter_flag.csv')) 


# Import the cleaned up hector results, format into long format. Do not calcualte the moving 
# average now because it will take too long. 
hector_all <- readr::read_csv(file.path(BASE, 'int-out', rcpXX, "C.hector_run_cleanup.csv")) 

hector_all %>% 
  gather(year, value, -run_name, -variable, -units) %>%  
  mutate(year = as.integer(year)) ->
  hector 


# Part 1 the Hector Results Figures
# Figure 1: all temp ---------------------------------------------------------------------------------
# All 50,00 Hector runs - mean and spread for temperature 

# Subset the Hector temperature and then calculate the moving average
hector %>% 
  filter(variable == "Tgav") ->
  hector_temp

# Find the mean and spread of Hector temp.
hector_temp %>% 
  group_by(year, variable, units) %>% 
  summarise(mean = mean(value), sd = sd(value)) %>%  
  ungroup -> 
  hector_mean

# Plot all of the lines
ggplot(hector_temp) + 
  geom_line(aes(year, value, group = run_name, color = "Hector")) + 
  geom_line(data = hector_mean, aes(year, mean, color = "Mean"), size = 1.5) + 
  geom_ribbon(data = hector_mean, 
              aes(x = year, ymin = mean - sd, ymax = mean + sd, color = "Mean", fill = "Mean"), 
              alpha = 0.5) -> 
  plot_layer

# Format the color and background
plot_layer + 
  scale_color_manual(values = c("black", cbPalette[8])) + 
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


# Figure 2: temp vs obs -------------------------------------------------------------------------------
# Plot of temperature observations on top of Hector runs - probably just the filtered runs

# Select the Hector runs that pass the observation filtering and get the moving average. 
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
  scale_color_manual(values = c("black", cbPalette[3])) + 
  scale_fill_manual(values = cbPalette[3]) + 
  guides(fill = FALSE, colour = FALSE) + 
  FIGURE_THEME -> 
  plot_aesthetics 

# Add the lables 
plot_aesthetics + 
  labs(title = "Hector Temp vs Temp Observation Window", 
       caption = "Only the Hector runs that pass the temp observation\n filtering process are shown here.\n15 year Hector and obs moving average.",
       y = "deg C") -> 
  script_output[["temp_obs"]]

 
# Figure 3: atom_co2 growth vs obs -------------------------------------------------------------------------------
# Plot of atmo observations on top of Hector runs

# Determine which ones should be include in the plot
passing_runs <- filter(flags, growth_flag == "1")

# Calculate the atm growth from Hector atoms_c
hector %>% 
  filter(variable == 'atmos_c') %>% 
  filter(run_name %in% passing_runs$run_name) %>% 
  arrange(run_name, year) %>% 
  split(., .$run_name) %>% 
  purrr::map(., function(.){mutate(., value = c(0, diff(value))) %>% 
      filter(year != max(year) , year != min(year))}) %>%
  bind_rows %>% 
  get_Hector_ma(., "atoms_c") ->
  passing_growth
  
# Create a vector of the years that the obs data should have in order to plot the full Hector run.  
hector_year <- unique(passing_growth$year)

# Import the observational data and add NA values for the years the observational data set is missing.
incomplete_obs <- readr::read_csv(file.path(BASE, 'int-out', 'observations', 'F.CDIAC_growth_ma.csv'))
tibble(year = hector_year, 
       drop = 1) %>%
  left_join(incomplete_obs, by = "year") %>% 
  select(names(incomplete_obs)) -> 
  obs

# Start making the plots  
ggplot(passing_growth) + 
  geom_line(aes(year, value, group = run_name, color = "Hector")) +
  geom_ribbon(data = obs, aes(year, ymin = growth_min, ymax = growth_max, color = "Observations", fill = "Observations"), 
              alpha  = 0.7) -> 
  plot_layer

# Format the color and background
plot_layer + 
  scale_color_manual(values = c("black", cbPalette[3])) + 
  scale_fill_manual(values = cbPalette[3]) + 
  guides(fill = FALSE, colour = FALSE) + 
  FIGURE_THEME -> 
  plot_aesthetics 

# Add the lables 
plot_aesthetics + 
  labs(title = "Hector vs Observation Window atm CO2 growth", 
       caption = "Only the Hector runs that pass the CO2 growth observation\n filtering process are shown here.",
       y = "Change in Pg C") -> 
  script_output[["growth_obs"]] 
  
 
# Figure 4: land_flux vs obs ------------------------------------------------------------------------------
# plot of land flux observations on top of Hector runs

# Start by determining which ones should be include in the plot
passing_runs <- filter(flags, landflux_flag == "1")

# Select the Hector runs to include in the plot 
hector %>% 
  filter(run_name %in% passing_runs$run_name & variable == "atm_land_flux") %>%  
  get_Hector_ma(., "atm_land_flux") ->
  passing_landFlux 

# Create a vector of the years that the obs data should have in order to plot the full Hector run.  
hector_year <- unique(passing_landFlux$year)


# Import the observational data and add NA values for the years the observational data set is missing.
incomplete_obs <- readr::read_csv(file.path(BASE, 'int-out', 'observations', 'E.CDIAC_obervation_ma.csv'))
tibble(year = hector_year, 
       drop = 1) %>%
  left_join(incomplete_obs, by = "year") %>% 
  select(names(incomplete_obs)) -> 
  obs


# Start making the plots  
ggplot(passing_landFlux ) + 
  geom_line(aes(year, value, group = run_name, color = "Hector")) +
  geom_ribbon(data = obs, aes(year, ymin = land_sink_min, ymax = land_sink_max, color = "Observations", fill = "Observations"), 
              alpha  = 0.7) -> 
  plot_layer

# Format the color and background
plot_layer + 
  scale_color_manual(values = c("black", cbPalette[3])) + 
  scale_fill_manual(values = cbPalette[3]) + 
  guides(fill = FALSE, colour = FALSE) + 
  FIGURE_THEME -> 
  plot_aesthetics 

# Add the lables 
plot_aesthetics + 
  labs(title = "Hector vs Observation Window CO2 Land Flux", 
       caption = "Only the Hector runs that pass the CO2 land flux observation\n filtering process are shown here.",
       y = "Pg C/yr") -> 
  script_output[["landFlux_obs"]] 


# Figure 5: Run Count  -------------------------------------------------------------
# bar chart of hector runs with filtered parameters

# Apply the different filters and count the number of runs. --- may be replace with 
# a mapping tibble or something? 
select_params_filter <- function(data, flags, fname){
  
  if(!is.null(flags)) {
    
    data %>% 
      filter_at(flags, all_vars(. == 1)) %>% 
      mutate(count = nrow(.)) %>% 
      gather(parameter, value, beta, q10, s, diff) %>% 
      mutate(filter = fname) 
    
  } else {
    
    data %>% 
      mutate(count = nrow(.)) %>% 
      gather(parameter, value, beta, q10, s, diff) %>% 
      mutate(filter = fname)
    
  }

}

none                 <- select_params_filter(flags, NULL, "None")
temp                 <- select_params_filter(flags, "tempature_flag", "Temp")
temp_landFlux        <- select_params_filter(flags, c("tempature_flag", "landflux_flag"), "Temp, Land Flux")
temp_landFlux_growth <- select_params_filter(flags, c("tempature_flag", "landflux_flag", "growth_flag"), "Temp, Land Flux, Growth")

params <- bind_rows(none, temp, temp_landFlux, temp_landFlux_growth)


params %>%  
  select(count, filter) %>% 
  distinct %>% 
  ggplot(aes(filter, count, fill = filter)) + 
  geom_col() + 
  geom_text(aes(label = count), position = position_dodge(0.9), 
            vjust=-.6) +
  labs(title = "Hector Runs that pass observation filtering", 
       y = "Number of Runs", 
       x = "Observation Filter Used") + 
  guides(fill = FALSE) -> 
  script_output[["passing_hector_runs"]]


# Figure 6 : Param vs 2100 -------------------------------------------------------
# parameter vs temperature or parameter vs CO2 plots.. do you know what might 
# be clearer? If the points were colored but then we included polygons outlining 
# where the paramter space is becsaue I think there are jsut too many overlapping 
# points in this plot for it to make much sense. I have spent a bit of time 
# looking into the geom_ellipse and also the geom_density 2 d or what ever.. 


# Start by selecting the hector resutls from the year 2100
hector_2100 <- filter(hector, year == 2100)

# Use the select 2100 function apply the different filer combinations. 
select2100_params_filter <- function(hector_2100, data, flags, fname){
  
  if(!is.null(flags)) {
    
    data %>% 
      filter_at(flags, all_vars(. == 1)) %>% 
      mutate(count = nrow(.)) %>% 
      gather(parameter, param_value, beta, q10, s, diff) %>% 
      mutate(filter = fname) %>% 
      left_join(hector_2100, by = "run_name") %>%  
      na.omit
    
  } else {
    
    data %>% 
      mutate(count = nrow(.)) %>% 
      gather(parameter, param_value, beta, q10, s, diff) %>% 
      mutate(filter = fname) %>% 
      left_join(hector_2100, by = "run_name") %>%  
      na.omit
    
  }
  
}

param_2100 <- bind_rows(select2100_params_filter(hector_2100, flags, NULL, "None"), 
                        select2100_params_filter(hector_2100, flags, "tempature_flag", "Temp"), 
                        select2100_params_filter(hector_2100, flags, c("tempature_flag", "landflux_flag"), "Temp, Land Flux"), 
                        select2100_params_filter(hector_2100, flags, c("tempature_flag", "landflux_flag", "growth_flag"), "Temp, Land Flux, Growth")) 


param_2100 %>%  
  filter(variable %in% c("Tgav", "Ca")) %>% 
  split(., interaction(.$parameter, .$variable, sep = "_")) -> 
  to_plot_list
  
script_output[["parameter_2100value"]] <- purrr::map(.x = to_plot_list, function(.x, caption = NULL){
  
  param_name <- unique(.x$parameter)
  var_name   <- unique(.x$variable)
  var_units  <- unique(.x$units)
  
  ggplot(.x,aes(param_value, value, color = filter, fill = filter)) + 
    geom_point() + 
    FIGURE_THEME + 
    labs(title = paste("2100", var_name, "vs", param_name, collapse = " "), 
         caption = caption, 
         y = paste(var_name, var_units, collapse = " "), 
         x = param_name)
  
  })
  
  
# Figure 7: Param Space --------------------------------------------------------------------

# Split the parameter data frame by paramter so that we can make an indiviual plot of 
# how the filter changes the paramter space for each paramter. 

list_params_data <- split(params, params$parameter)

# Paramter space jitter scatter plots. 
script_output[["param_space"]] <- map(list_params_data, function(data = .){
  
  # Store the paramter name for latter
  param_name <- unique(data$parameter)
  
  # Store the max param value to for height when labeling
  height <- max(data$value)
  
  # Create a data frame of coutns 
  data %>%
    select(parameter, filter, count) %>%
    distinct ->
    count_df
  
  # Jitter scatter plot with run count
  data %>%
    ggplot(aes(x = filter, y = value, color = filter)) +
    geom_point(position=position_jitterdodge(dodge.width = 0.9)) +
    annotate("text", x=count_df$filter[1], y=height, label= count_df$count[1],  vjust=-.4) +
    annotate("text", x=count_df$filter[2], y=height, label= count_df$count[2],  vjust=-.4) +
    annotate("text", x=count_df$filter[3], y=height, label= count_df$count[3],  vjust=-.4) +
    annotate("text", x=count_df$filter[4], y=height, label= count_df$count[4],  vjust=-.4) +
    labs(title = paste0(param_name, " Parameter Space"), 
         y = paste0(param_name, " value"), 
         x = "Observational fitlers applied") + 
    FIGURE_THEME
    

})




# Save ------------------------------------------------------------------------------------
save(script_output, file = file.path(BASE, "diag-out", paste0("figs_", rcpXX, "_UIUC.rda")))

# End 









