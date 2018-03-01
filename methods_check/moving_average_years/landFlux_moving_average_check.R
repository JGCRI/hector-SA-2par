# Purpose: Loook to see what the difference between a 15 and 7 year moving average filter is, 
# right now we are strating with the land flux data

# Conclusion: For CO2 it does not matter but it might for other variables,. 

# 0. Set Up -----------------------------------------------------------------------------------

BASE <- getwd()

# Libs 
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(knitr)

# output 
script_output <- list()

# 1. Import and format data -------------------------------------------------------------------
data_dir <- file.path(BASE, "methods_check",  "moving_average_years")

# filter_flag.csv
yrs15_filter_flag        <- read_csv(file.path(data_dir, "filter_flag_15years_flux.csv"))
yrs15_filter_flag$window <- '15'

yrs7_filter_flag        <- read_csv(file.path(data_dir, "filter_flag_7years_flux.csv"))
yrs7_filter_flag$window <- '7'

filter_flag <- bind_rows(yrs15_filter_flag, yrs7_filter_flag)

# obs
yrs15_obs        <- read_csv(file.path(data_dir, "E.CDIAC_obervation_ma_15years.csv"))
yrs15_obs$window <- '15'

yrs7_obs        <- read_csv(file.path(data_dir, "E.CDIAC_obervation_ma_7years.csv"))
yrs7_obs$window <- '7'

observations <- bind_rows(yrs15_obs, yrs7_obs)


# Hector 
yrs15_hector        <- read_csv(file.path(data_dir, "E.flux_hector_ma_15years.csv"))
yrs15_hector$window <- '15'

yrs7_hector        <- read_csv(file.path(data_dir, "E.flux_hector_ma_7years.csv"))
yrs7_hector$window <- '7'

hector <- bind_rows(yrs15_hector, yrs7_hector)

# 2. Data Plots -------------------------------------------------------------------

# Randomly select the runs to include on the plot
select_runs   <- sample(unique(hector$run_name), 50)

filter(hector, run_name %in% select_runs, variable == "atm_land_flux") %>%  
  gather(year, value, -run_name, -variable, -window) %>% 
  mutate(year = as.integer(gsub("X", "", year))) -> 
  selected_rslt
  
# Plot the observation ribbon and the hector runs against one another. 
ggplot() + 
  geom_ribbon(data = observations, aes(year, ymin = land_sink_min, ymax = land_sink_max, fill = window), alpha = 0.4) -> 
  obs_plot


obs_plot +
  geom_line(data = selected_rslt, aes(year, value, color = window, 
                                      group = interaction(run_name, window))) + 
  theme_bw() + 
  facet_wrap("window") + 
  labs(y = "Land Flux", 
       title = "Comparison of windowing years on Hectos & Obs data", 
       caption = "50 hecotr runs randomly selected") ->
  script_output[["obs_v_hector_ma"]]


obs_plot + 
  theme_bw() + 
  labs(y = "Land Flux", 
       title = "Comparison of windowing years on Obs data") -> 
  script_output[["obs_ma"]]


# 3. Passing Runs Count -------------------------------------------------------------------

# Count the number of runs that pass the atm co2 filter using 7 and 15 year windowing.
filter_flag %>% 
  filter(landflux_flag== 1) %>% 
  group_by(window) %>%  
  summarise(passing_count = n()) %>% 
  kable(.) -> 
  script_output[["passing_count"]]

# 4. Passing Paramter Space -------------------------------------------------------------------

# Do the paramter values that pass the atm co2 fitlering differ? 
filter_flag %>% 
  filter(landflux_flag== 1) %>% 
  select(run_name, beta, q10, s, diff, window) %>%
  distinct %>% 
  gather(parameter, value, -run_name, -window) -> 
  paramter_values

ggplot(paramter_values) +
  geom_density(aes(value, color = window, fill = window), alpha = 0.4) + 
  facet_wrap("parameter", scales = "free") -> 
  script_output[["passing_params"]]

save(script_output, file = file.path(data_dir, "landFlux_figures.rda"))


