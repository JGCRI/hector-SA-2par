# Purpose: Visulaize the moving averages are used to filter Hector temperature. 


# 0. Set Up ----------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}
BASE <- getwd()

# Libs 
library( 'tidyr' )
library( 'dplyr' )
library( 'ggplot2' )

# Output 
output      <- list()
output_path <- file.path(BASE, "int-out", "figures")


# Define functions 

# visulaize_obs_hector: is a function that formats wide hector results, reandomly selects some number of 
# randomly selected hecotr runs and plots them against one another. 
visulaize_obs_hector <- function(hector_wide, observations, title = NULL, sample_number = 25){
  
  # Randomly select the Hector runs to plot
  to_plot <- sample(hector_wide$run_name, sample_number, replace = F)
  
  # RCP label
  rcp     <- unique(hector_wide$rcp)
  
  # Subset hector for the randomly selected runs and format 
  # data frame in preperation of plotting.
  hector_wide %>%  
    filter(run_name %in% to_plot) %>% 
    gather(year, value, -run_name, -rcp) %>% 
    mutate(year = as.integer(gsub("X", "", year))) -> 
    hector
  
  # Create a line plot of the observations and the hector runs.
  ggplot() + 
    geom_line(data = hector, aes(year, value, group = run_name, color = "Random Hector Runs")) + 
    geom_ribbon(data = observations, aes(year, ymin = min, ymax = max, fill = "Observations"), alpha = 0.3) + 
    scale_color_manual(values = c("black", "black")) + 
    scale_fill_manual(values = c("black", "black")) + 
    labs(y = "Tgav deg C", 
         title = title, 
         subtitle = rcp, 
         caption = paste0("Randomly selected ", sample_number, " runs.")) + 
    theme(legend.position = "bottom")
  
  
}


# 1. Format and Plot --------------------------------------------------------

# Import observations moving average
observations <- read.csv( './int-out/observations/D.temperature_obervation_ma.csv', stringsAsFactors = F ) 


# Import and format the data frame in preperation for plotting.
list.files(file.path(BASE, 'int-out'), 'D.temperature_hector_ma.csv', full.names = T, recursive = T) %>% 
  lapply(., function(X){readr::read_csv(X) %>% mutate(rcp = X)}) %>% 
  bind_rows %>% 
  # Add the rcp name
  mutate(rcp = gsub("/D.temperature_hector_ma.csv", "", rcp)) %>% 
  mutate(rcp = basename(rcp)) %>%  
  # Split into list format in preperation of plotting.
  split(., .$rcp) -> 
  hector_wide_list

# Make the figures
output <- purrr::map(hector_wide_list, visulaize_obs_hector, observations = observations, title = "Observation Window & Represenative Hector Runs")
  
# 2. Save ------------------------------------------------------------------
save(output, file = file.path(output_path, "D.plots.rda"))
