# Purpose: This script looks at the D metric resutls. 

# 0. Set Up -----------------------------------------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Load the required libs
library(purrr)
library(dplyr)
library(ggplot2)


# Define directories
BASE       <- getwd()
sub_dir    <- 'rcp26'
OUTPUT_DIR <- file.path(BASE, 'int-out', sub_dir)


# Script output 
script_output <- list()

# 1. Import and Format Data Frames ------------------------------------------------------------------------

# Import the parameter values and add run_name column.
npar_wide          <- read.csv(file.path(BASE, 'int-out', 'A.par4_combinations.csv'))
npar_wide$run_name <- paste0( 'hectorSA-', sprintf( '%04d', npar_wide$run_index ) )

# Format the paramter values as a long data frame.
npar_wide %>% 
  select(beta, q10, s, diff, run_name) %>%  
  gather(param, param_value, beta, q10, s, diff) -> 
  npar_long


# Import and format the Dn data.
Dn_metric_unformatted <- read.csv(file.path(OUTPUT_DIR, 'E.all_Dmetric_results'), stringsAsFactors = FALSE)

# Add a flag to determine if the run matches the observations based on Dn stats. 
Dn_matching_long <- mutate(Dn_metric_unformatted, matching = if_else(Dn <= Dc, 1, 0))

# Create a wide Dn matching data frame.
Dn_matching_long %>% 
  select(run_name, variable, matching) %>% 
  distinct %>% 
  spread(key = variable, value = matching) -> 
  Dn_matching_wide


# 2. Run Count Figures ------------------------------------------------------------------------

# The number of passing runs per variable 
Dn_matching_long %>%
  group_by(variable) %>%
  summarise(count = sum(matching)) %>% 
  ungroup -> 
  run_count_df

ggplot(run_count_df) + 
  geom_col(aes(variable, count, col = variable, fill = variable)) + 
  geom_text(aes(variable, count, label = count), position = position_dodge(0.9), vjust=-.2) + 
  labs(x = NULL, 
       y = 'number of matching runs', 
       title = 'Dn metric matching run count', 
       caption = 'alpha = 0.05 \n window = 15 \n sigma = 2 * sd') -> 
  script_output$Dn_matching_count


# Now do a stacked method for the passing run count 
Dn_matching_wide %>% 
  mutate('Land Flux & Tgav' = if_else(`Tgav` == 1 & `Land Flux` == 1, 1, 0 )) %>% 
  mutate('Land Flux & Tgav & atm CO2' = if_else(`Tgav` == 1 & `Land Flux` == 1 & `atm CO2` == 1, 1, 0 )) %>% 
  mutate('Tgav & atm CO2' = if_else(`Tgav` == 1 & `atm CO2` == 1, 1, 0 )) %>% 
  select(run_name, `Land Flux`, `Land Flux & Tgav`, `Land Flux & Tgav & atm CO2`, `Tgav & atm CO2`) %>%
  gather(stacked_variable, passing, -run_name) %>%
  group_by(stacked_variable) %>% 
  summarise(count = sum(passing)) %>% 
  ungroup -> 
  stacked_run_count
  

ggplot(stacked_run_count) + 
  geom_col(aes(stacked_variable, count, col = stacked_variable, fill = stacked_variable)) + 
  geom_text(aes(stacked_variable, count, label = count), position = position_dodge(0.9), vjust=-.2) + 
  labs(x = NULL, 
       y = 'number of matching runs', 
       title = 'Dn metric matching run count for stacked variables', 
       caption = 'alpha = 0.05 \n window = 15 \n sigma = 2 * sd') + 
  theme(legend.title = element_blank()) ->
  script_output$Dn_matching_count_stacked


