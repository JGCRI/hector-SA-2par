# Purpose: This script looks at the D metric resutls. 

# 0. Set Up -----------------------------------------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Load the required libs
library(purrr)
library(dplyr)
library(ggplot2)
library(VennDiagram)


# Define directories
BASE       <- getwd()
sub_dir    <- 'vary_q10_only'
OUTPUT_DIR <- file.path(BASE, 'out-1', sub_dir)


# Script output 
script_output <- list()

# 1. Import and Format Data Frames ------------------------------------------------------------------------

# Import the parameter values and add run_name column.
npar_wide          <- read.csv(file.path(BASE, 'out-1', sub_dir, 'A.par4_combinations.csv'))
npar_wide$run_name <- paste0( 'hectorSA-', sprintf( '%04d', npar_wide$run_index ) )

# Format the paramter values as a long data frame.
npar_wide %>% 
  select(beta, q10, s, diff, run_name) %>%  
  gather(param, param_value, beta, q10, s, diff) -> 
  npar_long


# Import and format the Dn data.
Dn_metric_unformatted <- read.csv(file.path(OUTPUT_DIR, 'E.all_Dmetric_independent_results.csv'), stringsAsFactors = FALSE)

# Add a flag to determine if the run matches the observations based on Dn stats. 
Dn_matching_long <- mutate(Dn_metric_unformatted, matching = if_else(Dn <= Dc, 1, 0))

# Create a wide Dn matching data frame.
Dn_matching_long %>% 
  select(run_name, variable, matching) %>% 
  distinct %>% 
  spread(key = variable, value = matching) -> 
  Dn_matching_wide


# NPP observation and Hector data 
NPP_data <- read.csv(file.path(OUTPUT_DIR, 'D.NPP_Dmetric_input_table.csv'), stringsAsFactors = FALSE)

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
       caption = 'alpha = 0.05 \n window = 15 \n sigma = 2 * sd \n rolling sd not used on NPP') -> 
  script_output$Dn_matching_count


# Now do a stacked method for the passing run count 
Dn_matching_wide %>% 
#  mutate('Land Flux & Tgav' = if_else(`Tgav` == 1 & `Land Flux` == 1, 1, 0 )) %>% 
 # mutate('Land Flux & Tgav & atm CO2' = if_else(`Tgav` == 1 & `Land Flux` == 1 & `atm CO2` == 1, 1, 0 )) %>% 
  mutate('Tgav & atm CO2' = if_else(`Tgav` == 1 & `atm CO2` == 1, 1, 0 )) %>% 
#  select(run_name, `Land Flux`, `Land Flux & Tgav`, `Land Flux & Tgav & atm CO2`, `Tgav & atm CO2`) %>%
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
       caption = 'alpha = 0.05 \n window = 15 \n sigma = 2 * sd \n rolling sd not used for NPP') + 
  theme(legend.title = element_blank()) ->
  script_output$Dn_matching_count_stacked

# Save the run names as individual sets for used in the VienDiagram! 
atmCO2_set   <- pull(select(filter(Dn_matching_wide, `atm CO2` == 1), run_name))
#LandFlux_set <- pull(select(filter(Dn_matching_wide, `Land Flux` == 1), run_name))
Tgav_set     <- pull(select(filter(Dn_matching_wide, `Tgav` == 1), run_name))
NPP_set      <- pull(select(filter(Dn_matching_wide, `NPP` == 1), run_name))


# venn.diagram(list("Land Flux" = LandFlux_set, "atm CO2" = atmCO2_set, "Tgav" = Tgav_set, 
#                   "NPP" = NPP_set),
#              fill = c("red", "green", "blue", "orange"),
#              alpha = c(0.5, 0.5, 0.5, 0.5), cex = 2, main = "Matching Dn Metric Run Count",
#              filename = "./out-fig/run_count_venndiagram.png")



# 3. Obs vs Hector Figures -------------------------------------------------------------------------------


# Plot all of the Hector NPP values and observational data. Mark the runs that "match" based on the Dn metric. 
NPP_obs <- filter(NPP_data, run_name == 'hectorSA-0001')

NPP_data %>% 
  filter(run_name %in% pull(filter(Dn_matching_wide, NPP == 1), run_name)) -> 
  matching_NPP


ggplot() + 
  geom_line(data = NPP_data, aes(year, model, color = 'Hector', group = run_name)) + 
  geom_line(data = matching_NPP, aes(year, model, color = "Matching Hector", group = run_name)) + 
  geom_ribbon(data = NPP_obs, aes(year, ymin = obs - s2n, ymax = obs + s2n, fill = 'observational data'), alpha = 0.3)+ 
  geom_line(data = NPP_obs, aes(year, obs, color= 'observational data'), size = 1.5) + 
  scale_color_manual(values = c('grey', 'pink', 'blue')) + 
  scale_fill_manual(values = c('blue')) + 
  labs(title = 'Hector Runs vs Global MODIS NPP', 
       x = 'year', 
       y = 'Pg C / Yr') + 
  theme_bw()



  
  




