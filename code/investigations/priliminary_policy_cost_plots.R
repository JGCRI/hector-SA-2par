
# Purpose: create the cost curve figures, we are doing this in a seperate script for 
# now until we figure out what we like about these plots and what we don't like. 

# 0. Set Up ---------------------------------------------------------------------------------------------
# Make sure that the working directory is equal to the project directory
if( ! 'hector-SA-npar.Rproj' %in% list.files() ){ stop( 'Working dir must be the project location' ) }

# Load libs
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(knitr)
library(rgcam)
library(ggExtra)

# Directories
BASE    <- getwd() 
sub_dir <- 'vary_4_params'

# The list of the out-3/gcam.rda files to import and plot. 
rda_list <- list("Discounted policy cost.rda", 
                 "Policy Cost By Period.rda",
                 "Undiscounted policy cost.rda", 
                 "Global mean temperature.rda")

# 1. Import Data --------------------------------------------------------------------------------

gcam_rslt <- lapply(rda_list, function(name = x){
  
  get(load(file.path(BASE, 'out-3', sub_dir, name)))
  
  })

names(gcam_rslt) <- gsub('.rda', '', rda_list)


# 2. Mapping Information ------------------------------------------------------------------------------------------------------

# Factor order for the standalone Hector plots 
hector_filter_order <- c('None', 'atm CO2', 'NPP', 'Tgav', 'atm CO2, NPP', 'atm CO2, Tgav', 'NPP, Tgav', 'atm CO2, NPP, Tgav')

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



# 3. Aggregate Data ---------------------------------------------------------------------------

gcam_rslt$`Discounted policy cost` %>% 
  group_by(run_name, filter_name, units, title, query_name) %>% 
  summarise(value = sum(`DiscountedCost.1`)) %>% 
  filter(filter_name == 'atm CO2, NPP, Tgav') %>%  
  ungroup %>% 
  mutate(region = 'Global') -> 
  discounted_data


gcam_rslt$`Undiscounted policy cost` %>% 
  group_by(run_name, filter_name, units, query_name) %>% 
  summarise(value = sum(`UndiscountedCost.1`)) %>% 
  filter(filter_name == 'atm CO2, NPP, Tgav') %>%  
  ungroup %>% 
  mutate(region = 'Global', 
         title = 'Undiscounted policy cost') %>%
  ungroup -> 
  undiscounted_data


discounted_data %>%  
  bind_rows(undiscounted_data) %>% 
  select(title, region, value, run_name, units) %>% 
  spread(run_name, value) %>% 
  rename('hotter end' = `hectorSA-4595`, 'cooler end' = `hectorSA-4680`) %>% 
  mutate(range = `hotter end` - `cooler end`) %>% 
  select('Query Name' = title, 'Region' = region,  `hotter end`, 
         `cooler end`, range, 'Units' = units) %>% 
  write.csv(., file = file.path(BASE, 'out-3', 'policy-cost.csv'), row.names = FALSE)



gcam_rslt$`Global mean temperature` %>% 
  filter(year == 2100) %>% 
  filter(filter_name == 'atm CO2, NPP, Tgav') 
  

# 3. Plots -------------------------------------------------------------------------------------


# Okay we used the default discount rate of 0.05 
gcam_rslt$`Discounted policy cost` %>% 
  group_by(run_name, filter_name, units, title, query_name) %>% 
  summarise(value = sum(`DiscountedCost.1`)) %>% 
  filter(filter_name == 'atm CO2, NPP, Tgav') -> 
  discounted_data

discounted_data 


  
discounted_data %>% 
  ggplot() + 
  geom_col(aes(run_name, value)) + 
  labs(title = 'Global discounted policy cost', 
       subtitle = 'default discount rate of 0.05',
       y = unique(gcam_rslt$`Discounted policy cost`$units), 
       x = 'Parameter combination from Tgav, atm CO2, and NPP fitlered runs') + 
  UNIVERSTAL_THEME + 
  theme(axis.text.x = element_text(angle = 1800, hjust = 1))

gcam_rslt$`Policy Cost By Period` %>% 
  filter(filter_name == 'atm CO2, NPP, Tgav') %>%
  group_by(run_name, units, query_name, year, filter_name, keep) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  data

data %>% 
  mutate('2100 Tgav' = if_else(keep == 'max', 'hotter', 'cooler')) %>% 
  ggplot() + 
  geom_line(aes(year, value, color = `2100 Tgav`, group = run_name), 
            size = 1.5) + 
  scale_color_manual(values = c("cooler" = "blue", "hotter" = "red")) +
  labs(title = paste0('Global ', unique(data$query_name)), 
    #   subtitle = 'global aggregate', 
       y = unique(data$units), 
       x = 'Year') + 
  UNIVERSTAL_THEME +
  theme(legend.position = 'right') 


data %>%  
  group_by(run_name) %>%  
  summarise(mean = mean(value), 
            max = max(value)) -> 
  summary

abs(diff(summary$mean))



gcam_rslt$`Undiscounted policy cost` %>% 
  group_by(run_name, filter_name, units, query_name) %>% 
  summarise(value = sum(`UndiscountedCost.1`)) %>% 
  filter(filter_name == 'atm CO2, NPP, Tgav') %>%  
  ungroup -> 
  undiscounted_data

undiscounted_data %>% 
  bind_rows(discounted_data) %>% 
  ggplot() + 
  geom_col(aes(run_name, value, fill = query_name, group = query_name), 
           position = 'dodge') + 
  labs(title = 'discounted vs undiscounted policy cost',
       subtitle = 'global aggregate',
       y = unique(gcam_rslt$`Discounted policy cost`$units), 
       x = 'Parameter combination from Tgav, atm CO2, and NPP fitlered runs') + 
  UNIVERSTAL_THEME + 
  theme(axis.text.x = element_text(angle = 1800, hjust = 1)) + 
  theme(legend.position = 'right')

undiscounted_data %>% 
  ggplot() + 
  geom_col(aes(run_name, value), 
           position = 'dodge') + 
  labs(title = 'undiscounted policy cost',
       subtitle = 'global aggregate',
       y = unique(undiscounted_data$units), 
       x = 'Parameter combination from Tgav, atm CO2, and NPP fitlered runs') + 
  UNIVERSTAL_THEME + 
  theme(axis.text.x = element_text(angle = 1800, hjust = 1)) + 
  theme(legend.position = 'right')

undiscounted_data %>% 
  select(run_name, value)
  
abs(diff(undiscounted_data$value))

  
  
  
