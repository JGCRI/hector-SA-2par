
# Purpose: This is not a finalized script yet but it can visualize the GCAM run results 
# from the emissions sclara investigation.

BASE <- getwd()
path <- file.path(BASE, 'out-2', 'scaled_emissions', 'query_results.rda')
data <- get(load(path))

library(ggplot2)

units  <- unique(data$`CO2 prices`$units)
data$`CO2 prices` %>%  
 # filter(policy == 'RF-2p6') %>% 
  ggplot() + 
  geom_line(aes(year, value, color = emissions_scalar, group = policy)) + 
  facet_wrap("emissions_scalar") + 
  labs(title = 'CO2 prices with scaled industrial emissions', 
       y = units)



data$`CO2 prices`$emissions_scalar %>% unique()


data$`CO2 prices` %>% 
  filter(emissions_scalar == '.4') %>% 
  select(policy) %>%  unique()
