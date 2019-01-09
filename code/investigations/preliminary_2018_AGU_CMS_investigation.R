# CMS AGU anlaysis

# Lessons:
# Should always check the intermediate outputs and save copies of them that will not be overwritten. 
# Import the script required libraries
library(rgcam)
library(ggplo2)
library(dplyr)
library(map)

# Define the common DIRs
BASE  <- getwd() # This should be the project location
INPUT <- file.path(BASE, 'out-2', 'AGU')
OUTPUT <- file.path(BASE, 'investigations', 'AGU')

# 1: Look at the restults from the GCAM ref and policy overshoot runs. 


# 1. GCAM ref / policy -------------------------------------------------------------------------------------------------------

# Load the prjdata
load(list.files(INPUT, 'proj_merge_AGU.proj', full.names  = TRUE))

# The first we are going to want to format the data so that each tibble in the project list contains the hector SA run name, 
# and the policy information. 
modify_depth(.x = prjdata, .depth = 2, .f = function(input = .x){
  
  input %>%
    rename(units = Units) %>% 
    separate(scenario, into = c('run', 'policy'), sep = '_')
  
  }) -> 
  formated_porj
  
# Now we want to determine if the policy runs actually worked! We do this by removing the reference global mean temp from 
# policy global mean temp. If the temp change is equal to 0 then we know that the policy run failed for some reason. 
getQuery(formated_porj,  "Global mean temperature") %>% 
  spread(policy, value) %>%  
  mutate(value = `RF-2p6` - nop) -> 
  Tgav_noRef

# Plot the temperature time series, it does look like there is at least one run that had the policy runs fail. 
ggplot(data = Tgav_noRef) + 
  geom_line(aes(year, value, color = run)) + 
  labs(title = 'Policy Tgav - ref Tgav', 
       y = expression(degree~C))





# 2. GCAM policy discount results --------------------------------------------------------------

# Import the results from the discounted and undiscounted policy runs 
read.csv(file.path(BASE, 'out-3', 'AGU', 'Undiscounted_policy_cost.csv'), 
         stringsAsFactors = FALSE, 
         skip = 1) %>% 
  # Minor formatting 
  select(scenario, region = UndiscountedCost, value = UndiscountedCost.1) %>%
  mutate(scenario = gsub(pattern = ',.*', replacement = '', scenario), 
         run_name = gsub(pattern = 'AGUdiscount_', replacement = '', x = scenario), 
         variable = 'undiscounted policy cost') %>%  
  
  # Mulitply value by 1e6 since GCAM reports these values in millions 
  mutate(value = value * 1e6) %>% 
  
  # MUlitply the total cost value by 1.64753738 to convert from 1990$ to 2015$.
  mutate(value = value * 1.64753738,
         units = 'USD$2015') %>% 
  
  # Calculate the global aggregate
  group_by(scenario, run_name, units) %>% 
  summarise(value = sum(value)) %>%  
  ungroup  -> 
  undiscounted_rlst
  
# Import the results from the discounted and discounted policy runs 
read.csv(file.path(BASE, 'out-3', 'AGU', 'Discounted_policy_cost.csv'), 
         stringsAsFactors = FALSE, 
         skip = 1) %>% 
  # Minor formatting 
  select(scenario, region = DiscountedCost, value = DiscountedCost.1) %>%
  mutate(scenario = gsub(pattern = ',.*', replacement = '', scenario), 
         run_name = gsub(pattern = 'AGUdiscount_', replacement = '', x = scenario), 
         variable = 'discounted policy cost') %>%  
  
  # Mulitply value by 1e6 since GCAM reports these values in millions 
  mutate(value = value * 1e6) %>% 
  
  # MUlitply the total cost value by 1.64753738 to convert from 1990$ to 2015$.
  mutate(value = value * 1.64753738,
         units = 'USD$2015') %>% 
  
  # Calculate the global aggregate
  group_by(scenario, run_name, units) %>% 
  summarise(value = sum(value)) %>%  
  ungroup  -> 
  discounted_rlst

# Combine the discounted and undiscounted results 
policy_costs <- bind_rows(undiscounted_rlst, discounted_rlst)



    

