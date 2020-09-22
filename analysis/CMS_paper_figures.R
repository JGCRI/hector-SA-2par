## This script makes the figures for the CMS paper 
## First stand alone Hector is driven with historical emissions up until 2014. 
## Then the Hector results are compared with the Temp, atm CO2, and NPP observational products. 
## The paramter results from the extreeme temperatures from rcp26 are used as inputs into GCAM-Hector 
## where they solve for the the carbon taxes given uncertainty in the Earth System. 

### 0. Set Up ----------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggVennDiagram)

# Define the base name directory, should be the project directory
BASE_DIR   <- getwd()
OUTPUT_DIR <- file.path(BASE_DIR, 'analysis', 'CMSpaper_plots')
dir.create(OUTPUT_DIR)

# Consistent plotting scheme 
UNIVERSAL_THEME <- theme_bw(base_size = 14) +   theme(legend.title = element_blank()) 
COLOR_THEME     <- c('No Filter' = "#999999", 
                     'HadCRUT4' = "#E69F00", 
                     'Temp Filter' = "#56B4E9", # blue? 
                     #'Comb. Obs. + CMS Flux' = "#009E73",
                     "#F0E442",
                     "default Hector" = "#0072B2", 
                     'Combined Obs. Filter' = "#D55E00", 
                     "#CC79A7")

# The height and width to use to save the figures. 
PRINT_WIDTH  <- 8
PRINT_HEIGHT <- 6

### 1. Import Data -----------------------------------------------------------------------------
# Import temperature results and the Dn results for the historical driven runs. 
hist_emissions_dir <- file.path(BASE_DIR, "output", "out-1", "hist_emissions")
hist_miss_temp     <- read.csv(file = file.path(hist_emissions_dir, "C.Tgav_hector_run_cleanup.csv"), stringsAsFactors = FALSE)
hist_emiss_dn      <- read.csv(file = file.path(hist_emissions_dir, "E.Dn_metric_results.csv"), stringsAsFactors = FALSE)
tgav_obs           <- read.csv(file = file.path(hist_emissions_dir, 'D.Tgav_Dmetric_input_table.csv'), stringsAsFactors = FALSE)

# Import the paramter mapping file. 
param_mapping <- read.csv(file = file.path(hist_emissions_dir, 'A.par4_combinations.csv'), stringsAsFactors = FALSE)

# Okay wait until there is the combined file... 
# Import the runs that are being fed into the GCAM policy runs 
gcam_output_dir <- file.path(BASE_DIR, 'output', 'out-2', 'CMSpaper')

# Import the GCAM policy run results.
policy_rslts <- readRDS(list.files(gcam_output_dir, 'C.GCAM_rslts.rds', full.names = TRUE))

# Import the GCAM carbon run results. 
gcam_output_dir <- file.path(BASE_DIR, 'output', 'out-2', 'CMSpaper_carbon')

# Import the default GCAM restults
gcam_default_dir <- file.path(BASE_DIR, 'output', 'out-2', 'CMSdefault')
default_gcam     <- readRDS(list.files(gcam_default_dir, 'C.GCAM_rslts.rds', full.names = TRUE))

# Import the total cost results 
total_policy_rslts  <- readRDS(file.path(BASE_DIR, 'output', 'out-3', 'CMSpaper', 'total_policy_cost.rds'))
total_carbon_rslts  <- readRDS(file.path(BASE_DIR, 'output', 'out-3', 'CMSpaper_carbon', 'total_policy_cost.rds'))


### 2. Create the Dn metric mapping file ------------------------------------------------------
# Subset the dn metric file so that it only contains values for the runs that have the matching Dn values.
# and rename the filters.
hist_emiss_dn %>% 
  filter(Dn <= Dc) %>%  
  dplyr::filter(filter_name %in% c("Tgav", "Tgav, NPP, atmCO2 multi optimized")) %>%  
  mutate(filter_name = if_else(filter_name == "Tgav, NPP, atmCO2 multi optimized", 'Combined Obs. Filter', filter_name)) %>% 
  mutate(filter_name = if_else(filter_name == "Tgav", 'Temp Filter', filter_name)) %>% 
  # Add a set of all the unfiltered runs
  bind_rows(tibble(run_name = hist_emiss_dn$run_name, 
                   filter_name = 'No Filter')) %>%  
  select(run_name, filter_name) %>%  
  distinct ->
  dn_run_mapping





## Stand Alone Hector Plots -----------------------------------------------------------------------------
### 3.1  Temp comparison plot ####
# Make a plot of the categorize historical emission driven temperature as not passing and 
# passing vs the temperature observations. 
hector_temp_runs <- full_join(tgav_obs, dn_run_mapping, by = 'run_name') 

# Format the observationaal data set 
tgav_obs %>% 
  select(year, obs, sigma2) %>% 
  distinct() -> 
  obs_to_plot

# Make the line plot of the unfiltered runs, the temp filtered runs, and then the obs product. 
ggplot() + 
  geom_line(data = filter(hector_temp_runs, filter_name == 'No Filter'),
            aes(year, model, color = filter_name, group = run_name)) +
  geom_line(data = filter(hector_temp_runs, filter_name == 'Temp Filter'),
            aes(year, model, color = filter_name, group = run_name)) +
  geom_line(data = obs_to_plot, aes(year, obs, color = 'HadCRUT4'), 
            size = 1) +
  labs(x = 'Year', 
       y = expression('Temperature Anomaly ' ~ degree ~ 'C')) + 
  scale_color_manual(values = COLOR_THEME) + 
  UNIVERSAL_THEME + 
  theme(legend.position = c(0, 1), 
       legend.justification = c(0, 1), 
       legend.background = element_rect(colour = "black")) + 
  scale_x_continuous(breaks = seq(from = 1640, to = 2100, by = 10)) ->
  hector_obs_comparison_plot

ggsave(plot = hector_obs_comparison_plot, filename = file.path(OUTPUT_DIR, '1A.hector_tgav_obs_comparison_plot.png'),
       width = PRINT_WIDTH, height = PRINT_HEIGHT)


### 3.2. Run Count ####
# Count the number of runs
dn_run_mapping %>%  
  group_by(filter_name) %>% 
  summarise(count = n()) ->
  run_count_df
  
run_count_df$filter_name <- factor(x = run_count_df$filter_name, c('No Filter', 'Temp Filter', 'Combined Obs. Filter'), ordered = TRUE)

ggplot(run_count_df) + 
  geom_bar(aes(filter_name, count, fill = filter_name), stat = 'identity') + 
  geom_text(aes(x = filter_name, y = count, label = count), vjust = -0.5) +
  UNIVERSAL_THEME + 
  scale_fill_manual(values = COLOR_THEME) + 
  labs(x = NULL, 
       y = 'Number of Passing Runs') + 
  theme(axis.text.x = element_text(size = 10), 
        legend.position = 'none') -> 
  run_cout_plot

ggsave(run_cout_plot, file = file.path(OUTPUT_DIR, '2.run_count.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT)


### 3.4. Jitter Parameter Plots ####
param_mapping %>%
  full_join(dn_run_mapping, by = 'run_name') %>%
  rename(Beta = beta, Q10 = q10, S = s, Diff = diff) %>%
  gather(param, param_value, Beta, Q10, S , Diff) ->
  cate_params

# The paramater jitter plots 
cate_params$filter_name <- factor(cate_params$filter_name, 
                                  levels =  c('No Filter', 'Temp Filter', 'Combined Obs. Filter'), 
                                  ordered = TRUE)
cate_params$param       <- factor(cate_params$param, 
                                  levels = c("Beta", "Q10", "S", 'Diff'), 
                                  labels = c("beta", "Q[10]", "S", 'kappa'), 
                                  ordered = TRUE) 


ggplot(cate_params, aes(filter_name, param_value, color = filter_name, group = filter_name)) + 
  geom_jitter(height = 0) + 
  facet_wrap("param", scales = 'free', 
             labeller = label_parsed) + 
  UNIVERSAL_THEME + 
  scale_color_manual(values = COLOR_THEME) + 
  theme(legend.position = 'none') + 
  labs(y = 'Parameter Value', 
       x = NULL) +
  theme(axis.text.x = element_text(size = 8), 
        strip.background =element_rect(fill="white")) ->
  jitter_plot 

ggsave(jitter_plot, file = file.path(OUTPUT_DIR, '3A.parameter_jitter_plot.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT) 

### 3.5. Bivariate Paramter Plots ####

cate_params %>% 
  select(run_name, filter_name, param, param_value) %>% 
  spread(param, param_value) -> 
  wide_cate_params

ggplot(data = wide_cate_params) + 
  geom_point(aes(S, kappa, color = filter_name)) + 
  facet_wrap("filter_name") + 
  UNIVERSAL_THEME + 
  scale_color_manual(values = COLOR_THEME) + 
  theme(legend.position = 'none') + 
  labs(x = expression('S ('~degree~'C)'), 
       y = expression(kappa~' (cm'^2~'m'^-1~')')) -> 
  cliamte_bivariate_plot


ggplot(data = wide_cate_params) + 
  geom_point(aes(`Q[10]`,`beta`, color = filter_name)) + 
  facet_wrap("filter_name") + 
  UNIVERSAL_THEME + 
  scale_color_manual(values = COLOR_THEME) + 
  theme(legend.position = 'none') + 
  labs(x = expression(Q[10]), 
       y = expression(beta)) -> 
  carbon_bivariate_plot

ggsave(cliamte_bivariate_plot, file = file.path(OUTPUT_DIR, '3B.climate_bivariate_plot.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT) 
ggsave(carbon_bivariate_plot, file = file.path(OUTPUT_DIR, '3C.carbon_bivariate_plot.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT) 


### 3.6 Bivarte Plots With Hector-GCAM Runs Highlighted --------------------------------------------------

policy_rslts$`CO2 prices` %>% 
  select(run_name, filter_name, keep) %>%  
  filter(filter_name != 'None') %>% 
  mutate(filter_name = if_else(filter_name == 'Tgav, NPP, atmCO2 multi optimized', 'Combined Obs. Filter', 'Temp Filter')) %>% 
  mutate(GCAM = TRUE) %>%  
  full_join(wide_cate_params, by = c("run_name", "filter_name")) %>% 
  filter(GCAM) -> 
  wide_cate_params_gcam

ggplot(data = wide_cate_params) + 
  geom_point(aes(S, kappa, color = filter_name, alpha = 0.1), shape = 16) + 
  geom_point(data = wide_cate_params_gcam,
             aes(S, kappa, shape = keep), color = 'black') + 
   facet_wrap("filter_name") + 
  UNIVERSAL_THEME + 
  scale_color_manual(values = COLOR_THEME) + 
  theme(legend.position = 'none') + 
  labs(x = expression('S ('~degree~'C)'), 
       y = expression(kappa~' (cm'^2~'m'^-1~')')) -> 
  cliamte_bivariate_plot


ggplot(data = wide_cate_params %>% 
         filter(filter_name != 'No Filter')) + 
  geom_point(aes(`Q[10]`, beta, color = filter_name), shape = 16, alpha = 0.25) + 
  geom_point(data = wide_cate_params_gcam,
             aes(`Q[10]`, beta, shape = keep), color = 'black') + 
  facet_wrap("filter_name") + 
  UNIVERSAL_THEME + 
  scale_color_manual(values = COLOR_THEME) + 
  theme(legend.position = 'none') + 
  labs(x = expression('S ('~degree~'C)'), 
       y = expression(kappa~' (cm'^2~'m'^-1~')')) -> 
  cliamte_bivariate_plot



library("GGally")

ggpairs(wide_cate_params %>% 
          filter(filter_name != 'No Filter'), columns = 3:6, ggplot2::aes(colour=filter_name)) 


ggpairs(wide_cate_params_gcam, columns = 5:8, ggplot2::aes(colour=filter_name, shape = keep)) 



### 4. Table of the paramter values going into the total discount costs ----------------------------------
policy_rslts$`CO2 prices`%>% 
  filter(run_name %in% c('hectorSA-0319', 'hectorSA-1155', 'hectorSA-0650', 'hectorSA-2600')) %>%  
  select(run_name, filter_name, keep, beta, q10, s, diff) %>%  
  distinct() %>%  
  select(-run_name) %>% 
  arrange(filter_name, keep) -> 
  params_in_total_cost

write.csv(params_in_total_cost, file = file.path(OUTPUT_DIR, 'total_discount_policy_params.csv'), row.names = FALSE)

##  GCAM Plots -------------------------------------------------------------------------------------------
# Function that makes the gcam cone + line plots
layered_ribbon_spaghetti <- function(input, p, title = NULL, ylab = NULL, xlab = NULL){
  
  # Subset the data by the policy to plot
  to_plot <- filter(input, policy == p) %>% 
    filter(filter_name != 'None') %>% 
    mutate(filter_name = if_else(filter_name == 'Tgav, NPP, atmCO2 multi optimized', 'Combined Obs. Filter', 'Temp Filter')) 
    
  # Calcualte the ribbon plots 
  to_plot %>% 
    group_by(year, policy, filter_name) %>% 
    dplyr::summarise(min = min(value), 
                     max = max(value)) %>% 
    ungroup ->
    ribbon 
  
  
  ggplot() + 
    geom_ribbon(data = ribbon, 
                aes(year, ymin = min, ymax = max, fill = filter_name), 
                alpha = 0.5) +
    geom_line(data = to_plot, 
              aes(year, value, color = filter_name, group = run_name), 
              size = 0.3) +
    UNIVERSAL_THEME + 
    scale_color_manual(values = COLOR_THEME) + 
    scale_fill_manual(values = COLOR_THEME) +
    theme(legend.position = 'bottom') + 
    labs(title = title, 
         y = ylab, 
         x = xlab)
  
}


# Modify the default hector gcam results so that the filter name is the default gcam (so that script color theme works).
default_gcam %>% 
  purrr::map(function(input){mutate(input, filter_name = 'default Hector')}) -> 
  default_gcam


### 5. CO2 prices ----------------------------------------------------------------------------------------
CO2_price_plot <- layered_ribbon_spaghetti(input = policy_rslts$`CO2 prices` %>% 
                           filter(year >= 2010), p = 'RF-2p6', title = expression('CO'[2]~' Price'), 
                           ylab =  expression('2015$'/'CO'[2]~' price (tax)'), x = 'Year') + 
  scale_x_continuous(breaks = seq(from = 1640, to = 2100, by = 10))
  

ggsave(CO2_price_plot, file = file.path(OUTPUT_DIR, '5A.Global_CO2_price.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT)

# Save selected summary information
policy_rslts$`CO2 prices` %>% 
  bind_rows(gcam_cmsflux$`CO2 prices`) %>% 
  filter(year %in% c(2050, 2100)) %>% 
  group_by(filter_name, year) %>% 
  summarise(min = min(value), max = max(value), mean = mean(value)) %>% 
  ungroup %>% 
  mutate(variable = 'CO2 price', 
         units = '2015$/tCO2') %>% 
  write.csv(file = file.path(OUTPUT_DIR, '5.Global_CO2_price_summary.csv'), row.names = FALSE)

### 6. CO2 Emissions ----------------------------------------------------------------------------------------
policy_rslts$`CO2 emissions by region` %>% 
  filter(year >= 2010) %>% 
  group_by(units, year, run_name, policy, filter_name, keep) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  ungroup -> 
  global_co2_emissions_df 

gcam_cmsflux$`CO2 emissions by region` %>% 
  filter(year >= 2010) %>% 
  group_by(units, year, run_name, policy, filter_name, keep) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  ungroup -> 
  global_co2_emissions_df_cmsFlux


Global_co2_emiss_plot <- layered_ribbon_spaghetti(input = global_co2_emissions_df, p = 'RF-2p6', 
                                                  title = expression('Global CO'[2]~' Emissions'), ylab = expression("MTCO"[2]), x = 'Year') + 
  scale_x_continuous(breaks = seq(from = 1640, to = 2100, by = 10))

ggsave(Global_co2_emiss_plot, file = file.path(OUTPUT_DIR, '6A.Global_CO2_emiss.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT)


# Save selected summary information
global_co2_emissions_df %>% 
  bind_rows(global_co2_emissions_df_cmsFlux) %>% 
  na.omit %>% 
  filter(year %in% c(2050, 2100)) %>% 
  group_by(filter_name, year) %>% 
  summarise(min = min(value), max = max(value), mean = mean(value)) %>% 
  ungroup %>% 
  mutate(variable = 'CO2 Emissions', 
         units = 'MTCO2') %>% 
  write.csv(file = file.path(OUTPUT_DIR, '6.Global_CO2_emiss_summary.csv'), row.names = FALSE)

# Summary of the peak emissions 
global_co2_emissions_df %>% 
  bind_rows(global_co2_emissions_df_cmsFlux) %>% 
  na.omit %>% 
  group_by(run_name) %>% 
  filter(value == max(value)) %>% 
  ungroup %>% 
  group_by(filter_name) %>% 
  filter(value == max(value)) %>%
  bind_rows(tibble::tibble(units = 'MTCO2', 
                           year = 2015, 
                           run_name = 'All the runs that peak in 2015', 
                           policy = 'RF-2p6', 
                           filter_name = 'both', 
                           keep = NA, 
                           value = 36596.34)) %>% 
  write.csv(file = file.path(OUTPUT_DIR, '6.Global_CO2_emiss_peak_info.csv'))


# When the runs start being net negative 
global_co2_emissions_df %>% 
  bind_rows(global_co2_emissions_df_cmsFlux) %>% 
  na.omit %>% 
  filter(value <= 0) %>% 
  group_by(run_name) %>% 
  filter(year == min(year)) %>% 
  group_by(filter_name) %>% 
  filter(year == min(year)|year == max(year)) %>% 
  ungroup %>% 
  select(units, year, value, run_name) %>% 
  mutate(variable = 'when CO2 emissions start net neg') %>% 
  write.csv(file = file.path(OUTPUT_DIR, '6.Global_CO2_emiss_netNeg_info.csv'))


### 7i. Global Energy ----------------------------------------------------------------------------------------------------------------------- 
policy_rslts$`Primary Energy Consumption (Direct Equivalent)` %>% 
  filter(units == 'EJ') %>% 
  distinct %>% 
  group_by(year, run_name, filter_name, fuel, keep) %>%  
  summarise(value = sum(value)) %>% 
  ungroup -> 
  fuel_global

gcam_cmsflux$`Primary Energy Consumption (Direct Equivalent)` %>%
  filter(units == 'EJ') %>%
  distinct %>%
  group_by(year, run_name, filter_name, fuel, keep) %>%
  summarise(value = sum(value)) %>%
  ungroup ->
  fuel_global_cms

# Fuel Plot
fuel_global %>%
  # Pull out the min and max hector runs
  filter(run_name %in% c('hectorSA-0319', 'hectorSA-1155', 'hectorSA-0650', 'hectorSA-2600')) %>%
  filter(year %in% c(2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  mutate(keep = if_else(keep == 'max', 'Most Expensive', 'Least Expensive')) %>%
  filter(keep == 'Most Expensive') %>%
  mutate(filter_name = if_else(filter_name == 'Tgav', 'Temp Filter', 'Combined Obs. Filter')) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(fuel = if_else(fuel == 'total biomass', 'Total Biomass', fuel)) -> 
  to_plot 

to_plot$filter_name <- factor(to_plot$filter_name, levels =  c( "Temp Filter", "Combined Obs. Filter"), ordered = TRUE)

to_plot %>% 
  ggplot(aes(x = year, y = value, fill = fuel)) +
  geom_bar(stat = "identity", position = 'stack') +
  facet_grid(.~filter_name ) +
  UNIVERSAL_THEME +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background =  element_rect(fill = 'white')) +
  labs(x = 'Year',
       y = 'EJ') ->
  global_energy_plot
ggsave(global_energy_plot, file = file.path(OUTPUT_DIR, '7.Global_energy_consumption.png'), width = PRINT_WIDTH, height = 3)


# Calcualte the total energy 
fuel_global %>% 
  group_by(year, run_name, filter_name) %>%  
  summarise(totalE = sum(value)) %>%  
  ungroup -> 
  total_E

fuel_global_cms %>% 
  group_by(year, run_name, filter_name) %>%  
  summarise(totalE = sum(value)) %>%  
  ungroup -> 
  total_E_cms

# Percent Bio Mass 
fuel_global %>%  
  filter(run_name %in% c('hectorSA-0319', 'hectorSA-1155', 'hectorSA-0650', 'hectorSA-2600')) %>%  
  left_join(total_E,  by = c("year", "run_name", "filter_name")) %>%  
  filter(year %in% c(2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>% 
  mutate(keep = if_else(keep == 'max', 'Most Expensive', 'Least Expensive')) %>% 
  mutate(filter_name = if_else(filter_name == 'Tgav', 'Temp Filter', 'Combined Obs. Filter')) %>%  
  filter(fuel == 'total biomass') %>% 
  mutate(percent = 100 * value / totalE, 
         year = as.character(year)) -> 
  percent_biomass_df

percent_biomass_df$filter_name <- factor(percent_biomass_df$filter_name, levels = c("Temp Filter", "Combined Obs. Filter"), ordered = TRUE)

 
percent_biomass_df %>%
  filter(keep == 'Most Expensive') %>% 
  ggplot(aes(x = year, y = percent)) +
  geom_bar(stat = "identity", position = 'stack') +
  facet_grid(. ~ filter_name) +
  UNIVERSAL_THEME +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background =element_rect(fill="white"), 
        legend.position = 'none')  +
  labs(x = 'Year',
       y = '% Biomass Consumed') ->
  percent_biomass_plot
ggsave(percent_biomass_plot, file = file.path(OUTPUT_DIR, '7.Percent_E_biomass_bar.png'), width = PRINT_WIDTH, height = 3)


### 7ii. Regional Energy ----------------------------------------------------------------------------------------------------------------------- 
policy_rslts$`Primary Energy Consumption (Direct Equivalent)` %>% 
  filter(units == 'EJ' & region %in% c('China', 'USA')) %>% 
  distinct %>% 
  group_by(year, run_name, filter_name, fuel, keep, region) %>%  
  summarise(value = sum(value)) %>% 
  ungroup -> 
  fuel_region

gcam_cmsflux$`Primary Energy Consumption (Direct Equivalent)` %>% 
  filter(units == 'EJ' & region %in% c('China', 'USA')) %>% 
  distinct %>% 
  group_by(year, run_name, filter_name, fuel, keep, region) %>%  
  summarise(value = sum(value)) %>% 
  ungroup -> 
  fuel_region_cms

# Fuel Plot 
fuel_region %>%
  # Pull out the min and max hector runs  
  filter(run_name %in% c('hectorSA-0319', 'hectorSA-1155', 'hectorSA-0650', 'hectorSA-2600')) %>%  
  filter(year %in% seq(from = 2010, to = 2100, by = 10)) %>% 
  mutate(keep = if_else(keep == 'max', 'Most Expensive', 'Least Expensive')) %>% 
  mutate(filter_name = if_else(filter_name == 'Tgav', 'Temp Filter', 'Combined Obs. Filter')) %>% 
  mutate(year = as.character(year)) %>% 
  filter(keep == 'Most Expensive') %>%  
  ggplot(aes(x = year, y = value, fill = region, group = region)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  facet_grid(. ~ filter_name) + 
  UNIVERSAL_THEME + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.background = element_rect(fill = 'white')) + 
  labs(x = 'Year', 
       y = 'Total Energy EJ') -> 
  regional_energy_plot
ggsave(regional_energy_plot, file = file.path(OUTPUT_DIR, '7.Regional_energy_consumption.png'), width = PRINT_WIDTH, height = 3)


# Calcualte the total energy 
fuel_region %>% 
  group_by(year, run_name, filter_name, region) %>%  
  summarise(totalE = sum(value)) %>%  
  ungroup -> 
  total_E

fuel_region_cms %>% 
  group_by(year, run_name, filter_name, region) %>%  
  summarise(totalE = sum(value)) %>%  
  ungroup -> 
  total_E_cms

# Percent Bio Mass 
fuel_region %>%  
  filter(run_name %in% c('hectorSA-0319', 'hectorSA-1155', 'hectorSA-0650', 'hectorSA-2600')) %>%  
  left_join(total_E,  by = c("year", "run_name", "filter_name", 'region')) %>%  
  filter(year %in% seq(from = 2010, to = 2100, by = 10)) %>% 
  mutate(keep = if_else(keep == 'max', 'Most Expensive', 'Least Expensive')) %>% 
  mutate(filter_name = if_else(filter_name == 'Tgav', 'Temp Filter', 'Combined Obs. Products')) %>%  
  filter(keep == 'Most Expensive') %>% 
  filter(fuel == 'total biomass') %>% 
  mutate(percent = 100 * value / totalE) %>% 
  ggplot(aes(x = year, y = percent, fill = region)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  facet_grid(. ~ filter_name) + 
  UNIVERSAL_THEME + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.background = element_rect(fill = 'white'))  + 
  labs(x = 'Year', 
       y = '% Biomass') -> 
  percent_biomass_plot
ggsave(percent_biomass_plot, file = file.path(OUTPUT_DIR, '7.Regional_percent_E_biomass_bar.png'), width = PRINT_WIDTH, height = 3)


### 8. Temp Plot  ----------------------------------------------------------------------------------------
global_temp <- layered_ribbon_spaghetti(input = policy_rslts$`Global mean temperature`, 
                                             p = 'RF-2p6', title = expression('Global Mean Temperature Anomaly'), 
                                           ylab =  expression(degree~'C'), x = 'Year') + 
  scale_x_continuous(breaks = seq(from = 1640, to = 2100, by = 10)) 
  

ggsave(global_temp, file = file.path(OUTPUT_DIR, '8A.Global_temp.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT)


# Save selected summary information
policy_rslts$`Global mean temperature` %>% 
#  bind_rows(gcam_cmsflux$`Global mean temperature`) %>% 
  filter(year %in% c(2050, 2100)) %>% 
  group_by(filter_name, year) %>% 
  summarise(min = min(value), max = max(value), mean = mean(value)) %>% 
  ungroup %>% 
  mutate(variable = 'Tgav', 
         units = 'degC') %>% 
  write.csv(file = file.path(OUTPUT_DIR, '8.Global_temp_summary.csv'), row.names = FALSE)

### 9. RF  ----------------------------------------------------------------------------------------
global_rf <- layered_ribbon_spaghetti(input = policy_rslts$`Climate forcing`,
                                        p = 'RF-2p6', title = expression('Climate Forcing'), 
                                        ylab =  expression('W/m'^2), x = 'Year') + 
  scale_x_continuous(breaks = seq(from = 1640, to = 2100, by = 10))
  

ggsave(global_rf, file = file.path(OUTPUT_DIR, '9A.Global_RF.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT)


# Save selected summary information
policy_rslts$`Climate forcing` %>% 
 # bind_rows(gcam_cmsflux$`Climate forcing`) %>% 
  na.omit %>% 
  filter(year %in% c(2050, 2100)) %>% 
  group_by(filter_name, year) %>% 
  summarise(min = min(value), max = max(value), mean = mean(value)) %>% 
  ungroup %>% 
  mutate(variable = 'forcing', 
         units = 'W/m^2') %>% 
  write.csv(file = file.path(OUTPUT_DIR, '9.Global_RF_summary.csv'), row.names = FALSE)


# Summary of the peak emissions 
policy_rslts$`Climate forcing` %>% 
 # bind_rows(gcam_cmsflux$`Climate forcing`) %>% 
  na.omit %>% 
  group_by(run_name) %>% 
  filter(value == max(value)) %>% 
  ungroup %>% 
  group_by(filter_name) %>% 
  filter(value == max(value)| value == min(value)) %>%
  select(year, value, units, filter_name) %>% 
  mutate(variable = 'total RF') %>% 
  write.csv(file = file.path(OUTPUT_DIR, '9.climate_forcing_peak_info.csv'))


### 10. Policy Costs ------------------------------------------------------------------------------

# First figure the filter_name info for the hector-SA-npar runs. 
policy_rslts$`CO2 prices` %>% 
  filter(year == 2050) %>% 
  group_by(filter_name) %>% 
  filter(value == min(value) | value == max(value)) %>%  
  ungroup %>% 
  select(run_name, filter_name, keep, s, diff, beta, q10) -> 
  total_policy_cost_runs
  
# Add the filter information. 
total_policy_rslts %>% 
  left_join(total_policy_cost_runs, by = 'run_name') -> 
  total_policy_classified

# Make a nice table 
total_policy_classified %>% 
  select(variable, filter_name, value, s, diff, beta, q10, run_name) %>%  
  arrange(variable, filter_name, value) %>% 
  write.csv(file = file.path(OUTPUT_DIR, '10.total_cost_table.csv'), row.names = FALSE)

# Total Cost Plots 
total_policy_classified %>%
  mutate(filter_name = if_else(filter_name == "Tgav, NPP, atmCO2 multi optimized", "Combined Obs. Filter", 'Temp Filter')) %>% 
  mutate(filter_name = factor(`filter_name`, levels = c('Temp Filter', "Combined Obs. Filter"), ordered = TRUE)) %>% 
  filter(variable == 'discountedCost') %>% 
  mutate(value = value / 1e13) %>% 
  ggplot(aes(filter_name, value, color = filter_name)) + 
  geom_line(size = 35) + 
 coord_cartesian(ylim = c(0, 9)) + 
  scale_color_manual(values = COLOR_THEME) + 
  UNIVERSAL_THEME +
  theme(legend.position = 'none') + 
  labs(y = expression('Trillion 2015 USD$'), 
       x = NULL) -> 
  total_cost_bar 

ggsave(total_cost_bar, file = file.path(OUTPUT_DIR, '10A.total_cost.png'), width = PRINT_WIDTH, height = PRINT_HEIGHT)

total_default_rslts %>% 
  filter(variable == 'discountedCost') %>% 
  mutate(value = value / 1e13) -> 
  total_cost_default

# With the CMS flux data 
# First figure the filter_name info for the hector-SA-npar runs. 
gcam_cmsflux$`CO2 prices` %>% 
  filter(year == 2050) %>% 
  group_by(filter_name) %>% 
  filter(value == min(value) | value == max(value)) %>%  
  ungroup %>% 
  select(run_name, filter_name, keep, s, diff, beta, q10) -> 
  total_policy_cost_runs_cms

# Add the filter information. 
total_cmsFlux_rslts %>% 
  left_join(total_policy_cost_runs_cms, by = 'run_name') %>% 
  bind_rows(total_policy_classified) -> 
  total_policy_classified

# Make a nice table 
total_policy_classified %>% 
  select(variable, filter_name, value, s, diff, beta, q10, run_name) %>%  
  arrange(variable, filter_name, value) %>% 
  write.csv(file = file.path(OUTPUT_DIR, '10.total_cost_table.csv'), row.names = FALSE)

## The End 
  

### 11. Observation Summary Information 


