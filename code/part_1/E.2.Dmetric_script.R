# Purpose: Calculate the Dn statistics for each Hector run and the Dc cut off value. 
# This script requires functions defined in E.0.Dmetric_functions. This calculates the 
# Dn metric scores for single and multiple variables. 

# TODO need to annotate this better and clean it up so that it makes more sense! 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.


# 0. Set Up ------------------------------------------------------------------------
# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE  <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_4_params'
}

# Informative messages
script_name <- 'E.1.Dmetric_script.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)

# Define the output directory
OUTPUT_DIR <- file.path(BASE, 'out-1', 'AGU'); dir.create(OUTPUT_DIR)

# Load the D metric functions, (Dn_func, Dc_func)
source(file.path(BASE, 'code', 'part_1', 'E.0.Dmetric_functions.R'))


# 1. Temperature Only ---------------------------------------------------------------
# Calcualte the Dn and the Dc metrics to compare Hector Tgav with the observational 
# temperature record. 

# Import the temperature data. 
Tgav_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.Tgav_Dmetric_input_table.csv'), 
                          stringsAsFactors = FALSE ) 

# Calculate the Dn value for each Hector run 
split(Tgav_Dn_input, Tgav_Dn_input$run_name) %>% 
  map_dfr(Dn_func) -> 
  Tgav_Dn_values

# Subset the Dn input so that it only includes entries for one Hector run, so that 
# is only contains one set of observational values. 
Tgav_Dn_input %>% 
  select(obs, s2n, sigma2) %>% 
  distinct %>% 
  Dc_func(alpha = 0.05) %>% 
  mutate(filter_name = 'Tgav') ->
  Tgav_Dc

Tgav_Dmetric_results <- join_Dmetric(Tgav_Dn_values, Tgav_Dc)


# 2. Atmospheric CO2 Only ---------------------------------------------------------------
# Calcualte the Dn and the Dc metrics to compare Hector Ca with the observational 
# atmospheric CO2 record. 

# Import the temperature data. 
atmCO2_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.atmCO2_Dmetric_input_table.csv'), 
                            stringsAsFactors = FALSE ) 

# Calculate the Dn value for each Hector run 
split(atmCO2_Dn_input, atmCO2_Dn_input$run_name) %>% 
  map_dfr( Dn_func ) -> 
  atmCO2_Dn_values


# Subset the Dn input so that it only includes entries for one Hector run, so that 
# is only contains one set of observational values. 
atmCO2_Dn_input %>% 
  select(year, obs, s2n, sigma2) %>% 
  distinct %>% 
  Dc_func(alpha = 0.05) %>% 
  mutate(filter_name = 'atm CO2') ->
  atmCO2_Dc


atmCO2_Dmetric_results <- join_Dmetric(atmCO2_Dc, atmCO2_Dn_values) 


# 3. LandFlux CO2 Only ---------------------------------------------------------------
# Calcualte the Dn and the Dc metrics to compare Hector land flux with the observations
# from the global carbon project data.

# Import the temperature data.
LandFlux_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.LandFlux_Dmetric_input_table.csv'),
                              stringsAsFactors = FALSE )

# Calculate the Dn value for each Hector run
split(LandFlux_Dn_input, LandFlux_Dn_input$run_name) %>%
  map_dfr( Dn_func ) ->
  LandFlux_Dn_values

# Subset the Dn input so that it only includes entries for one Hector run, so that
# is only contains one set of observational values.
LandFlux_Dn_input %>%
  select(year, obs, s2n, sigma2) %>%
  distinct %>% 
  Dc_func(alpha = 0.05) %>%
  mutate(filter_name = 'Land Flux') ->
  LandFlux_Dc

LandFlux_Dmetric_results <- join_Dmetric(LandFlux_Dc, LandFlux_Dn_values)


# 4. NPP Only ---------------------------------------------------------------------------

# Import the temperature data. 
NPP_Dn_input <- read.csv(file.path(BASE, 'out-1', sub_dir, 'D.NPP_Dmetric_input_table.csv'), 
                         stringsAsFactors = FALSE ) 

# Calculate the Dn value for each Hector run 
split(NPP_Dn_input, NPP_Dn_input$run_name) %>% 
  map_dfr( Dn_func ) -> 
  NPP_Dn_values

# Subset the Dn input so that it only includes entries for one Hector run, so that 
# is only contains one set of observational values. 
NPP_Dn_input %>% 
  select(year, obs, s2n, sigma2) %>% 
  distinct %>% 
  Dc_func(alpha = 0.05) %>% 
  mutate(filter_name = 'NPP') ->
  NPP_Dc

NPP_Dmetric_results <- join_Dmetric(NPP_Dn_values, NPP_Dc)


# Can we do multiple variables at once ----------------------------------

bind_rows(NPP_Dn_input,
          Tgav_Dn_input, 
          atmCO2_Dn_input) -> 
  NPP_Tgav_atmCO2_input 

NPP_Tgav_atmCO2_input %>% 
  split(.$run_name) %>% 
  map_dfr( Dn_func ) -> 
  NPP_Tgav_atmCO2_Dn

NPP_Tgav_atmCO2_input %>% 
  select(obs, s2n, sigma2) %>%
  distinct %>% 
  Dc_func(alpha = 0.05) %>% 
  mutate(filter_name = 'Tgav, NPP, atmCO2 multi optimized') -> 
  NPP_Tgav_atmCO2_Dc

atmCO2_NPP_Tgav_Dmetric_results <- join_Dmetric(NPP_Tgav_atmCO2_Dn, NPP_Tgav_atmCO2_Dc)



# 5. Save Outputs ------------------------------------------------------------------------

# write.csv(Tgav_Dmetric_results, file = file.path(OUTPUT_DIR, 'E.Tgav_Dmetric_results'), row.names = FALSE)
# write.csv(atmCO2_Dmetric_results, file = file.path(OUTPUT_DIR, 'E.atmCO2_Dmetric_results'), row.names = FALSE)
# write.csv(LandFlux_Dmetric_results, file = file.path(OUTPUT_DIR, 'E.LandFlux_Dmetric_results'), row.names = FALSE)

# Save as a large file
Dmetric_results <- bind_rows(Tgav_Dmetric_results, 
                             atmCO2_Dmetric_results, 
                             NPP_Dmetric_results, 
                             atmCO2_NPP_Tgav_Dmetric_results)
write.csv(Dmetric_results, file = file.path(OUTPUT_DIR, 'E.Dn_metric_results.csv'), row.names = FALSE)

message(seperator)


