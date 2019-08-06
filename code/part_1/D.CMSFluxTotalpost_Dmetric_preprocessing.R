# Purpose: Use the CMS Total Posterior Flux data processed from monthly gridded values to annual global 
# values as points of comparison with Hector output with the Dn values. 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.

# 0. Set Up -------------------------------------------------------------------------------------------------------------------------------
# If sourcing the script as part of the run all script then then the directories will be defined 
# by the the settings in the run all script.
if(!exists('run_all')){
  
  BASE     <- getwd()
  sub_dir  <- 'hist_emissions'
  
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  stopifnot(dir.exists(file.path(BASE, 'output', sub_dir)))
  

}


# Get the sigma function
source(file.path(BASE, 'code', 'part_1', 'D.0.Dmetric_preprocessing_functions.R')) # The simga2 function

script_name <- 'D.CMSFluxTotalpost_Dnmetric_preprocessing.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('output/', sub_dir, appendLF = T)

# Directories 
INPUT_DIR <- file.path(BASE, 'output', 'out-1', sub_dir)

# 1. Import Data ----------------------------------------------------------------------------------------------------------------------------

# Import the observational data 
obs_data    <- read.csv(file.path(BASE, 'input', 'observations', 'CMSFluxTotalpost.csv')) %>% 
  mutate(value = value / 12) # Because Edil said so
hector_data <- read.csv(list.files(INPUT_DIR, 'C.atmos_c_hector_run_cleanup.csv', full.names = TRUE)) %>% 
  filter(year %in% c(min(obs_data$time) - 1, obs_data$time, max(obs_data$time) + 1))



# 2. Process Hector Data ---------------------------------------------------------------------------------------------------------------
# Convert the Hector atmospheric carbon data to a flux value (change in years). 
split(hector_data, interaction(hector_data$run_name, hector_data$variable, hector_data$units), drop = TRUE) %>%  
  lapply(function(input){
    
    input %>% 
      arrange(run_name, year) %>%  
      pull(value) %>% 
      diff -> 
      diff
    
    input$value <- c(NA, diff) 
    
    input
  }) %>% 
  bind_rows() %>% 
  na.omit %>%
  select(run_name, year, model = value) -> 
  hector_data



# 3. Format the Dn output data ---------------------------------------------------------------------------------------------------------------

obs_data$min <- min(obs_data$value)
obs_data$max <- max(obs_data$value)

hector_data %>%  
  left_join(obs_data %>%  
              select(year = time, obs_min = min, obs_max = max), by = 'year') %>% 
  na.omit -> 
  output_table


output_file <- file.path(BASE, 'output', 'out-1', sub_dir, 'D.CMSFlux_comparison_table.csv')
write.csv(output_table, output_file, row.names = F)

message(seperator)







