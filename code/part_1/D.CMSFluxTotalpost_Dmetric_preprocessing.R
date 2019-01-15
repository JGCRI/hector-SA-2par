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
obs_data    <- read.csv(file.path(BASE, 'input', 'observations', 'CMSFluxTotalpost.csv'))
hector_data <- read.csv(list.files(INPUT_DIR, 'C.atmos_c_hector_run_cleanup.csv', full.names = TRUE)) %>% 
  filter(year %in% obs_data$time) 


ggplot() + 
  geom_line(data = hector_data, aes(year, value, color = 'Hector Runs', group = run_name), 
            alpha = 0.5) + 
  geom_point(data = obs_data,
             aes(time, value, color = 'obs value'), 
             size = 3) + 
  geom_point(data = obs_data,
             aes(time, uncertainty, color = 'obs uncertainty value'), 
             size = 3) + 
  theme_bw() + 
  labs(x = 'year', 
       y = 'Pg', 
       title = 'Comparison of Hector and CMS obs Flux')

  



