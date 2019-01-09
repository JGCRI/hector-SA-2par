# Purpose: Add the Dn filtering results to the filter_flag.csv file. 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.


# 0. Set Up ------------------------------------------------------------------------
library(dplyr)

# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'AGU'
}

script_name <- 'F.Dn_filter_flag.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)


OUTPUT_DIR <- file.path(BASE, 'out-1', 'AGU')


# 1. Input Data ------------------------------------------------------------------------

# Load the csv containing the csv of Dn metric results. 
#Dn_tibble <- read.csv(file.path(BASE, 'out-1', sub_dir, 'E.all_Dmetric_independent_results.csv' ), stringsAsFactors = FALSE)
Dn_tibble <- read.csv(file.path(BASE, 'out-1', sub_dir, 'E.Dn_metric_results.csv'), stringsAsFactors = FALSE)

# Load the filter flag csv, the results from the Dn filtering will go here. 
filter_flag <- readr::read_csv(file.path(BASE, 'out-1', sub_dir, 'filter_flag.csv' ))

# 2. Filter Hector Runs ------------------------------------------------------------------------

# Start by determining which runw pass through the filters indiviudally.
Dn_tibble %>% 
  mutate(pass = if_else(Dn <= Dc, TRUE, FALSE)) %>% 
  select(run_name, filter_name, pass) %>% 
  spread(filter_name, pass) -> 
  Dn_passing

# Add a column for the filters that pass through no filters. 
Dn_passing$None <- TRUE 

# Add a column for the filters that pass through "atm CO2, NPP"
Dn_passing$`atm CO2, NPP` <- c(Dn_passing$NPP == TRUE & Dn_passing$`atm CO2` == TRUE)

# Add a column for the filters that pass through "atm CO2, Tgav"
Dn_passing$`atm CO2, Tgav` <- c(Dn_passing$Tgav == TRUE & Dn_passing$`atm CO2` == TRUE)

# Add a column for the filters that pass through "atm CO2, NPP, Tgav"
Dn_passing$`atm CO2, NPP, Tgav` <- c(Dn_passing$NPP == TRUE & Dn_passing$Tgav == TRUE & Dn_passing$`atm CO2` == TRUE)




# 3. Save Outputs ------------------------------------------------------------------------

filter_flag %>% 
  select(c( 'run_name', names(.)[!names(.) %in% names(Dn_passing)])) %>% 
  left_join(Dn_passing, by = 'run_name') -> 
  filter_flag_out 

write.csv(filter_flag_out, file = file.path(BASE, 'out-1', sub_dir, 'filter_flag.csv'), 
          row.names = FALSE)

message(seperator)
