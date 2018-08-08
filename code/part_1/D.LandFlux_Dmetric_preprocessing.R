# Purpose: Use land flux data from the 2016 Global Carbon Budget as the observational points of 
# comparison to calculate the Dn metric. The Dn calculations will need a data frame containing
# hector values, observational values and an observational data variability value.

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.


# 0. Set Up ------------------------------------------------------------------------
library( 'readxl' )

# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_q10_only'
  
}

script_name <- 'D.LandFlux_Dmetric_preprocessing.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)

INT_OUTPUT_DIR <- file.path(BASE, 'out-1', sub_dir)


# 1. Import and Format Obs Data -----------------------------------------------------

# Import the data 
ob_df          <- read_excel( './input/observations/Global_Carbon_Budget_2016_v1.0.xlsx', sheet = "Global Carbon Budget", skip = 21 )
names( ob_df ) <- tolower( names( ob_df ) ) 
ob_df          <- ob_df[ , c( 'year', 'land sink' ) ]
names( ob_df ) <- gsub( ' ', '_', names( ob_df ) )
#ob_df$unit    <- 'Gt C'# 1 Gt C = 1 Pg C (Pg used in hector)

# According to the documentation in the execl book 
# "The land sink (uncertainty of ±0.8 GtC/yr on average) was estimated from the residual of the other budget terms: 
# land_sink = fossil_fuel + land_use_change - atm_growth - ocean_sink."  

# Add the s2n column to the observational data frame recall s2n = sd ^ 2
ob_df$s2n <- 0.8 ^ 2


# Rename the observational data frame 
names(ob_df) <- c("year", "obs", "s2n")



# 2. Import and Format Hector Data --------------------------------------------------
hector_land_flux_path <- list.files(INT_OUTPUT_DIR, "C.atm_land_flux_hector_run_cleanup.csv", full.names = T)
hector_data           <- read.csv(hector_land_flux_path, stringsAsFactors = FALSE)


# Subset the Hector data frame so that it only includes the atm land flux data 
# for the same years as the observational data. 
hector_data %>% 
  filter(variable == "atm_land_flux", 
         year %in% ob_df$year) %>% 
  rename(model = value) -> 
  hector_data


# 3. Prepare the Dn Input Data Frame -----------------------------------------------------------

hector_data %>%  
  full_join(ob_df %>% select(year, obs, s2n), by = "year") -> 
  LandFlux_Dn_input_table

output_file <- file.path(BASE, 'out-1', sub_dir, 'D.LandFlux_Dmetric_input_table.csv')
write.csv(LandFlux_Dn_input_table, output_file, row.names = F)

message(seperator)

