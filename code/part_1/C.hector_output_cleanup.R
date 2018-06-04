# This script read through all output files and sparate into different data frames by variable. This script 
# also saves each variable indiviually in wide format to minimze the read in time in the filtering steps. 
library( 'tidyr' )
library( 'readr' )
library( 'map' )

# Define the rcp to process
rcpXX <- 'rcp26'

# Select the variables to save as individual files. 
keep_variables <- c('atm_land_flux', 'Ca', 'Tgav', 'npp')

# --------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

raw_output <- read.csv( file.path('./int-out', rcpXX, 'B.hector_run_results.txt'), header = F, stringsAsFactors = F )

names( raw_output ) <- c( 'year', 'run_name', 'spinup', 'component', 'variable', 'value', 'units' )

cleanup_df <- raw_output 
rm( raw_output ) 

cleanup_df$spinup <- NULL 
cleanup_df$component <- NULL

vars <- sort( unique( cleanup_df$variable ) ) 

cleanup_wide <- spread( cleanup_df, year, value )

write.csv( cleanup_wide, file.path('./int-out', rcpXX, 'C.hector_run_cleanup.csv'), row.names = F )


# ---
# Filter flag table
filter_flag <- read.csv( file.path('./int-out/A.par4_combinations.csv'), stringsAsFactors = F )
filter_flag$run_name <- paste0( 'hectorSA-', sprintf( '%04d', filter_flag$run_index ) )
write.csv( filter_flag, file.path('./int-out', rcpXX, 'filter_flag.csv'), row.names = F )


# --------------------------------------------------
# Format into the long format 
cleanup_wide %>% 
  filter(variable %in% keep_variables) %>% 
  gather(year, value, -run_name, -variable, -units) %>% 
  mutate(year = as.integer(gsub('X', '', year))) -> 
  long_format 

# Separate into data sets by variable name
data_list <- split(long_format, long_format$variable)

# Save each variable as a csv file
map(data_list, function(data){
  
  # Save the variable name to use in the file name
  vari   <- unique(data$variable)
  f_name <- paste0('C.', vari, '_hector_run_cleanup.csv')
  
  # Write the data as a csv output.
  write.csv(data, file.path('./int-out', rcpXX, f_name), row.names = F ) })


