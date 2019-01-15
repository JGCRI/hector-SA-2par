# Purpose:This script read through all output files and sparate into different data 
# frames by variable. This script also saves each variable indiviually in wide format 
# to minimze the read in time in the filtering steps. 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.

# 0. Set Up -------------------------------------------------------------------------
library( 'tidyr' )
library( 'readr' )
library( 'purrr' )

# Define directories
if(!exists('run_all')){
  
  # Base directory 
  if(!"hector-SA-npar.Rproj" %in% list.files()){stop('working dir must be the project location')}
  
  BASE <- getwd()
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_q10_only'
  
  }

script_name <- 'E.1.Dmetric_independent_script.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('pulling/saving data from out/', sub_dir, appendLF = T)


# Select the variables to save as individual files. 
keep_variables <- c('atm_land_flux', 'Ca', 'Tgav', 'npp', 'atmos_c')

# --------------------------------------------------
# The working directory should be the project directory. 

raw_output <- read.csv( file.path('./out-1', sub_dir, 'B.hector_run_results.txt'), header = F, stringsAsFactors = F )

# If one of the rows contains columns names by mistatke remove the row(s) containg the 
# column names. There should not be any letter Vs in the year column.
if( any(grepl('V', raw_output$V1)) ){
  raw_output <- raw_output[!grepl('V', raw_output$V1), ]
}

names( raw_output ) <- c( 'year', 'run_name', 'spinup', 'component', 'variable', 'value', 'units' )

cleanup_df <- raw_output 
rm( raw_output ) 

cleanup_df$spinup <- NULL 
cleanup_df$component <- NULL

vars <- sort( unique( cleanup_df$variable ) ) 

cleanup_wide <- spread( cleanup_df, year, value )

write.csv( cleanup_wide, file.path('./out-1', sub_dir, 'C.hector_run_cleanup.csv'), row.names = F )

cleanup_wide <- read.csv(file.path('./out-1', sub_dir, 'C.hector_run_cleanup.csv'))

# ---
# Filter flag table
filter_flag <- read.csv( file.path('./output/out-1', sub_dir, 'A.par4_combinations.csv'), stringsAsFactors = F )
filter_flag$run_name <- paste0( 'hectorSA-', sprintf( '%04d', filter_flag$run_index ) )
write.csv( filter_flag, file.path('./output/out-1', sub_dir, 'filter_flag.csv'), row.names = F )


# --------------------------------------------------
# Format into the long format 
cleanup_wide %>% 
  filter(variable %in% keep_variables) %>% 
#  select(-spinup, -component) %>% 
  gather(year, value, -run_name, -variable, -units) %>% 
  filter(year != 'spinup') %>% 
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
  write.csv(data, file.path('./output/out-1', sub_dir, f_name), row.names = F ) })

message(seperator)

if(!exists('run_all')){
  rm(list=setdiff(ls(), c("run_all", "BASE", "sub_dir")))
  }
