# This script read through all output files and sparate into different data frames by variable 
library( 'tidyr' )
library( 'readr' )

# Define the rcp to process
rcpXX <- 'rcp26'

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
# Filter flga table
filter_flag <- read.csv( file.path('./int-out/A.par4_combinations.csv'), stringsAsFactors = F )
filter_flag$run_name <- paste0( 'hectorSA-', sprintf( '%04d', filter_flag$run_index ) )
write.csv( filter_flag, file.path('./int-out', rcpXX, 'filter_flag.csv'), row.names = F )


