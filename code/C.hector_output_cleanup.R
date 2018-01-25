# This script read through all output files and sparate into different data frames by variable 
library( 'tidyr' )
library( 'readr' )

# --------------------------------------------------
# Settings you will definitely need to overwrite in your code
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-npar' )

raw_output <- read.csv( './int-out/B.hector_run_results.txt', header = F, stringsAsFactors = F )

names( raw_output ) <- c( 'year', 'run_name', 'spinup', 'component', 'variable', 'value', 'units' )

cleanup_df <- raw_output 
rm( raw_output ) 

cleanup_df$spinup <- NULL 
cleanup_df$component <- NULL

vars <- sort( unique( cleanup_df$variable ) ) 

cleanup_wide <- spread( cleanup_df, year, value )

write.csv( cleanup_wide, './int-out/C.hector_run_cleanup.csv', row.names = F )


# ---
# Filter flga table
filter_flag <- read.csv( './int-out/A.par4_combinations.csv', stringsAsFactors = F )
filter_flag$run_name <- paste0( 'hectorSA-', sprintf( '%04d', filter_flag$run_index ) )
write.csv( filter_flag, './int-out/filter_flag.csv', row.names = F )


