# This script read through all output files and sparate into different data frames by variable 
library( 'tidyr' )
library( 'readr' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

raw_output <- read.csv( './int-out/B.hector_run_results.txt', header = F, stringsAsFactors = F )

names( raw_output ) <- c( 'year', 'run_name', 'spinup', 'component', 'variable', 'value', 'units' )

cleanup_df <- raw_output 
rm( raw_output ) 

cleanup_df$spinup <- NULL 
cleanup_df$component <- NULL

vars <- sort( unique( cleanup_df$variable ) ) 

cleanup_wide <- spread( cleanup_df, year, value )

write.csv( cleanup_wide, './int-out/C.hector_run_cleanup.csv', row.names = F )


