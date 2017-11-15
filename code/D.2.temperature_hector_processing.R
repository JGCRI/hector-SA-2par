library( 'tidyr' )
library( 'smooth' )
library( 'forecast' )
library( 'Mcomp' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

# ---
mav <- function( x, n ) { stats::filter( x, rep( 1 / n, n ), sides = 2 ) }

# ---
# 1. read in processed observation
observation <- read.csv( './int-out/D.temperature_obervation_ma.csv', stringsAsFactors = F )
observation_years <- sort( unique( observation$year ) ) 

# --- 
# 2. read in hector Tgav and calculate standarized Tgav
hector_res <- read.csv( './int-out/C.hector_run_cleanup.csv', stringsAsFactors = F )
hector_tgav <- hector_res[ hector_res$variable == 'Tgav', ]
tgav_crop <- hector_tgav[ , c( 'run_name', 'variable', 'units', paste0( 'X', observation_years ) ) ]
tgav_crop$ref_mean <- apply( tgav_crop[ , c( paste0( 'X', 1951 : 1990 ) ) ], 1, mean )
tgav_crop[ , c( paste0( 'X', observation_years ) ) ] <- tgav_crop[ , c( paste0( 'X', observation_years ) ) ] -  tgav_crop$ref_mean
tgav_crop$ref_mean <- NULL

# ---
# 3. experiment on tgav_crop -- moving average for each hector run 
tgav_ma_res_list <- lapply( 1 : nrow( tgav_crop ), function( i ) { 
  #print( i )
  temp_line <- tgav_crop[ i, ]
  run_name <- temp_line$run_name 
  run_ts <- unlist( temp_line[ , paste0( 'X', observation_years ) ] )
  run_ts_ma <- mav( run_ts, 15 ) 
  out_df <- data.frame( t( c( run_name, run_ts_ma ) ) )
  colnames( out_df ) <- c( 'run_name', paste0( 'X', observation_years ) )
  return( out_df )
} )
tgav_crop_ma <- do.call( 'rbind', tgav_ma_res_list )

write.csv( tgav_crop_ma, './int-out/D.temperature_hector_ma.csv', row.names = F )


