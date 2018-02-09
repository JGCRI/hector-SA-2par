library( 'tidyr' )
library( 'caTools' )

# ----------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Define the rcp to process
rcpXX <- 'rcp85' 

# ---
# 1. read in processed observation
observation <- read.csv( './int-out/observations/D.temperature_obervation_ma.csv', stringsAsFactors = F )
observation_years <- sort( unique( observation$year ) ) 

# --- 
# 2. read in hector Tgav and calculate standarized Tgav
hector_res <- read.csv( file.path('./int-out', rcpXX, 'C.hector_run_cleanup.csv'), stringsAsFactors = F )
hector_tgav <- hector_res[ hector_res$variable == 'Tgav', ]
tgav_crop <- hector_tgav[ , c( 'run_name', 'variable', 'units', paste0( 'X', observation_years ) ) ]
tgav_crop$ref_mean <- apply( tgav_crop[ , c( paste0( 'X', 1951 : 1990 ) ) ], 1, mean )
tgav_crop[ , c( paste0( 'X', observation_years ) ) ] <- tgav_crop[ , c( paste0( 'X', observation_years ) ) ] -  tgav_crop$ref_mean
tgav_crop$ref_mean <- NULL

# ---
# 3. experiment on tgav_crop -- moving average for each hector run 
tgav_ma_res_list <- lapply( 1 : nrow( tgav_crop ), function( i ) { 
  # print( i )
  temp_line <- tgav_crop[ i, ]
  run_name <- temp_line$run_name 
  run_ts <- unlist( temp_line[ , paste0( 'X', observation_years ) ] )
  run_ts_ma <- runmean( run_ts, 
                        15, 
                        alg = c( 'C' ), 
                        endrule = c( 'mean' ), 
                        align = c( 'center' ) )
  out_df <- data.frame( t( c( run_name, run_ts_ma ) ) )
  colnames( out_df ) <- c( 'run_name', paste0( 'X', observation_years ) )
  return( out_df )
} )
tgav_crop_ma <- do.call( 'rbind', tgav_ma_res_list )

write.csv( tgav_crop_ma, file.path('./int-out', rcpXX, 'D.temperature_hector_ma.csv'), row.names = F )


