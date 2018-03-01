library( 'tidyr' )
library( 'caTools' )

# 0. Set Up ----------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Define the rcp to process
rcpXX <- "rcp26"

# Define the length of years to use in the moving average window
windowYrs <- 15 


# ---
# 1. read in processed observation
observation <- read.csv( './int-out/observations/E.CDIAC_obervation_ma.csv', stringsAsFactors = F )
observation_years <- sort( unique( observation$year ) ) 

# --- 
# 2. read in hector land and ocean flux 
hector_res <- read.csv( file.path('./int-out', rcpXX, 'C.hector_run_cleanup.csv'), stringsAsFactors = F )

hector_land_ocean_flux <- hector_res[ hector_res$variable %in% c( 'atm_land_flux', 'atm_ocean_flux' ), ]
flux_crop <- hector_land_ocean_flux[ , c( 'run_name', 'variable', 'units', paste0( 'X', observation_years ) ) ]

# ---
# 3. experiment on tgav_crop -- moving average for each hector run 
flux_ma_res_list <- lapply( 1 : nrow( flux_crop ), function( i ) { 
  #print( i )
  temp_line <- flux_crop[ i, ]
  var <- temp_line$variable
  run_name <- temp_line$run_name 
  run_ts <- unlist( temp_line[ , paste0( 'X', observation_years ) ] )
  run_ts_ma <- runmean( run_ts, windowYrs,
                        alg = c( 'C' ), 
                        endrule = c( 'mean' ), 
                        align = c( 'center' ) )  
  out_df <- data.frame( t( c( run_name, var, run_ts_ma ) ) )
  colnames( out_df ) <- c( 'run_name', 'variable', paste0( 'X', observation_years ) )
  return( out_df )
} )
flux_crop_ma <- do.call( 'rbind', flux_ma_res_list )

write.csv( flux_crop_ma, file.path('./int-out', rcpXX, 'E.flux_hector_ma.csv'), row.names = F )


