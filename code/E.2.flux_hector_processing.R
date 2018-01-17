library( 'tidyr' )
library( 'caTools' )

# ----------------------------------------------------------------
# Settings you will definitely need to overwrite in your code
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

# ---
# 1. read in processed observation
observation <- read.csv( './int-out/E.CDIAC_obervation_ma.csv', stringsAsFactors = F )
observation_years <- sort( unique( observation$year ) ) 

# --- 
# 2. read in hector land and ocean flux 
hector_res <- read.csv( './int-out/C.hector_run_cleanup.csv', stringsAsFactors = F )

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
  run_ts_ma <- runmean( run_ts, 7,
                        alg = c( 'C' ), 
                        endrule = c( 'mean' ), 
                        align = c( 'center' ) )  
  out_df <- data.frame( t( c( run_name, var, run_ts_ma ) ) )
  colnames( out_df ) <- c( 'run_name', 'variable', paste0( 'X', observation_years ) )
  return( out_df )
} )
flux_crop_ma <- do.call( 'rbind', flux_ma_res_list )

write.csv( flux_crop_ma, './int-out/E.flux_hector_ma.csv', row.names = F )


