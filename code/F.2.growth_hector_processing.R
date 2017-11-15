library( 'tidyr' )
library( 'smooth' )
library( 'Mcomp' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

# ---
mav <- function( x, n ) { stats::filter( x, rep( 1 / n, n ), sides = 2 ) }

# ---
# 1. read in processed observation
observation <- read.csv( './int-out/F.CDIAC_growth_ma.csv', stringsAsFactors = F )
observation_years <- sort( unique( observation$year ) ) 

# --- 
# 2. read in hector atmos_c and compute the growth 
hector_res <- read.csv( './int-out/C.hector_run_cleanup.csv', stringsAsFactors = F )

hector_atmosc <- hector_res[ hector_res$variable == 'atmos_c', ]
x_years <- grep( 'X', names( hector_atmosc ), value = T )
hector_growth <- hector_atmosc[ , x_years[ 2 : length( x_years ) ] ] - hector_atmosc[ , x_years[ 1 : ( length( x_years ) - 1 ) ] ]
hector_growth <- cbind( hector_atmosc[ , c( "run_name", "variable", "units" ) ], hector_growth )

growth_crop <- hector_growth[ , c( 'run_name', 'variable', 'units', paste0( 'X', observation_years ) ) ]

# ---
# 3. experiment on previously filtered runs -- moving average for each hector run 
growth_ma_res_list <- lapply( 1 : nrow( growth_crop ), function( i ) { 
  #print( i )
  temp_line <- growth_crop[ i, ]
  var <- temp_line$variable
  run_name <- temp_line$run_name 
  run_ts <- unlist( temp_line[ , paste0( 'X', observation_years ) ] )
  run_ts_ma <- mav( run_ts, 7 ) 
  out_df <- data.frame( t( c( run_name, var, run_ts_ma ) ) )
  colnames( out_df ) <- c( 'run_name', 'variable', paste0( 'X', observation_years ) )
  return( out_df )
} )
growth_crop_ma <- do.call( 'rbind', growth_ma_res_list )

write.csv( growth_crop_ma, './int-out/F.growth_hector_ma.csv', row.names = F )


