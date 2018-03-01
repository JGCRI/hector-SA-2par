library( 'tidyr' )

# ----------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Define the rcp to process
rcpXX <- 'rcp26'

# ---
# 1. read in observations and hector Tgav 
observations <- read.csv( './int-out/observations/D.temperature_obervation_ma.csv', stringsAsFactors = F ) 
hector_tgav <- read.csv( file.path('./int-out', rcpXX, 'D.temperature_hector_ma.csv'), stringsAsFactors = F )


# ---
# 2. fall in test 
filter_year_list <- 1940 : 2010 
filter_year_res_list <- lapply( filter_year_list, function( year ) { 
  year_min <- unlist( observations[ observations$year == year, 'min' ] )
  year_max <- unlist( observations[ observations$year == year, 'max' ] )
  selected_run_names <- hector_tgav$run_name[ which( hector_tgav[ , paste0( 'X', year ) ] <= year_max & hector_tgav[ , paste0( 'X', year ) ] >= year_min ) ]
  return( selected_run_names )
} )

fall_in_threshold <- .70
fall_in_count <- floor( length( filter_year_list ) * fall_in_threshold ) 

total_run_names <- unlist( filter_year_res_list )
freq_table <- as.data.frame( sort( table( total_run_names ), decreasing = T ), stringsAsFactors = F ) 
#max_oc <- max( freq_table$Freq )
max_oc <- unique( freq_table$Freq )[ which( unique( freq_table$Freq ) > fall_in_count ) ] 

selected_run_names <- freq_table[ freq_table$Freq %in% max_oc, 'total_run_names' ]

# ---
# 3. Selected run preview 
tgav_selected <- hector_tgav[ hector_tgav$run_name %in% selected_run_names, ]
temp_df <- t( as.matrix( observations ) )
temp_df <- data.frame( temp_df[ c( 2, 3 ), ] )
temp_df$run_name <- c( 'min', 'max' )
colnames( temp_df )  <- c( paste0( 'X', 1850 : 2017 ), 'run_name' ) 
temp_df <- temp_df[ , c( 'run_name', paste0( 'X', 1850 : 2017 ) ) ]

tgav_selected <- rbind( tgav_selected, temp_df )
tgav_selected$fall_in_rate <- fall_in_threshold

write.csv( tgav_selected, file.path('./int-out', rcpXX, 'D.hector_tgav_filtered.csv'), row.names = F )

# ---
# 4. update filter flag table
filter_flag <- read.csv( file.path('./int-out', rcpXX, 'filter_flag.csv'), stringsAsFactors = F )
filter_flag$tempature_flag <- 0 
filter_flag$tempature_flag <- ifelse( filter_flag$run_name %in% selected_run_names, 1, 0 )
write.csv( filter_flag, file.path('./int-out', rcpXX, 'filter_flag.csv'), row.names = F )

