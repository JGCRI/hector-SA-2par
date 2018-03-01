library( 'tidyr' )

# ----------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

# Define the rcp to process
rcpXX <- 'rcp26'

# ---
# 1. read in observations and hector Tgav 
observations <- read.csv( './int-out/observations/E.CDIAC_obervation_ma.csv', stringsAsFactors = F ) 
hector_flux <- read.csv( file.path('./int-out', rcpXX, 'E.flux_hector_ma.csv'), stringsAsFactors = F )

# ---
# 2. fall in test
# 2.1 land flux 
filter_year_list <- sort( unique( observations$year ) ) 
observation_land <- observations[ , c( 'year', 'land_sink_min', 'land_sink_max' ) ]
observation_land$min <- observation_land$land_sink_min 
observation_land$max <- observation_land$land_sink_max 
observation_land$land_sink_min <- NULL
observation_land$land_sink_max <- NULL

hector_flux_land <- hector_flux[ hector_flux$variable == 'atm_land_flux', ]

filter_year_res_list <- lapply( filter_year_list, function( year ) { 
  year_min <- unlist( observation_land[ observation_land$year == year, 'min' ] )
  year_max <- unlist( observation_land[ observation_land$year == year, 'max' ] )
  selected_run_names <- hector_flux_land$run_name[ which( hector_flux_land[ , paste0( 'X', year ) ] <= year_max & 
                                                            hector_flux_land[ , paste0( 'X', year ) ] >= year_min ) ]
  return( selected_run_names )
} )

fall_in_threshold <- .70
fall_in_count <- floor( length( filter_year_list ) * fall_in_threshold ) 

total_run_names <- unlist( filter_year_res_list )
freq_table <- as.data.frame( sort( table( total_run_names ), decreasing = T ), stringsAsFactors = F ) 
#max_oc <- max( freq_table$Freq )
max_oc <- unique( freq_table$Freq )[ which( unique( freq_table$Freq ) > fall_in_count ) ] 

land_selected_run_names <- freq_table[ freq_table$Freq %in% max_oc, 'total_run_names' ]

# # 2.2 ocean flux 
# filter_year_list <- sort( unique( observations$year ) ) 
# observation_ocean <- observations[ , c( 'year', 'ocean_sink_min', 'ocean_sink_max' ) ]
# observation_ocean$min <- observation_ocean$ocean_sink_min 
# observation_ocean$max <- observation_ocean$ocean_sink_max 
# observation_ocean$ocean_sink_min <- NULL
# observation_ocean$ocean_sink_max <- NULL
# 
# hector_flux_ocean <- hector_flux[ hector_flux$variable == 'atm_ocean_flux', ]
# 
# filter_year_res_list <- lapply( filter_year_list, function( year ) { 
#   year_min <- unlist( observation_ocean[ observation_ocean$year == year, 'min' ] )
#   year_max <- unlist( observation_ocean[ observation_ocean$year == year, 'max' ] )
#   selected_run_names <- hector_flux_ocean$run_name[ which( hector_flux_ocean[ , paste0( 'X', year ) ] <= year_max & 
#                                                             hector_flux_ocean[ , paste0( 'X', year ) ] >= year_min ) ]
#   return( selected_run_names )
# } )
# 
# fall_in_threshold <- .70
# fall_in_count <- floor( length( filter_year_list ) * fall_in_threshold ) 
# 
# total_run_names <- unlist( filter_year_res_list )
# freq_table <- as.data.frame( sort( table( total_run_names ), decreasing = T ), stringsAsFactors = F ) 
# #max_oc <- max( freq_table$Freq )
# max_oc <- unique( freq_table$Freq )[ which( unique( freq_table$Freq ) > fall_in_count ) ] 
# 
# ocean_selected_run_names <- freq_table[ freq_table$Freq %in% max_oc, 'total_run_names' ]

# ---
# 3. update filter_flag 
filter_flag <- read.csv( file.path('./int-out', rcpXX, 'filter_flag.csv'), stringsAsFactors = F )
filter_flag$landflux_flag <- 0  
filter_flag$landflux_flag <- ifelse( filter_flag$run_name %in% land_selected_run_names, 1, 0 )
write.csv( filter_flag, file.path('./int-out', rcpXX, 'filter_flag.csv'), row.names = F )


