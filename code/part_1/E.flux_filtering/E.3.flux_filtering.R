library( 'tidyr' )

# This section is commented out so that the script can be sourced from the 
# run_all script. If you would like to run this script by it's self make sure that 
# the rest of the code in the set up section is not commented out.
# # 0. Set Up ----------------------------------------------------------------
# # The working directory should be the project directory. 
# if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}
# 
# # Define the rcp to process
# rcpXX <- 'rcp26'
# 
# # The percent of hector years that must fall within the observation range in order for the 
# # run to count as passing. 
# fall_in_threshold <- .70

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


fall_in_count <- floor( length( filter_year_list ) * fall_in_threshold ) 

total_run_names <- unlist( filter_year_res_list )
freq_table <- as.data.frame( sort( table( total_run_names ), decreasing = T ), stringsAsFactors = F ) 
#max_oc <- max( freq_table$Freq )
max_oc <- unique( freq_table$Freq )[ which( unique( freq_table$Freq ) > fall_in_count ) ] 

land_selected_run_names <- freq_table[ freq_table$Freq %in% max_oc, 'total_run_names' ]


landFlux_selected <- hector_flux_land[hector_flux_land$run_name %in% land_selected_run_names , ]
write.csv( landFlux_selected, file.path('./int-out', rcpXX, 'E.hector_landFlux_filtered.csv'), row.names = F )



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


