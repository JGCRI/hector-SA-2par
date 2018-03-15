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
# # The percent of Hecotr run years that must fall within the observation range in order
# # for the run to pass the filter.
# fall_in_threshold <- .50


# ---
# 1. read in observations and hector Tgav 
observations <- read.csv( './int-out/observations/F.CDIAC_growth_ma.csv', stringsAsFactors = F ) 
hector_growth <- read.csv( file.path('./int-out', rcpXX, 'F.growth_hector_ma.csv'), stringsAsFactors = F )

# ---
# 2. fall in test
filter_year_list <- sort( unique( observations$year ) ) 
observation_growth <- observations[ , c( 'year', 'growth_min', 'growth_max' ) ]
observation_growth$min <- observation_growth$growth_min 
observation_growth$max <- observation_growth$growth_max 
observation_growth$growth_min <- NULL
observation_growth$growth_max <- NULL

filter_year_res_list <- lapply( filter_year_list, function( year ) { 
  year_min <- unlist( observation_growth[ observation_growth$year == year, 'min' ] )
  year_max <- unlist( observation_growth[ observation_growth$year == year, 'max' ] )
  selected_run_names <- hector_growth$run_name[ which( hector_growth[ , paste0( 'X', year ) ] <= year_max & 
                                                            hector_growth[ , paste0( 'X', year ) ] >= year_min ) ]
  return( selected_run_names )
} )


fall_in_count <- floor( length( filter_year_list ) * fall_in_threshold ) 

total_run_names <- unlist( filter_year_res_list )
freq_table <- as.data.frame( sort( table( total_run_names ), decreasing = T ), stringsAsFactors = F ) 
#max_oc <- max( freq_table$Freq )
max_oc <- unique( freq_table$Freq )[ which( unique( freq_table$Freq ) > fall_in_count ) ] 

growth_selected_run_names <- freq_table[ freq_table$Freq %in% max_oc, 'total_run_names' ]

growth_selected <- hector_growth[hector_growth$run_name %in% growth_selected_run_names , ]
write.csv( growth_selected, file.path('./int-out', rcpXX, 'F.hector_growth_filtered.csv'), row.names = F )


# ---
# 3. update filter_flag 
filter_flag <- read.csv( file.path('./int-out', rcpXX, 'filter_flag.csv'), stringsAsFactors = F )
filter_flag$growth_flag <- 0  
filter_flag$growth_flag <- ifelse( filter_flag$run_name %in% growth_selected_run_names, 1, 0 )
write.csv( filter_flag, file.path('./int-out', rcpXX, 'filter_flag.csv'), row.names = F )


