library( 'tidyr' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

# ---
# 1. read in observations and hector Tgav 
observations <- read.csv( './int-out/F.CDIAC_growth_ma.csv', stringsAsFactors = F ) 
hector_growth <- read.csv( './int-out/F.growth_hector_ma.csv', stringsAsFactors = F )

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

fall_in_threshold <- .50
fall_in_count <- floor( length( filter_year_list ) * fall_in_threshold ) 

total_run_names <- unlist( filter_year_res_list )
freq_table <- as.data.frame( sort( table( total_run_names ), decreasing = T ), stringsAsFactors = F ) 
#max_oc <- max( freq_table$Freq )
max_oc <- unique( freq_table$Freq )[ which( unique( freq_table$Freq ) > fall_in_count ) ] 

growth_selected_run_names <- freq_table[ freq_table$Freq %in% max_oc, 'total_run_names' ]

# ---
# 3. update filter_flag 
filter_flag <- read.csv( './int-out/filter_flag.csv', stringsAsFactors = F )
filter_flag$growth_flag <- 0  
filter_flag$growth_flag <- ifelse( filter_flag$run_name %in% growth_selected_run_names, 1, 0 )
write.csv( filter_flag, './int-out/filter_flag.csv', row.names = F )

# # ---
# # 4. preview in Tgav
# hector_res <- read.csv( './int-out/C.hector_run_cleanup.csv', stringsAsFactors = F )
# hector_tgav <- hector_res[ hector_res$variable == 'Tgav', ]
# tgav_selected <- hector_tgav[ hector_tgav$run_name %in% filter_flag[ filter_flag$tempature_flag == 1, "run_name" ], ]
# 
# tgav_w_flag <- merge( tgav_selected, filter_flag, by = 'run_name', all.x = T )
# 
# write.csv( tgav_w_flag, 'c:/Users/feng999/Desktop/temp/land_flux_filtering.csv', row.names = F )
