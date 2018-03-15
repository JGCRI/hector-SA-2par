library( 'tidyr' )
library( 'readxl' )
library( 'caTools' )


# This section is commented out so that the script can be sourced from the 
# run_all script. If you would like to run this script by it's self make sure that 
# the rest of the code in the set up section is not commented out. 
# # 0. Set Up ----------------------------------------------------------------
# # The working directory should be the project directory. 
# if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}
# 
# # Length of years to use in the moving average
# windowYrs <- 15 

# ---
# 1. read in observations and reformat
ob_df <- read_excel( './input/observations/globaltemps.xlsx', skip = 5 )
ob_df$min <- apply( ob_df[ , c( "GISS", "NOAA", "HadCRUT4", "Berkeley" ) ], 1, min, na.rm = T )
ob_df$max <- apply( ob_df[ , c( "GISS", "NOAA", "HadCRUT4", "Berkeley" ) ], 1, max, na.rm = T )
ob_year_list <- sort( unique( ob_df$Year ) )
filter_df <- ob_df[ , c( 'Year', "min", "max" ) ]
colnames( filter_df ) <- c( 'year', 'min', 'max' )

# ---
# 3. Apply moving average on filter_df 
filter_exp_df <- filter_df 

filter_exp_df$min_mav <- runmean( filter_exp_df$min, 
                                  windowYrs, 
                                  alg = c( 'C' ), 
                                  endrule = c( 'mean' ), 
                                  align = c( 'center' ) )
filter_exp_df$max_mav <- runmean( filter_exp_df$max, 
                                  windowYrs, 
                                  alg = c( 'C' ), 
                                  endrule = c( 'mean' ), 
                                  align = c( 'center' ) )

# plot( filter_exp_df$year , filter_exp_df$min_mav, 'l')
# lines( filter_exp_df$year, filter_exp_df$max_mav, col = 'red' )
filter_exp_df$min <- filter_exp_df$min_mav
filter_exp_df$max <- filter_exp_df$max_mav
filter_exp_df$min_mav <- NULL
filter_exp_df$max_mav <- NULL

write.csv( filter_exp_df, './int-out/observations/D.temperature_obervation_ma.csv', row.names = F )
