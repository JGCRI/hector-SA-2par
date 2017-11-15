library( 'tidyr' )
library( 'readxl' )
library( 'smooth' )
library( 'forecast')
library( 'Mcomp' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

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

mav <- function( x, n ) { stats::filter( x, rep( 1 / n, n ), sides = 2 ) }

filter_exp_df$min_mav <- mav( filter_exp_df$min, 15 )
filter_exp_df$max_mav <- mav( filter_exp_df$max, 15 )

#plot( filter_exp_df$year , filter_exp_df$min_mav, 'l')
#lines( filter_exp_df$year, filter_exp_df$max_mav, col = 'red' )
filter_exp_df$min <- filter_exp_df$min_mav
filter_exp_df$max <- filter_exp_df$max_mav
filter_exp_df$min_mav <- NULL
filter_exp_df$max_mav <- NULL

write.csv( filter_exp_df, './int-out/D.temperature_obervation_ma.csv', row.names = F )
