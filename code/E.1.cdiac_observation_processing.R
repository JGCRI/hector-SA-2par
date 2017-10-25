library( 'tidyr' )
library( 'readxl' )
library( 'smooth' )
library( 'Mcomp' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

# ---
# 1. read in observations and reformat
ob_df <- read_excel( './input/observations/Global_Carbon_Budget_2016_v1.0.xlsx', sheet = "Global Carbon Budget", skip = 21 )
names( ob_df ) <- tolower( names( ob_df ) ) 
ob_df <- ob_df[ , c( 'year', 'ocean sink', 'land sink' ) ]
names( ob_df ) <- gsub( ' ', '_', names( ob_df ) )
ob_df$unit <- 'Gt C'# 1 Gt C = 1 Pg C (Pg used in hector)
ob_df$ocean_sink_min <- ob_df$ocean_sink - 0.5 
ob_df$ocean_sink_max <- ob_df$ocean_sink + 0.5 
ob_df$land_sink_min <- ob_df$land_sink - 0.8 
ob_df$land_sink_max <- ob_df$land_sink + 0.8 

# ---
# 3. Apply moving average on filter_df
filter_exp_df <- ob_df

mav <- function( x, n ) { filter( x, rep( 1 / n, n ), sides = 2 ) }

filter_exp_df$ocean_sink_min <- mav( filter_exp_df$ocean_sink_min, 7 )
filter_exp_df$ocean_sink_max <- mav( filter_exp_df$ocean_sink_max, 7 )

filter_exp_df$land_sink_min <- mav( filter_exp_df$land_sink_min, 7 )
filter_exp_df$land_sink_max <- mav( filter_exp_df$land_sink_max, 7 )

write.csv( filter_exp_df, './int-out/E.CDIAC_obervation_ma.csv', row.names = F )
