library( 'tidyr' )
library( 'readxl' )
library( 'smooth' )
library( 'Mcomp' )
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

# ---
# 1. read in observations and reformat
ob_df <- read_excel( './input/observations/Global_Carbon_Budget_2016_v1.0.xlsx', sheet = "Global Carbon Budget", skip = 21 )
names( ob_df ) <- tolower( names( ob_df ) ) 
ob_df <- ob_df[ , c( 'year', 'atmospheric growth' ) ]
names( ob_df ) <- gsub( ' ', '_', names( ob_df ) )
ob_df$unit <- 'Gt C'# 1 Gt C = 1 Pg C (Pg used in hector)
ob_df$growth_min <- ob_df$atmospheric_growth - 0.2 
ob_df$growth_max <- ob_df$atmospheric_growth + 0.2 

# ---
# 3. Apply moving average on filter_df
filter_exp_df <- ob_df

mav <- function( x, n ) { stats::filter( x, rep( 1 / n, n ), sides = 2 ) }

filter_exp_df$growth_min <- mav( filter_exp_df$growth_min, 7 )
filter_exp_df$growth_max <- mav( filter_exp_df$growth_max, 7 )

write.csv( filter_exp_df, './int-out/F.CDIAC_growth_ma.csv', row.names = F )
