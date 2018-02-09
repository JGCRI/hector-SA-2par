library( 'tidyr' )
library( 'readxl' )

# ----------------------------------------------------------------
# The working directory should be the project directory. 
if(!(basename(getwd()) == 'hector-SA-npar')){stop('working directory should be the project directory')}

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

filter_exp_df$growth_min <- runmean( filter_exp_df$growth_min, 7,
                                     alg = c( 'C' ),
                                     endrule = c( 'mean' ),
                                     align = c( 'center' ) )
filter_exp_df$growth_max <- runmean( filter_exp_df$growth_max, 7, 
                                     alg = c( 'C' ), 
                                     endrule = c( 'mean' ), 
                                     align = c( 'center' ) ) 

write.csv( filter_exp_df, './int-out/observations/F.CDIAC_growth_ma.csv', row.names = F )
