# this script generates beta, q10, s, and diff combinations using quasi-random generator 
library( 'rngWELL' )
library( 'randtoolbox' )

# ----------------------------------------------------------------
# Settings you will definitely need to overwrite in your code
setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-npar' )

# ----------------------------------------------------------------
# set seed for random generator 
setSeed( 6 )

# ----------------------------------------------------------------
# Settings you may probably need to overwrite
beta_range <- c( 0, 1 )
q10_range <- c( 0.2, 5 )
s_range <- c( 1, 7 )
diff_range <- c( 0.5, 4 )

combination_amount <- 5000 

# ----------------------------------------------------------------
# quasi-random -- Sobol sequence
combination_mat <- sobol( combination_amount, 4 ) # 4-D  
combination_df <- data.frame( combination_mat )
names( combination_df ) <- c( 'beta', 'q10', 's', 'diff' )

# the default range for combintation_mat is from 0 to 1 
# so shift the range from 0 - 1 to desired range for each parameter 

# shift the beta from 0 ~ 1 to 0 ~ 1 
# so shift is needed 

# shift the q10 from 0 ~ 1 to 0.2 ~ 5 
combination_df$q10 <- combination_df$q10 * ( max( q10_range ) - min( q10_range ) ) + min( q10_range )

# shift the s from 0 ~ 1 to 2.1 ~ 4.6 
combination_df$s <- combination_df$s * ( max( s_range ) - min( s_range ) ) + min( s_range )

# shif the diff from 0 ~ 1 to 0 ~ 4 
combination_df$diff <- combination_df$diff * ( max( diff_range ) - min( diff_range ) ) + min( diff_range )

combination_df <- round( combination_df, 5 )

# add run index 
combination_df$run_index <- sprintf( '%04d', 1 : nrow( combination_df ) ) 
combination_df <- combination_df[ , c( 'run_index', 'beta', 'q10', 's', 'diff' ) ]

# -------------------------------------------------------------------
# write out
write.csv( combination_df, './int-out/A.par4_combinations.csv', row.names = F )

# -------------------------------------------------------------------
# diagnostic 
# sample plot 1  
jpeg( "./diag-out/beta vs q10.jpeg", width = 500, height = 500, res = 72)
plot( x = combination_df$beta, 
      y = combination_df$q10, 
      'p',
      main = '2-d plot of beta vs q10' )
dev.off( )

# sample plot 2 
jpeg( "./diag-out/beta vs s.jpeg", width = 500, height = 500, res = 72)
plot( x = combination_df$beta, 
      y = combination_df$s, 
      'p',
      main = '2-d plot of beta vs s' )
dev.off( )

