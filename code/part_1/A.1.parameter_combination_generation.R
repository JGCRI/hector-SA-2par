
# Purpose: this script generates the hector input carbon - climate cycle parameter combinations 
# to use in standalone hector. 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.

# 0. Set Up --------------------------------------------------------------------------------
library( 'rngWELL' )
library( 'randtoolbox' )


# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_q10_only'
  
}

script_name <- 'A.1.parameter_combination_generation.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('output/', sub_dir, appendLF = T)

# 1. Set Up Geneator Inputs ----------------------------------------------------------------
# set seed for random generator 
setSeed( 6 )

beta_range <- c( 0, 1 )
q10_range <- c( 0.2, 5 )
s_range <- c( 1, 7 )
diff_range <- c( 0.5, 4 )

combination_amount <- 5000 

# 2. Generate Sequences ---------------------------------------------------------------------
# quasi-random -- Sobol sequence
combination_mat <- sobol( combination_amount, 4 ) # 4-D  
combination_df <- data.frame( combination_mat )
names( combination_df ) <- c( 'beta', 'q10', 's', 'diff' )

# the default range for combintation_mat is from 0 to 1 
# so shift the range from 0 - 1 to desired range for each parameter 

# shift the beta from 0 ~ 1 to 0 ~ 1 
# so no shift is needed 

# shift the q10 from 0 ~ 1 to 0.2 ~ 5 
combination_df$q10 <- combination_df$q10 * ( max( q10_range ) - min( q10_range ) ) + min( q10_range )

# shift the s from 0 ~ 1 to 2.1 ~ 4.6 
combination_df$s <- combination_df$s * ( max( s_range ) - min( s_range ) ) + min( s_range )

# shif the diff from 0 ~ 1 to 0 ~ 4 
combination_df$diff <- combination_df$diff * ( max( diff_range ) - min( diff_range ) ) + min( diff_range )

combination_df <- round( combination_df, 5 )

# add run index 
combination_df$run_index <- sprintf( '%04d', 1 : nrow( combination_df ) ) 
combination_df$run_name  <- paste0('hectorSA-', sprintf( '%04d', 1 : nrow( combination_df ) )) 
combination_df <- combination_df[ , c( 'run_index', 'run_name', 'beta', 'q10', 's', 'diff' ) ]

# 3. Save output -------------------------------------------------------------------
# write out
write.csv( combination_df, './output/out-1/A.par4_combinations.csv', row.names = F )

# 4. Sanity plot -------------------------------------------------------------------
# diagnostic 
# sample plot 1  
# jpeg( ".output/out-fig/beta vs q10.jpeg", width = 500, height = 500, res = 72)
# plot( x = combination_df$beta, 
#       y = combination_df$q10, 
#       'p',
#       main = '2-d plot of beta vs q10' )
# dev.off( )
# 
# # sample plot 2 
# jpeg( ".output/out-fig/beta vs s.jpeg", width = 500, height = 500, res = 72)
# plot( x = combination_df$beta, 
#       y = combination_df$s, 
#       'p',
#       main = '2-d plot of beta vs s' )
# dev.off( )

message(seperator)