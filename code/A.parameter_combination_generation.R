# this script generates beta and q10 pairs using quasi-random generator 
library( 'randtoolbox' )

setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

setSeed(6)

beta_range <- c( 0, 1 )
q10_range <- c( 0.2, 5 )

# quasi-random -- 	Sobol sequence
combination_mat <- sobol( 2000, 3 ) # 2-D 1000 pairs in total 
combination_df <- data.frame( combination_mat )
names( combination_df ) <- c( 'beta', 'q10', 's' )

# shift the q10 from 0 ~ 1 to 0.2 ~ 5 
combination_df$q10 <- combination_df$q10 * 4.8 + 0.2
# shift the s from 0 ~ 1 to 2.1 ~ 4.6 
combination_df$s <- combination_df$s * 2.5 + 2.1
combination_df <- round( combination_df, 5 )

# add run index and write out 
combination_df$run_index <- sprintf( '%04d', 1 : nrow( combination_df ) ) 
combination_df <- combination_df[ , c( 'run_index', 'beta', 'q10', 's' ) ]

write.csv( combination_df, './int-out/A.par3_combinations.csv', row.names = F )

# diagnostic 
plot( combination_df$beta, combination_df$q10, 'p' )
length( unique( comb_df$beta ) ) 
length( unique( comb_df$q10 ) )


