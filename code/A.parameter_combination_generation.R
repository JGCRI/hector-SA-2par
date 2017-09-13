# this script generates beta and q10 pairs using quasi-random generator 
library( 'randtoolbox' )

setwd( 'c:/Users/feng999/Documents/CMS/hector-SA-2par' )

beta_range <- c( 0, 1 )
q10_range <- c( 0.2, 5 )

# quasi-random -- 	Sobol sequence
pair_mat <- sobol( 1000, 2 ) # 2-D 1000 pairs in total 
pair_df <- data.frame( pair_mat )
names( pair_df ) <- c( 'beta', 'q10' )

# shift the q10 from 0 ~ 1 to 0.2 ~ 5 
pair_df$q10 <- 	pair_df$q10 * 4.8 + 0.2 
pair_df <- round( pair_df, 5 )

# add run index and write out 
comb_df <- pair_df 
comb_df$run_index <- sprintf( '%04d', 1 : nrow( comb_df ) ) 
comb_df <- comb_df[ , c( 'run_index', 'beta', 'q10' ) ]

write.csv( comb_df, './int-out/A.par2_combinations.csv', row.names = F )

# diagnostic 
plot( comb_df$beta, comb_df$q10 )
length( unique( comb_df$beta ) ) 
length( unique( comb_df$q10 ) )


