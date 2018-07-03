install.packages('plot3D')
library('plot3D')
library(dplyr)

BASE <- getwd()

params_df <- read.csv(file.path(BASE, 'int-out','A.par4_combinations.csv'), stringsAsFactors = FALSE)

scatter3D(params_df$beta, params_df$q10, params_df$diff, phi = 0, col = 'black', 
          xlab = 'beta', ylab = 'Q 10', zlab = 'diff', bty = 'g')
scatter3D(params_df$q10, params_df$diff, params_df$s, col = params_df$ phi = 0, col = 'black', 
          xlab = 'Q 10', zlab = 'diff', ylab = 's', bty = 'g')
