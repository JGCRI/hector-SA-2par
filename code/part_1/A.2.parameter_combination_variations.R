
# Purpose: this script generates the hector paramter unput combinations based on the output from 
# A.1.paramter_combination_generation.R script. The idea is that this script will can create 
# hector inputs that hold some carbon-cycle and climate parameter inputs constant. 

# Notes/TODO there might be a way to make this script more flexible but for now I am going to hard 
# code it so that script outputs are transparent. 

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.


# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)

# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}

}

script_name <- 'A.2.parameter_combination_variations.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)

# 1. Import Data ---------------------------------------------------------------------------------------

# Import the npar paramter combinations, the varying parameters values will come from this csv file.  
input_params <- read.csv(file.path(BASE, 'out-1', 'A.par4_combinations.csv'), stringsAsFactors = FALSE)

# Define the default parameter values. 
default_s     <- 3.0
default_diff  <- 2.3
default_q10   <- 2.0 
default_beta  <- 0.36


# 2. Vary Q10 only -------------------------------------------------------------------------------------

# Relace S, diff, and beta values with the defaults values. 
vary_q10_only  <- input_params

vary_q10_only$beta <- default_beta
vary_q10_only$s    <- default_s
vary_q10_only$diff <- default_diff

# Save the output 
dir_path <- file.path(BASE, 'out-1', 'vary_q10_only') 
dir.create(dir_path, showWarnings = FALSE)
write.csv(vary_q10_only, file = file.path(dir_path, 'A.par4_combinations.csv'), row.names = FALSE)


# 3. Vary beta only -------------------------------------------------------------------------------------

# Repalce the S, q10, and diff values with the default values. 
vary_beta_only <- input_params

vary_beta_only$s    <- default_s
vary_beta_only$q10  <- default_q10
vary_beta_only$diff <- default_diff


# Save the output 
dir_path <- file.path(BASE, 'out-1', 'vary_beta_only')
dir.create(dir_path, showWarnings = FALSE)
write.csv(vary_beta_only, file = file.path(dir_path, 'A.par4_combinations.csv'), row.names = FALSE)


# 4. Vary beta and q10 -------------------------------------------------------------------------------------

# Replace the s and diff values with the default values 
vary_beta_q10 <- input_params

vary_beta_q10$s    <- default_s
vary_beta_q10$diff <- default_diff

dir_path <- file.path(BASE, 'out-1', 'vary_beta_q10')
dir.create(dir_path, showWarnings = FALSE)
write.csv(vary_beta_q10, file = file.path(dir_path, 'A.par4_combinations.csv'), row.names = FALSE)


message(seperator)

# 5. Vary diff and s -------------------------------------------------------------------------------------

# Replace the beta and q10 values with the default values 
vary_diff_s <- input_params

vary_diff_s$q10  <- default_q10
vary_diff_s$beta <- default_beta

dir_path <- file.path(BASE, 'out-1', 'vary_diff_s')
dir.create(dir_path, showWarnings = FALSE)
write.csv(vary_diff_s, file = file.path(dir_path, 'A.par4_combinations.csv'), row.names = FALSE)


message(seperator)

# 6. Vary diff only -------------------------------------------------------------------------------------

# Replace the s, beta and q10 values with the default values 
vary_diff_only <- input_params

vary_diff_only$q10  <- default_q10
vary_diff_only$beta <- default_beta
vary_diff_only$s <- default_beta


dir_path <- file.path(BASE, 'out-1', 'vary_diff_only')
dir.create(dir_path, showWarnings = FALSE)
write.csv(vary_diff_only, file = file.path(dir_path, 'A.par4_combinations.csv'), row.names = FALSE)


message(seperator)
