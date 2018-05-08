# Purpose: This script allows you to source the different scripts for this repo in order for the 
# on a local machine, the part 1 level B code will not work here. Also if you try to source everything 
# at once it may take a long time. 


# 0. Set Up ------------------------------------------------------------------------------------------------

# Check to make sure that the working directory and the base dir path is the location of the project 
# directory. 
BASE <- getwd() 
if(basename(BASE) != "hector-SA-npar"){stop("Working directory should be the project location.")}



# Part 1 ---------------------------------------------------------------------------------------------------

# A 
#
# Randomly generate the 50,000 paramter combinations to use with Hector, this should only be sourced once 
# and the csv that is produced should be comitted so that the rest of the project code uses the correct 
# paramter combinations.  
# source(file.path(BASE, "code", "part_1", "A.parameter_combination_generation.R"))


# B 
# 
# Run Hector 50,000 times using the different parameter combinations as inputs on pic. Level 
# B scripts are ment to run on pic. 
# source(file.path(BASE, "code", "part_1", "B.ini_generation_and_pic_run.R"))
# sbatch B_jobscript.sh


# C 
# 
# Clean up and fromat the Hector results. 
# source(file.path(BASE, "code", "part_1", "C.hector_output_cleanup.R"))


# Observation Filtering Set UP -----------------------------------------------------------------------------
# This section acts at the set up section for the filtering related scripts. Note this only works if the 
# setup up sections are commented out in the sourced scripts otherwise these values will be over written.
# The hector processing scripts take a long time becasue of the size of the hector results file.

# rcp subdirectory selection.
rcpXX <- "rcp26"

# Length of years to use in the moving average
windowYrs <- 15 

# The percent of Hector run years that must fall within the observation range in order
# for the run to pass the fitler.
fall_in_threshold <- .80

# D
# Temperature Filtering
#source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.1.temperature_observation_processing.R"))
#source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.2.temperature_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "D.temperature_filtering", "D.3.temperature_filtering.R"))


# E
# CO2 Flux Filtering 
#source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.1.cdiac_observation_processing.R"))
#source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.2.flux_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "E.flux_filtering", "E.3.flux_filtering.R"))


# F 
# CO2 Growth 
#source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.1.cdiac_growth_processing.R"))
#source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.2.growth_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "F.co2_growth_filtering", "F.3.growth_filtering.R"))


# G 
# CO2 atm 
#source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.1.co2_noaa_processing.R"))
#source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.2.co2_hector_processing.R"))
source(file.path(BASE, "code", "part_1", "G.atm_co2_filtering", "G.3.co2_filtering.R" ))



# Part 2 ---------------------------------------------------------------------------------------------------
# The code for this section is not finalized yet.