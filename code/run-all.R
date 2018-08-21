# Purpose: This script allows you to source the different scripts for this repo in order for the 
# on a local machine, the part 1 level B code will not work here. Also if you try to source everything 
# at once it may take a long time. 


# Set Up ---------------------------------------------------------------------------------------------------

# Check to make sure that the working directory and the base dir path is the location of the project 
# directory. 
BASE <- getwd() 
if(basename(BASE) != "hector-SA-npar"){stop("Working directory should be the project location.")}

# If the run_all object exists in the environment then the BASE directory and the out/sub_dir
# will be defined by this script's settings, otherwise they will be defined within each 
# script INDEPENDETLY.

run_all <- TRUE

sub_dir <- 'vary_diff_s'

if( !any(grepl( sub_dir, list.dirs(file.path(BASE, 'out-1')))) ){
  
  stop('Could not find out-1/', sub_dir)
  
}


# Part 1 ---------------------------------------------------------------------------------------------------

# 1.A ####
# Randomly generate the 50,000 paramter combinations to use with Hector, this should only be sourced once 
# and the csv that is produced should be comitted so that the rest of the project code uses the correct 
# paramter combinations.  
# source(file.path(BASE, "code", "part_1", "A.parameter_combination_generation.R"))


# 1.B ####
# Run Hector 50,000 times using the different parameter combinations as inputs on pic. Level 
# B scripts are ment to run on pic. 
# source(file.path(BASE, "code", "part_1", "B.ini_generation_and_pic_run.R"))
# sbatch B_jobscript.sh


# 1.C ####
# Clean up and fromat the Hector results. 
#source(file.path(BASE, "code", "part_1", "C.hector_output_cleanup.R"))

# 1.D ####
# Process observation and Hector data to make the input tables used to calculate the Dn and Dc metrics. 
source(file.path(BASE, 'code', 'part_1', 'D.atmCO2_Dmetric_preprocessing.R'))
source(file.path(BASE, 'code', 'part_1', 'D.LandFlux_Dmetric_preprocessing.R'))
source(file.path(BASE, 'code', 'part_1', 'D.NPP_Dmetric_preprocessing.R'))      # This script is slow
source(file.path(BASE, 'code', 'part_1', 'D.Tgav_Dmetric_preprocessing.R'))

# 1.E ####
# Calculate Dn and Dc metric stats 
#source(file.path(BASE, 'code', 'part_1', 'E.1.Dmetric_independent_script.R'))
source(file.path(BASE, 'code', 'part_1', 'E.2.Dmetric_script.R'))


# 1.F ####
# Figure out which observational filters pass through the 
source(file.path(BASE, 'code', 'part_1', 'F.Dn_filter_flag.R'))


# Part 2 ---------------------------------------------------------------------------------------------------

# 2.A ####
# Select parameter combinations to use as inputs to Hector-GCAM.
source(file.path(BASE, 'code', 'part_2', 'A.0.Hector_run_selection_functions.R'))
source(file.path(BASE, 'code', 'part_2', 'A.1.parameter_selection_extremes_method.R'))

# 2.B ####
# Make the Hector GCAM inis, pointer xmls, and batch xml. This script needs to be run on pic or wherever 
# gcam-parallel is set up. 
# source(file.path(BASE, 'code', 'part_2', 'B.hector-gcam_ini_xml_maker.R'))

# 2.C 
#
# Extract queries from the database_basexdbs and save in a single rgcam.proj output. This script 
# needs to be run on pic or wherever the gcam-parallel strucutre is set up. 
# source(file.path(BASE, 'code', 'part_2', 'C.1.extract_GCAM_database_basexdb.R'))

# 2.D 
# 
# Process the rgcam project into a series of tibbles that is ready fro visualization and anlaysis. 
# This script depends on the merged project created in step C is saved in the out-2 directory. 
source(file.path(BASE, 'code', 'part_2', 'C.2.process_GCAM_output.R'))


# Visualize --------------------------------------------------------------------------------------

# TODO I don't really know exactly what I want to do here... 
