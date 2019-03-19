
# Purpose: This script extracts and merges GCAM output databases from the GCAM parallel runs into 
# a single project file. This script needs to be run on pic or wherever the parallel gcam strcuture 
# is set up. 
#
# This is set up to run on pic. For the bigger batches a job script may have to be submitted. 
#
# TODO figure out a way to avoid the manual steps associated with this script. 

# 0. Set Up ----------------------------------------------------------------------------------------

# Load rgcam the library call only works if devtools::install_github('JGCRI/rgcam') has already 
# been installed.
#library(rgcam, lib.loc = '/people/dorh012/R/x86_64-pc-linux-gnu-library/3.4')
library(rgcam)
# Define the location on pic and reset the working dir.  
pic_gcam_dir <- '/pic/projects/GCAM/Dorheim/CMS/GCAM5/gcam-parallel'


# Define the exe directory to search - this requires that all of the exe_x be manually moved into the 
# the named exe_dir. 
exe_dir <- 'policy1_exe'; setwd(pic_gcam_dir)


# Define the path to the hector sensitivity analysis repository 
pic_hector_SA_npar_dir <- '/pic/projects/GCAM/Dorheim/CMS/hector-SA-npar'



# 1. database_basexdb to project ------------------------------------------------------------------
# In this section data will be extracted from the database_basexdbs using rgcam and saved as a projs.
# but right now the current gcam-parallel set up does not write the full database_basxdb out instead
# it saves it as an R proj, so for now this section of code can be commented out.
#
# Save the contents of exe_x/db/database_basexdb as .proj files using rgcam.

# Find all of the exe_x directories
dirs <- dir(path = exe_dir, pattern = 'exe_', full.names = T)

# Generate the dir/proj_exe_x.proj.proj list.
proj_list  <- file.path(dirs,  paste0('proj_', basename(dirs), '.proj'))

for (prj in proj_list){

    path <- dirname(prj)

    contents <- list.dirs(path, full.names = T, recursive = T)

    if( any( grepl("database_basexdb", contents) ) ){

      conn               <- localDBConn(dbPath = file.path(path, "db"), dbFile = "database_basexdb")
      project_file       <- addScenario(conn = conn, proj = paste0('./', prj), clobber = TRUE)
      extra_project_file <- addScenario(conn = conn, proj = paste0('./', paste0(prj, 'extra.proj')), clobber = TRUE)
      

    }
}


# # 2. Concatenate projs ---------------------------------------------------------------------------
# 
# Merge the projects into a single proj
path <- list.files(dirname(proj_list), '.proj', full.names = TRUE)
prj  <- mergeProjects(paste0('./', sub_dir, '_proj.proj'), path)


# You will want to make sure that the merged project is moved to the correct hector-SA-npar/out-2 location.  
message('Saving... ', proj_merge.proj)
# # End 
