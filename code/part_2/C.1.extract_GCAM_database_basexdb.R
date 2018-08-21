
# Purpose: This script extracts and merges GCAM output databases from the GCAM parallel runs into 
# a single project file. This script needs to be run on pic or wherever the parallel gcam strcuture 
# is set up. 
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
exe_dir <- 'exe_scaled_emissions'; setwd(pic_gcam_dir)


# Define the path to the hector sensitivity analysis repository 
pic_hector_SA_npar_dir <- '/pic/projects/GCAM/Dorheim/CMS/hector-SA-npar'
query_path             <- file.path(pic_hector_SA_npar_dir, 'input', 'cms_queries.xml')


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

# for (prj in proj_list){
# 
#     path <- dirname(prj)
# 
#     contents <- list.dirs(path, full.names = T, recursive = T)
# 
#     if( any( grepl("database_basexdb", contents) ) ){
# 
#       conn         <- localDBConn(dbPath = file.path(path, "db"), dbFile = "database_basexdb")
#       project_file <- addScenario(conn = conn, proj = paste0('./', prj), queryFile = query_path)
# 
#     }
# }

# message('Saving... \n', paste(proj_list, collapse =  ' \n'))


# # 2. Concatenate projs ---------------------------------------------------------------------------
# 
# Merge the projects into a single proj
proj_files <- list.files(paste0('./', exe_dir), pattern = "proj_[0-9]+.proj", recursive = T, full.names = T)
proj_files <- proj_files[file.exists(proj_files)]
prj        <- mergeProjects(paste0("./proj_merge", gsub('exe', '', exe_dir),".proj"), proj_files)

# You will want to make sure that the merged project is located in the hector-SA-npar/out-2


message('Saving... ', proj_merge.proj)
# # End 
