# Purpose: This script extracts and merges GCAM output databases from the GCAM parallel runs into 
# a single project file. 

# 0. Set Up ----------------------------------------------------------------------------------------
# Load rgcam the library call only works if devtools::install_github('JGCRI/rgcam') has already 
# been installed.
library(rgcam, lib.loc = '/people/dorh012/R/x86_64-pc-linux-gnu-library/3.4')

# Define the location on pic and reset the working dir.  
pic_gcam_dir <- '/pic/projects/GCAM/Dorheim/CMS/GCAM5/gcam-parallel'
setwd(pic_gcam_dir)

# Define the path to the query location
pic_hector_SA_npar_dir <- '/pic/projects/GCAM/Dorheim/CMS/hector-SA-npar'
query_path <- file.path(pic_hector_SA_npar_dir, 'input', 'cms_queries.xml')


# 1. database_basexdb to project ------------------------------------------------------------------

# Save the contents of exe_x/db/database_basexdb as .proj files using rgcam.

# Find all of the exe_x directories 
dirs <- dir(pattern = "exe_", full.names = T)

# Generate the dir/proj_exe_x.proj.proj list. 
proj_list  <- file.path(dirs,  paste0('proj_', basename(dirs), '.proj'))

for (prj in proj_list){
  path         <- dirname(proj_list[prj])
  conn         <- localDBConn(dbPath = file.path(path, "db"), dbFile = "database_basexdb")
  project_file <- addScenario(conn, proj_list[prj], queryFile = query_path)
}

message('Saving... ', paste(proj_list, sep = ', '))


# 2. Concatenate projs ---------------------------------------------------------------------------

# Merge the projects into a single proj
proj_files <- list.files(pattern = "proj_[0-9]+.proj", recursive = T, full.names = T)
prj        <- mergeProjects("proj_merge.proj", proj_files, clobber = TRUE)

message('Saving... ', proj_merge.proj)
# End
