# Purpose: this script creates the hetor-gcam ini files and the xml files that point 
# to the ini files for the paramter combinations that passed level G. This code requires
# gcam-core to in the same dir as the hector-SA-npar. This script is not computaionally 
# slow or anything but must be run on pic or where ever gcam hector is set up.

# 0. set Up -------------------------------------------------------------------
# Load libs
library( 'tidyr' )
library( 'XML' )
library( 'dplyr' )

# Decide which rcp scenario to run
rcpXX <- "rcp85"

# Define directories
pic_hectorSA_path <- '/pic/projects/GCAM/Dorheim/CMS/hector-SA-npar'
setwd( pic_hectorSA_path )

pic_gcam_path <- '/pic/projects/GCAM/Dorheim/CMS/gcam-core'
gcam_ini_path <- file.path(pic_gcam_path, "input/climate")


# Create the directory to store the new ini files in, clean up if neccessary. 
output_ini_path <- file.path(pic_gcam_path, "exe", "batch", "test")

if(dir.exists(output_ini_path)){
  
  old_ini <- as.vector(list.files(output_ini_path, ".ini", full.names = T))
  message("Removing ", paste(old_ini, collapse = " , "))
  file.remove(old_ini)
  
} else {
  
  dir.create(output_ini_path)
  
}


# 1. Create the hecotr-gcam ini files ------------------------------------------

# Import the paramter sets that selected in level G
param_sets <- readr::read_csv(file.path(pic_hectorSA_path, "int-out", rcpXX, "G.filtered_parameter_sets.csv"))

# Read in the gcam ini template
template_name <- paste0("hector-gcam_template.ini")
ini_template  <- readLines(file.path(pic_hectorSA_path, "input", "hector-gcam_template.ini"))

# This is left over from LY idk what it does
run_index_total_digits <- nchar( as.character( max( param_sets$run_index ) ) )  


# Create the hector gcam ini files using the paramters extracted from the filtered paramter sets file.
ini_path_list <- lapply( 1 : nrow( param_sets ), function( i ) { 
  
  # Set paramter values equal to the values from the param_sets data frame.
  beta <- param_sets[ i, 'beta' ]
  q10  <- param_sets[ i, 'q10' ]
  s    <- param_sets[i, 's']
  diff <- param_sets[ i, 'diff' ]
#  run_index <- sprintf( paste0('%0', run_index_total_digits, 'd' ), param_sets[ i, 'run_index' ] )
  
  # Create the path and the file for the new ini
  run_name <- param_sets[i, 'run_name']
  ini_name <- paste0( run_name, '.ini' )
  ini_file_path_name <- file.path( output_ini_path, ini_name )
  ini_file <- file( ini_file_path_name )
  
  # Start out by copying the ini template
  new_ini <- ini_template 
  
  # Change the paramter values in the ini file to match the paramter values from the 
  # paramter set csv file.
  new_ini[ 75 ]  <- paste0( 'beta=', beta, '     \t; 0.36=about +20% @2xCO2' ) 
  new_ini[ 76 ]  <- paste0( 'q10_rh=', q10, '\t\t; respiration response Q10, unitless' )
  new_ini[ 146 ] <- paste0( 'S=', s, ' \t\t\t\t; equilibrium climate sensitivity for 2xCO2, degC' )
  new_ini[ 147 ] <- paste0( 'diff=', diff, '\t\t\t; ocean heat diffusivity, cm2/s' )
  writeLines( new_ini, ini_file )
  close( ini_file )
  
  return(ini_file_path_name)
  
} ) 


# 2. Create the pointer xml files ------------------------------------------

# create_pointer_xml: is the function that uses an input data frame consisting of a 
# repalcement string and new_ini.xml name to replace the string in the template 
# xml file.

create_pointer_xml <- function(input, output_ini_path){
  
  # Pares out the resplacement and xml name from the input tibble.
  replacement <- input[["replacement"]]
  xml_name    <- input[["new_ini"]]
  
  # Load the template xml file
  xml_doc <- xmlTreeParse(file.path(pic_hectorSA_path, "input", "xml_pointer_template.xml"), useInternal = TRUE)
  nodes   <- getNodeSet(xml_doc, "//hector-ini-file")
  
  # Replace the string in the xml node with the new one pointing to the nex xml file.
  lapply(nodes, function(n) {
    xmlValue(n) <- gsub("../input/climate/rcpXX/replace.ini", replacement, xmlValue(n))
  })
  
  # Save the pointer xml
  saveXML(xml_doc, file = file.path(output_ini_path, new_ini))
  
  # Clean up and return useful information
  remove(xml_doc)
  
  return(file.path(output_ini_path, xml_name))
}


# Start by mmaking a dataframe of the string to insert into the pointer xml 
# and the name to give the xml from the ini paths.
tibble::tibble(path = unlist(ini_path_list)) %>% 
  mutate(replacement = gsub(pic_gcam_path, "../", path)) %>% 
  mutate(new_ini = gsub(".ini", ".xml", basename(path))) -> 
  ini_df

# Apply the create_pointer_xml function to all of the entries in the ini_df
apply(ini_df, 1, function(X){create_pointer_xml(X, output_ini_path)})




