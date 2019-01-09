# Purpose: this script creates the hetor-gcam ini files, the xml files that point 
# to the ini files for the paramter combinations that passed part 2 A. This script 
# also creates a batch file to run gcam with the different ini files. 
# This script is not computaionally slow or anything but must be run on pic or 
# where ever gcam hector is set up. See section 0. A for user decisions. 

# 0. Set Up -------------------------------------------------------------------

# Load libs
library(tidyr)
library(XML)
library(dplyr)
library(xml2)
library(tibble)


# User decisions
sub_name          <- "AGU" 
sub_dir           <- 'AGU'
pic_hectorSA_path <- '/pic/projects/GCAM/Dorheim/CMS/hector-SA-npar'; setwd( pic_hectorSA_path ) # Where the hecotr-SA-npar lives on pic
param_path        <- file.path(pic_hectorSA_path, 'out-2', sub_dir, 'A.Hector_GCAM_parameters.csv')  # Define the path to the GCAM Hector parameters to use

# The gcam dirs must reflect the gcam parallel structure
pic_gcam_path     <- '/pic/projects/GCAM/Dorheim/CMS/GCAM5/gcam-parallel'     # Where CMS gcam-parallel lives
gcam_ini_path     <- file.path(pic_gcam_path, "input/climate", sub_name)      # The dir to write the new hector-gcam ini files to 
xml_pointer_path  <- file.path(pic_gcam_path, "configuration-sets", sub_name) # The dir to write the poitner xml files to 


# Decide what hector paramters to use in the hector-gcam ini files. These paramters should come 
# from part 2 A that selects the paramter combinations to read into hector-gcam.
param_sets <- read.csv(param_path, stringsAsFactors = FALSE)


# Clean up or create the ini directory.
if(dir.exists(gcam_ini_path)){
  
  old_ini <- as.vector(list.files(gcam_ini_path, ".ini", full.names = T))
  message("Removing old ", sub_name, "/ini files.")
  file.remove(old_ini)
  
} else {
  
  dir.create(gcam_ini_path)
  
}

if(dir.exists(xml_pointer_path)){
  
  old_xml <- as.vector(list.files(xml_pointer_path, ".xml", full.names = T))
  message("Removing old ", sub_name, "/xml files.")
  file.remove(old_xml)
  
} else {
  
  dir.create(xml_pointer_path)
  
}



# 1. Create the hector-gcam ini files ------------------------------------------

# This sction uses the CMS hector-gcam_template.ini file from the CMS repo and 
# replaces the default params with the new paramter combinations. The ini contains 
# relative pathways to emissions in gcam so if a new tempalte is used or the 
# inis are written to directory other than gcam-core/input/climate/sub_name then 
# the inis will fail. 

# Read in the gcam ini template
template_name <- paste0("hector-gcam_template.ini")
ini_template  <- readLines(file.path(pic_hectorSA_path, "input", "hector-gcam_template.ini"))

# This is left over from LY idk what it does
run_index_total_digits <- nrow(param_sets)

# Create the hector gcam ini files using the paramters extracted from the filtered paramter sets file.
ini_path_list   <- lapply( 1 : nrow( param_sets ), function( i ) { 
  
  # Set paramter values equal to the values from the param_sets data frame.
  beta <- param_sets[ i, 'beta' ]
  q10  <- param_sets[ i, 'q10' ]
  s    <- param_sets[i, 's']
  diff <- param_sets[ i, 'diff' ]
  # run_index <- sprintf( paste0('%0', run_index_total_digits, 'd' ), param_sets[ i, 'run_index' ] )
  
  # Create the path and the file for the new ini
  run_name <- param_sets[i, 'run_name']
  ini_name <- paste0( run_name, '.ini' )
  ini_file_path_name <- file.path( gcam_ini_path, ini_name )
  ini_file <- file( ini_file_path_name )
  
  # Start out by copying the ini template
  new_ini <- ini_template 
  
  # Change the paramter values in the ini file to match the paramter values from the 
  # paramter set csv file.
  new_ini[ 69 ]  <- paste0( 'beta=', beta, '     \t; 0.36=about +20% @2xCO2' ) 
  new_ini[ 70 ]  <- paste0( 'q10_rh=', q10, '\t\t; respiration response Q10, unitless' )
  new_ini[ 140 ] <- paste0( 'S=', s, ' \t\t\t\t; equilibrium climate sensitivity for 2xCO2, degC' )
  new_ini[ 141 ] <- paste0( 'diff=', diff, '\t\t\t; ocean heat diffusivity, cm2/s' )
  writeLines( new_ini, ini_file )
  close( ini_file )
  
  return(ini_file_path_name)
  
} ) 


# 2. Create the pointer xml files ------------------------------------------

# create_pointer_xml: is the function that uses a data frame consisting of a 
# gcam realive path to ini and and new_ini name to replace the string in 
# the template xml and then saves the new xml as the run_name.xml in 
# preparation of running a batch. This function returns the full file name 
# of the new xml.

create_pointer_xml <- function(input, output_ini_path){
  
  # Pares out the resplacement and xml name from the input tibble.
  replacement <- input[["replacement"]]
  xml_name    <- input[["new_ini"]]
  
  
  # Load the template xml file
  xml_doc <- xmlTreeParse(file.path(pic_hectorSA_path, "input", "xml_pointer_template.xml"), useInternal = TRUE)
  
  # Replace the string in the xml node with the new one pointing to the nex xml file.
  lapply(getNodeSet(xml_doc, "//hector-ini-file"), function(n) {
    xmlValue(n) <- gsub("../input/climate/sub_dir/replace.ini", replacement, xmlValue(n))
  })
  
  # Save the pointer xml
  saveXML(xml_doc, file = file.path(output_ini_path, xml_name))
  
  # Clean up and return useful information
  remove(xml_doc)
  
  return(file.path(output_ini_path, xml_name))
}

# Create pointer xmls

# Start by mmaking a dataframe of the string to insert into the pointer xml 
# and the name to give the xml from the ini paths.
tibble::tibble(path = unlist(ini_path_list)) %>% 
  mutate(replacement = gsub(pic_gcam_path, "..", path)) %>%
  mutate(new_ini = gsub(".ini", ".xml", basename(path))) -> 
  ini_df

# Apply the create_pointer_xml function to all of the entries in the ini_df.
pointer_xmls <- apply(ini_df, 1, function(X){create_pointer_xml(X, xml_pointer_path)})


# 3. Create the batches --------------------------------------------------

# batch_maker: is a function that will write all of the pointer xmls into 
# a single batch so that gcam can use all of the new hector ini files in 
# batch mode. This code is based off of code from Russell Horowitz. This
# function uses a data frame that contains the scenario name (hector run name)
# and the relative xml path name. This function will save the xml at the 
# batch_path input as batch_name.xml.

batch_maker <- function(xml_df, batch_path, batch_name){
  
  # Create the batch runner xml and add the component set
  doc <- xml_new_root("BatchRunner")
  cmp <- xml_add_child(doc, "ComponentSet", name="hector_ini")
  
  # For every 
  for(ini in 1:nrow(xml_df)){
    
    scenario_name <- xml_df$scenario_name[ini]
    fileNode      <- xml_add_child(cmp, "FileSet", name = scenario_name)
    valueNode     <- xml_add_child(fileNode, "Value", name = "ini", xml_df$xml_path[ini])
    
  }
  
  write_xml(doc, file = file.path(batch_path, paste0(batch_name, ".xml")))
  
}


# Use the pointer xml full file names to create columns for the scenario 
# name and relative xml path. 
tibble(full_name = pointer_xmls) %>% 
  mutate(xml_path = paste0("..", gsub(pic_gcam_path, "", full_name))) %>% 
  mutate(scenario_name = gsub(".xml", "", basename(xml_path))) -> 
  xml_df


batch_maker(xml_df, file.path(pic_gcam_path, "/configuration-sets/"), sub_name)


# End ---- 
message("script complete")

