# Purpose: Aggregate gridded MODIS NPP data to total global NPP data. Then prepare the Dn 
# metric input data to include obs, model, and variability columns.

# Note: The if statement in section 0 determines what the BASE & sub_dir 
# is set to depending on how the script is being sourced. If it is being 
# sourced from the run_all script then BASE and sub_dir used are going to 
# be defined there. If sourcing a single script at a time then BASE & sub_dir 
# will be defined here.

# 0. Set Up  ----------------------------------------------------------------------------------------------
# Define directories
if(!exists('run_all')){
  
  # Base directory 
  BASE       <- getwd()
  if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
  
  # The out-1/sub_directory to pull data from
  sub_dir    <- 'vary_4_params'

}


library(dplyr)
library(tidyr)
source(file.path(BASE, 'code', 'part_1', 'D.0.Dmetric_preprocessing_functions.R')) # The simga2 function


script_name <- 'D.NPP_Dmetric_preprocessing.R'
seperator   <- '----------'
message(script_name)
message('BASE directory is ', BASE, appendLF = T)
message('output/', sub_dir, appendLF = T)


# TRUE/FALSE to save the intermediate outputs created when processing the NPP data
save_intermediates <- FALSE



# 1. MODIS Data Processing ------------------------------------------------------------------------

# Import the MODIS file from Min, I think that there was quite a bit of processing done before but 
# we want to go from gridded data to a total annual global value. 

# The last column is the grid area in km2
# The unit for NPP is g C m-2 year-1.
# Import the data, it has no column names but we know that the first column is lon, the second is lat, the 
# final column is the size of the grid (km2), the remaining columns refer to NPP g C m-2 year-1 from 2000
# to 2013.

data <- read.csv(file.path(BASE, 'input', 'observations', 'MODIS_NPP_2000_2013_area.csv'), stringsAsFactors = FALSE, header = FALSE)

# Add the names to the MODIS data frame. 
names(data) <- c("lon", "lat", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                 "2009", "2010", "2011", "2012", "2013", "area_km2")

# According to Min the units on the values are 
data$units <- 'g C m-2 year-1'


# The NPP is in m-2 and the grid area is in km-2, convert the grid area to m-2 to be 
# consistent.
data$area_m2 <- data$area_km2 * 1000 ^ 2
data_m2 <- data[, names(data) != 'area_km2' ]


# Now calculate the total NPP for each grid cell by multiplying the 
# gird NPP by the grid area. This will convert NPP from gC m-2 yr-1 to 
# g C yr-1. 
data_m2 %>% 
  select( names(data_m2)[grepl('^2', names(data_m2))] ) %>%
  apply(2, function(data){
    
    no_area <- data * data_m2$area_m2
    
    return(no_area)
    
  }) %>% 
  as.data.frame %>% 
  mutate(units = 'g C year-1') -> 
  no_area_values


# Calculate the total global annual NPP value by aggregating the 
# total cell NPP values together. At this time we will also convert from 
# grams of Carbon to Pentagrams  of Carbon.

# Save a vector of the NPP column names to process.
npp_columns <- names(no_area_values)[grepl( '^2', names(no_area_values)) ]

# Find global NPP.
no_area_values %>% 
  select(npp_columns) %>% 
  apply(MARGIN = 2, FUN = function(data){
    
    # Sum all of the NPP entries per year and then divide by 1e15 to 
    # convert from g C to Pg C
    sum(data, na.rm = TRUE) / 1e15
    
  }) %>% 
  as.data.frame -> 
  global_NPP

# Format global NPP
names(global_NPP) <- 'obs'
global_NPP$year   <- as.integer(row.names(global_NPP))

global_NPP <- global_NPP[global_NPP$year < 2005, ]

global_NPP$units  <- 'Pg C/yr'
 
# We are going to assume that the NPP observations have around a 10 % error. 
global_NPP$s2n <- signif(global_NPP$obs * .10, 3)

# Add the sigma^2 value 
sigma2_value      <- sigma2(global_NPP, sd_coef = 2, use_rolling_sd = FALSE)
global_NPP$sigma2 <- sigma2_value



# If the save intermediates option is set to be true. 
if(save_intermediates){
  
  # Save the labeled MODIS file 
  file_name <- file.path(BASE, 'output', 'out-1', 'observations', 'MODIS_NPP_labeled.csv')
  write.csv(data, fie = file_name, row.names = FALSE)
  
  # the m2 MODIS file 
  file_name <- file.path(BASE, 'output', 'out-1', 'observations', 'MODIS_NPP_m2.csv')
  write.csv(data_m2, file = file_name, row.names = FALSE)
  
  # the MODIS file g C / year 
  file_name <- file.path(BASE, 'output', 'out-1', 'observations', 'MODIS_NPP_noArea_landWeights.csv')
  write.csv(no_area_values, file = file_name, row.names = FALSE)
  
  
}


# Process Hector NPP ----------------------------------------------------------------------------------------

# Import the Hector NPP data 
all_hector_npp <- read.csv(file.path(BASE, 'output', 'out-1', sub_dir, 'C.npp_hector_run_cleanup.csv'), stringsAsFactors = FALSE)

# Subset Hector NPP to make sure that it only includes NPP data for the same years as the observational 
# dataset being process.
all_hector_npp %>% 
  filter(year %in% global_NPP$year) %>% 
  rename(model = value) %>%
  full_join(global_NPP, by = c('units', 'year')) -> 
  NPP_Dn_input_table

# Save the Dn metric input table.
output_file <- file.path(BASE, 'output', 'out-1', sub_dir, 'D.NPP_Dmetric_input_table.csv')
write.csv(NPP_Dn_input_table, output_file, row.names = F)  
  
message(seperator)