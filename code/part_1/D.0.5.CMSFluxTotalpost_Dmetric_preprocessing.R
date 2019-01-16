# Purpose: To use the CMS Flux total Posterior carbon flux and uncertainty in Dn observational filtering 
# of Hector atmos_c restults. This script does the preprocessing of the CMS data to convert it from 
# grided values to a global mean. 
# 
# nc4 data must be downloaded from the CMS archive and stored on pic to be processed with a CDO
#
# ** Since this script only needs to be run once it will not be incorperated into the run all script 
# I want to wait for the hector runs to stop running before I change the dirs on peopel


# 0. Set Up ------------------------------------------------------------------------

BASE <- getwd()
OUTPUT <- file.path(BASE, 'input', 'observations')

if(!"hector-SA-npar.Rproj" %in% list.files(BASE)){stop('BASE must be the project location')}
stopifnot(dir.exists(OUTPUT))


library(ncdf4)
library(tidyr)
library(dplyr)


# 1. Define Functions ---------------------------------------------------------------

# Process monnthly grided flux data a global average
#
# Args: 
#   path: the path to the nc file to process
# Return: a data frame of time, value, and units 

process_flux_nc <- function(path){
  
  # Import the nc file
  nc <- nc_open(path)
  
  # Parse out the data, area, and grid information from the nc
  data_3d        <- ncvar_get(nc, 'Total_Flux_Posterior')
  data_3d_uncert <- ncvar_get(nc, 'Total_Flux_Posterior_Uncertainty')
  area    <- ncvar_get(nc, 'Area')
  lat     <- ncvar_get(nc, 'lat')
  lon     <- ncvar_get(nc, 'lon')
  
  # Figure out the lat/lon information
  nlat <- length(lat)
  nlon <- length(lon)
  
  # Parse out the time infromation from the file path name
  time <- gsub(x = basename(path), pattern = 'CMSFluxTotalpost_|_v1.nc4', replacement = '')
  
  
  # Flatten the 3d data extracted from the netcdf into a 2d array where lat is the 
  # most rapidly increasing index.
  data      <- aperm(data_3d, c(3,2,1))
  dim(data) <- c(12, nlat*nlon)
  
  data_uncert      <- aperm(data_3d_uncert, c(3,2,1))
  dim(data_uncert) <- c(12, nlat*nlon)
  
  # Duplicate the area vector so that it matches gridcells.
  weights <- rep(x = area, nlon)
  
  if(length(weights) != ncol(data)) {stop('The area weights and data are inconsistent')}
  
  # Mulitply the gridcells by the weights and claculate the 
  global_values <- apply(data, 1, 
                         function(input = x){
                           
                           sum(input) # * weights, na.rm = TRUE)
                           
                         })
  
  global_uncert <- apply(data_uncert, 1, 
                         function(input = x){
                           
                           sum(input * weights, na.rm = TRUE)
                           
                         })
  
  
  # Calculate the annual value 
  annual_value  <- sum(global_values)
  annual_uncert <- sum(global_uncert)
  
  # Convert from  kg/s to PgC by muiltplying 31557600 seconds and 1e-12 to convert from kg to Pg
  rslt_PgC    <- annual_value * 31557600 / 1e12
  rslt_uncert <- annual_uncert * 31557600 / 1e12
  
  data.frame(time = time,
             value = rslt_PgC,
             uncertainty = rslt_uncert,
             units = 'PgC')
  
}


# 2. Process the netcdfs --------------------------------------------------------------------
nc_paths <- c('C:/Users/dorh012/Documents/hector-SA-npar/input/observations/CMSFluxTotalpost/CMSFluxTotalpost_2010_v1.nc4',
              'C:/Users/dorh012/Documents/hector-SA-npar/input/observations/CMSFluxTotalpost/CMSFluxTotalpost_2011_v1.nc4',
              'C:/Users/dorh012/Documents/hector-SA-npar/input/observations/CMSFluxTotalpost/CMSFluxTotalpost_2012_v1.nc4')


global_annual_posterior_flux <- bind_rows(lapply(X = nc_paths, FUN = process_flux_nc))

# 3. Save the results ----------------------------------------------------------------------

# Save the output to the input/observations directory 
output_dir <- 'C:/Users/dorh012/Documents/hector-SA-npar/input/observations'
write.csv(file = file.path(output_dir, 'CMSFluxTotalpost.csv'), x = global_annual_posterior_flux, row.names = FALSE)



nc <- nc_open('C:/Users/dorh012/Documents/hector-SA-npar/input/observations/CMSFluxTotalpost/CMSFluxTotalpost_2010_v1.nc4')
# End 



