#### Script to fix missing data 2018-12-26 - 2018-12-31 ####

#### Load packages ####
library(tidyverse)
library(terra)
library(ncdf4)
library(sf)
library(raster)

#### Set directory #### 
directory_out = "Data/processed/DELFT3D/"

#### Read data ####
dep = raster::raster("Data/processed/DELFT3D/2018/DELFT3D_20180101T0000Z.nc", var = 'DEPTH')

dep = terra::rast(dep)
e = terra::ext(3.5, 4, 51.5, 51.7)
dep_sub = terra::crop(dep, e)

dep_sub = ifel(dep_sub > 30, dep_sub +20, dep_sub )
plot(check_sub)

dep_new = merge(dep_sub, dep)

var_dep_array <- as.array(t(dep_new))

#### Save lon and lat variable ####
nc_input = nc_open("Data/processed/DELFT3D/2018/DELFT3D_20180101T0000Z.nc")
lon = nc_input$dim['longitude']$longitude$vals
lat = nc_input$dim['latitude']$latitude$vals
nc_close(nc_input)

#### Set dimensions ####
londim <- ncdim_def("longitude","degrees_east", 
                    longname = "longitude", vals = lon) 
latdim <- ncdim_def("latitude","degrees_north", 
                    longname = "latitude", vals = lat)


#### Create nc file ####
outfile_new <- "Data/processed/DELFT3D/DELFT3D_depth_edSchelde.nc"

# Define variables
var_dep <- ncvar_def("DEPTH", units = "m", 
                     longname = "Water depth at pressure points",
                     dim = list(londim, latdim),
                     missval = -3.4e+38)

# Create new nc file 
nc_new <- nc_create(
  filename = outfile_new,
  vars = list(var_dep)
)

# Put values in variables
ncvar_put(
  nc = nc_new,
  varid = 'DEPTH',
  vals = var_dep_array
)

nc_close(nc_new)
print(paste0("Saved in ", outfile_new))






