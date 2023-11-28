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
dep = raster::raster("C:/Users/joligoos/geoloc/data/Depth data/Gebco/GEBCO_2014_2D.nc", var = 'elevation')

dep = terra::rast(dep)
plot(dep)
e = terra::ext(-10, 10, 40, 60)
dep = terra::crop(dep, e)
plot(dep)
e = terra::ext(3.5, 4, 51.5, 51.7)
dep_sub = terra::crop(dep, e)

dep_sub = ifel(dep_sub < -20, dep_sub -30, dep_sub )
plot(dep_sub)

dep_new = merge(dep_sub, dep)

var_dep_array <- as.array(t(dep_new))

#### Save lon and lat variable ####
nc_input = nc_open("C:/Users/joligoos/geoloc/data/Depth data/Gebco/GEBCO_2014_2D.nc")
lon = nc_input$dim['lon']$lon$vals
lon = lon[lon>=-10 & lon <= 10]
lat = nc_input$dim['lat']$lat$vals
lat = lat[lat >= 40 & lat <= 60]
lat = rev(lat)
nc_close(nc_input)

#### Set dimensions ####
londim <- ncdim_def("lon","degrees_east", 
                    longname = "longitude", vals = lon) 
latdim <- ncdim_def("lat","degrees_north", 
                    longname = "latitude", vals = lat)


#### Create nc file ####
outfile_new <- "Data/processed/Gebco/GEBCO_2014_2D_ed.nc"

# Define variables
var_dep <- ncvar_def("elevation", units = "m", 
                     longname = "Elevation relative to sea level",
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
  varid = 'elevation',
  vals = var_dep_array
)

nc_close(nc_new)
print(paste0("Saved in ", outfile_new))



dep = raster::raster("Data/processed/Gebco/GEBCO_2014_2D_ed.nc", var = 'elevation')
plot(dep)



