#### Load packages and adapted code####
library(tidyverse)
library(raster)
library(ncdf4)
library(sp)
library(rgdal)

#### Read nc data ####
file_name <- "Data/external/MARC_F1-MARS3D-MANGAE2500_20190101T0000Z.nc"
ncmars <- nc_open(file_name)
temp <- ncvar_get(ncmars, var = "TEMP")
h0 <- ncvar_get(ncmars, var = "H0")
latitude <- ncvar_get(ncmars, var = "latitude")
longitude <- ncvar_get(ncmars, var = "longitude")
raster_mars <- brick("Data/external/MARC_F1-MARS3D-MANGAE2500_20190101T0000Z.nc", varname = "TEMP")
raster_h0 <- raster("Data/external/MARC_F1-MARS3D-MANGAE2500_20190101T0000Z.nc", varname = "H0")
raster_lon <- raster("Data/external/MARC_F1-MARS3D-MANGAE2500_20190101T0000Z.nc", varname = "longitude")
raster_lat <- raster("Data/external/MARC_F1-MARS3D-MANGAE2500_20190101T0000Z.nc", varname = "latitude")

as.array(raster_lon)
as.array(raster_lat)

plot(raster_mars)
plot(raster_lon)
plot(raster_mars[[1]])
nc_close(ncmars)
#hc <- ncvar_get(ncmars, var = "hc")


outfile_mars <- "Data/interim/test_mars.nc"
writeRaster(raster_mars, outfile, overwrite = T, format = "CDF",
            varname = "TEMP", longname = "temperature", varunit="degrees_Celsius", xname = "lon", yname = "lat")
?writeRaster
