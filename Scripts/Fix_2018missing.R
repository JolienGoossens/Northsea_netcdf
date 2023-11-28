#### Script to fix missing data 2018-12-26 - 2018-12-31 ####

#### Load packages ####
library(tidyverse)
library(terra)
library(ncdf4)
library(sf)
library(raster)

#### Set directory #### 
directory_year_out = "Data/processed/DELFT3D/2018/missing_interp/"

#### Read data ####
christmas = raster::brick("Data/processed/DELFT3D/2018/DELFT3D_20181225T0000Z.nc", var = 'TEMP')
newyear = raster::brick("Data/processed/DELFT3D/2019/DELFT3D_20190101T0000Z.nc", var = 'TEMP')
dep = raster::raster("Data/processed/DELFT3D/2018/DELFT3D_20181225T0000Z.nc", var = 'DEPTH')



#### Save sigma and time variable ####
nc_input = nc_open("Data/processed/DELFT3D/2018/DELFT3D_20181225T0000Z.nc")
sigma <- ncvar_get(nc_input, "SIGMA")
dep = ncvar_get(nc_input, "DEPTH")
lon = nc_input$dim['longitude']$longitude$vals
lat = nc_input$dim['latitude']$latitude$vals
nc_close(nc_input)

#### Set dimensions ####
londim <- ncdim_def("longitude","degrees_east", 
                    longname = "longitude", vals = lon) 
latdim <- ncdim_def("latitude","degrees_north", 
                    longname = "latitude", vals = lat)
leveldim <- ncdim_def("level", "layer", longname = "sigma_level", vals = 1:20)

#### Make dates out of time stamps
timestart <- lubridate::parse_date_time("2011-12-22 00:00:00", orders = "ymd HMS")
datestamp = seq.POSIXt(lubridate::parse_date_time("2018-12-25 00:00:00", orders = "ymd HMS"),
                       lubridate::parse_date_time("2018-12-31 00:00:00", orders = "ymd HMS"), by = "day")
datestamp = lubridate::parse_date_time(paste0(as.character(datestamp), " 00:00:00"), 
                                      orders = "ymd HMS ", tz = 'UTC')
time_seconds = as.numeric(difftime(lubridate::parse_date_time(paste0(as.character(datestamp), " 12:00:00"), 
                                                              orders = "ymd HMS"), timestart, units = 'secs'))

#### Make dep array ####
var_dep_array <- as.array(dep)

# Interpolate for all z levels

lapply(2:7, function(timestep_i){
  list_rast = lapply(1:20, function(i){
    r25 = christmas[[i]]
    r26 = setValues(christmas[[i]], NA)
    r27 = setValues(christmas[[i]], NA)
    r28 = setValues(christmas[[i]], NA)
    r29 = setValues(christmas[[i]], NA)
    r30 = setValues(christmas[[i]], NA)
    r31 = setValues(christmas[[i]], NA)
    r01 = newyear[[i]]
    s = raster::stack(r25, r26, r27, r28, r29, r30, r31, r01)
    x1 = approxNA(s, method = "linear")
    return(x1)
  })
  list_rast_day = lapply(1:20, function(i){
    r_i = list_rast[[i]][[timestep_i]]
  })
  rast_day = raster::brick(list_rast_day)
  var_temp_array <- as.array(t(rast_day[[c(1:20)]]))
  
  #### Save as nc ####
  
  # Define time dimension
  timedim <- ncdim_def("time", "seconds since 2011-12-22 00:00:00", longname = 'time',
                       vals = time_seconds[timestep_i])
  # Define variables
  var_dep <- ncvar_def("DEPTH", units = "m", 
                       longname = "Water depth at pressure points",
                       dim = list(londim, latdim),
                       missval = -3.4e+38)
  var_temp <- ncvar_def("TEMP", units = "degrees_Celsius", 
                        longname = "temperature", 
                        dim = list(londim, latdim, leveldim, timedim), 
                        missval = -3.4e+38)
  var_sig <- ncvar_def("SIGMA", units = "", dim = list(leveldim), missval = 1e+30)
  
  # Create new nc file 
  outfile_new <- paste0(directory_year_out, 
                        "DELFT3D_", str_replace_all(as.character(datestamp[timestep_i]), "-", ""), 
                        "T0000Z.nc")
  nc_new <- nc_create(
    filename = outfile_new,
    vars = list(var_temp, var_dep, var_sig)
  )
  
  # Put values in variables
  ncvar_put(
    nc = nc_new,
    varid = 'TEMP',
    vals = var_temp_array
  )
  ncvar_put(
    nc = nc_new,
    varid = 'DEPTH',
    vals = var_dep_array
  )
  
  ncvar_put(
    nc = nc_new,
    varid = 'SIGMA',
    vals = sigma
  )
  
  nc_close(nc_new)
  print(paste0("Saved in ", outfile_new))
})



