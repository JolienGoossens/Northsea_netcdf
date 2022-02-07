#### Script to create whole year of deltares data ####

#### Load packages ####
library(tidyverse)
library(terra)
library(ncdf4)
library(sf)

flag_plot = TRUE

#### Get coastline ####
iho = st_read("Data/external/iho", layer = "iho")

# Make mask from coastline
iho = iho %>% filter(name %in% c("English Channel", "Bay of Biscay", "Celtic Sea", "North Sea", "Bristol Channel"))

#### Set directory ####
directory <- "D:/Joligoos/OneDrive - UGent/WEc446/Joligoos/Documents/Doctoraat/Data/Data_Deltares/"
directory_out <- "Data/processed/DELFT3D/"

# Specify year
year_id = 2020
year_id = 2019
year_id = 2018
year_id = 2014

# Change directory to the year
directory_year = paste0(directory, year_id)
directory_year_out = paste0(directory_out, year_id, "/")


#### Save sigma and time variable ####
file_name = paste(directory_year, list.files(directory_year)[1], sep = "/")
nc_input <- nc_open(file_name)
sigma <- ncvar_get(nc_input, "mesh2d_layer_sigma")
time_seconds <- nc_input$dim['time']$time$vals
n_timestep = length(time_seconds)
nc_close(nc_input)
rm(file_name)

#### Make dates out of time stamps
timestart <- lubridate::parse_date_time("2011-12-22 00:00:00", orders = "ymd HMS")
datestamp <- timestart + time_seconds
datestamp <- datestamp %>% str_replace_all("-", "") %>% str_replace_all(" 12:00:00", "")

#### Make dataframe of depth and temperature data of all zones ####
if(year_id == 2014){
  domains = c("07", "09", "12")
} else {
  domains = c("02","12")
}
# Seem to only need 2 and 12 (leave 13 and 14 for now)
list_nc = lapply(domains, function(zone) {
  file_name = paste0(directory_year, "/DCSM-FM_0_5nm_00", zone, "_map.nc")
  
  # Get depth and temperature data of zone
  nc_input <- nc_open(file_name)
  lon <-  ncvar_get(nc_input, "mesh2d_face_x")
  lat <- ncvar_get(nc_input, "mesh2d_face_y")
  dep <- ncvar_get(nc_input, "mesh2d_waterdepth")
  temp <- ncvar_get(nc_input, "mesh2d_tem1")
  nc_close(nc_input)
  
  #### Make data frame of netcdf data ####
  # Depth data
  nc_df <- data.frame(
    timestep = rep(1:n_timestep, each = length(lon)),
    lon = rep(lon, n_timestep),
    lat = rep(lat, n_timestep),
    depth = as.vector(dep)
  )
  
  # Temperature data English Channel
  temp_list <- lapply(1:20, function(depth_layer) {
    temp <- as.vector(temp[depth_layer, , ])
  })
  names(temp_list) <- paste0("temp", seq(1:20))
  
  # Bind data 
  nc_df <- cbind(nc_df, temp_list)
  print(paste0("Finished zone ", zone))
  return(nc_df)
  
})

# Join the output
nc_df = do.call(rbind, list_nc)
rm(list_nc)

if(flag_plot){
  nc_df %>% 
    filter(timestep == 1) %>% 
    select(2,3) %>% 
    ggplot() +
    coord_quickmap() +
    geom_point(aes(lon,lat),
               colour = "palegreen4", size = 0.01)
}

#### Filter ####
nc_df = nc_df %>% filter(lat > 48.8 & lat < 52.2 & lon > -3.2 & lon < 4.7)

#### Put data on raster ####
# Create raster of appropriate resolution
rast_new_reshi = rast(xmin = -3, xmax = 4.5, ymin = 49, ymax= 52, resolution = c(0.0125, 0.00833))
rast_new_reslow = rast(xmin = -3, xmax = 4.5, ymin = 49, ymax= 52, resolution = c(0.0125, 0.00833)*2)

# Make mask
iho_v = iho %>% vect() # vectorize iho
iho_mask = rasterize(iho_v, rast_new_reshi) # make mask

# Define dimensions (other than time)
rast_crds = as.data.frame(crds(rast_new_reshi, na.rm = F))

lon <- unique(rast_crds$x)
lat <-unique(rast_crds$y) # rev? lat <- rev(unique(df_dist_all$lat))

londim <- ncdim_def("longitude","degrees_east", 
                    longname = "longitude", vals = lon) 
latdim <- ncdim_def("latitude","degrees_north", 
                    longname = "latitude", vals = lat)
leveldim <- ncdim_def("level", "layer", longname = "sigma_level", vals = 1:20)

# Make sf object of df
sf_df = st_as_sf(nc_df, 
                 coords = c("lon", "lat"),
                 crs = 4326)
rm(nc_df)

#### Make raster per day ####

lapply(48:n_timestep, function(timestep_i){
  print(paste0("Started with date ", datestamp[timestep_i]))
  # Filter for specific timestep
  sf_df_day = sf_df %>% filter(timestep == timestep_i)
  
  # Vectorize data to put on raster
  sf_df_v = sf_df_day %>% vect()
  
  # Perform rasterization for every layer (20 temp layers, 1 depth)
  list_rastlayers = lapply(2:length(names(sf_df_v)), function(layer_i){
    print(names(sf_df_v)[layer_i])
    # Rasterize data to the two resolutions
    rast_reshi = rasterize(sf_df_v, rast_new_reshi, field = names(sf_df_v)[layer_i], touches = T, update = T)
    rast_reslow = rasterize(sf_df_v, rast_new_reslow, field = names(sf_df_v)[layer_i], touches = T, update = T)
    
    # Add low resolution values when high is NA
    rast_reslow = resample(rast_reslow, rast_reshi) # put low resolution in high resolution (to have matching dimensions)
    rast_tot = terra::cover(rast_reshi, rast_reslow, values = NA)
    
    # Add mask
    rast_totmask = terra::mask(rast_tot, iho_mask)
    
    return(rast_totmask)
  })
  
  rast_all = rast(list_rastlayers)
  names(rast_all) = names(sf_df_v)[2:length(names(sf_df_v))]
  
  #### Make arrays from raster ####
  var_dep_array <- as.array(t(rast_all$depth))
  var_temp_array <- as.array(t(rast_all[[c(2:21)]]))
  
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
                        "DELFT3D_", datestamp[timestep_i], 
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


#### Stuur Firmijn ####
# 2018: data 20181226 - 20181231 ontbreken

