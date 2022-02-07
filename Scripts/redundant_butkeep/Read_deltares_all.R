#### Script to create whole year of deltares data ####

#### Load packages ####
library(tidyverse)
library(raster)
library(ncdf4)
library(sp)
library(rgdal)
library(akima)
library(geosphere)
library(gridExtra)

flag_plot = TRUE


#### Read nc data ####
directory <- "D:/Joligoos/OneDrive - UGent/WEc446/Joligoos/Documents/Doctoraat/Data/Data_Deltares"
file_name_ec <- paste(directory, "DCSM-FM_0_5nm_0006_map.nc", sep = "/") # English Channel
file_name_ns <- paste(directory, "DCSM-FM_0_5nm_0015_map.nc", sep = "/") # southern North Sea

# English Channel
nc_input <- nc_open(file_name_ec)
lon_ec <-  ncvar_get(nc_input, "mesh2d_face_x")
lat_ec <- ncvar_get(nc_input, "mesh2d_face_y")
dep_ec <- ncvar_get(nc_input, "mesh2d_waterdepth")
temp_ec <- ncvar_get(nc_input, "mesh2d_tem1")
nc_close(nc_input)

# southern North Sea
nc_input <- nc_open(file_name_ns)
lon_ns <-  ncvar_get(nc_input, "mesh2d_face_x")
lat_ns <- ncvar_get(nc_input, "mesh2d_face_y")
dep_ns <- ncvar_get(nc_input, "mesh2d_waterdepth")
temp_ns <- ncvar_get(nc_input, "mesh2d_tem1")
nc_close(nc_input)


# Save sigma and time variable
nc_input <- nc_open(file_name_ns)
sigma <- ncvar_get(nc_input, "mesh2d_layer_sigma")
time_seconds <- nc_input$dim['time']$time$vals
n_days = length(time_seconds)
nc_close(nc_input)

rm(directory, file_name_ec, file_name_ns, nc_input)

#### Make data frame of netcdf data ####
# Depth data English Channel
# nc_df_ec <- data.frame(
#   depth = dep_ec[,daynr],
#   lon = lon_ec,
#   lat = lat_ec
# )

nc_df_ec <- data.frame(
  day = rep(1:n_days, each = length(lon_ec)),
  lon = rep(lon_ec, n_days),
  lat = rep(lat_ec, n_days),
  depth = as.vector(dep_ec)
)

# Temperature data English Channel
# temp_list_ec <- lapply(1:20, function(depth_layer) {
#   temp <- temp_ec[depth_layer, , daynr]
# })
temp_list_ec <- lapply(1:20, function(depth_layer) {
  temp <- as.vector(temp_ec[depth_layer, , ])
})
names(temp_list_ec) <- paste0("temp", seq(1:20))

# Bind data Englisch Channel
nc_df_ec <- cbind(nc_df_ec, temp_list_ec)
nc_df_ec <- nc_df_ec %>% filter(lat >49 & lat < 51)

# Depth data southern North Sea
nc_df_ns <- data.frame(
  day = rep(1:n_days, each = length(lon_ns)),
  lon = rep(lon_ns, n_days),
  lat = rep(lat_ns, n_days),
  depth = as.vector(dep_ns)
)

# Temperature data southern North Sea
temp_list_ns <- lapply(1:20, function(depth_layer) {
  temp <- as.vector(temp_ns[depth_layer, , ])
})
names(temp_list_ns) <- paste0("temp", seq(1:20))

# Bind data southern North Sea
nc_df_ns <- cbind(nc_df_ns, temp_list_ns)
nc_df_ns <- nc_df_ns %>% filter(lat < 53)

# Join data
nc_df <- rbind(nc_df_ec, nc_df_ns)

# Plot input and output
if (flag_plot) {
  # Input
  p1 <- ggplot() +
    coord_quickmap()+
    geom_point(data = filter(nc_df_ns, day == 1), 
               aes(lon, lat),
               colour = "palegreen4", size = 0.01) +
    geom_point(data = filter(nc_df_ec, day == 1),
               aes(lon, lat), 
               colour = "blue", size = 0.01)
  # Output
  p2 <-ggplot() +
    coord_quickmap()+
    geom_point(data = filter(nc_df, day == 1), 
               aes(lon, lat),
               colour = "black", size = 0.01)
  grid.arrange(p1, p2, nrow = 1)
  rm(p1, p2)
}
  
# Remove what you don't need
rm(nc_df_ns, lat_ns, lon_ns, dep_ns, temp_ns, temp_list_ns,
   nc_df_ec, lat_ec, lon_ec, dep_ec, temp_ec, temp_list_ec)

#### Adjust resolution ####
# Check resolution
# nc_df_check <- nc_df %>% filter(lon < -0.45 & lon > -0.55 & lat < 50.8 & lat > 50.71)
# summary(diff(nc_df_check$lon)) #0.0125
# summary(diff(nc_df_check$lat)) #0.008333
# rm(nc_df_check)

# Make grid in smallest resolution
# grid_total <- expand.grid(lon = seq(min(nc_df$lon), max(nc_df$lon), 0.0125),
#                           lat = seq(min(nc_df$lat), max(nc_df$lat), 0.008333))
# # Plot to check
# if (flag_plot) {
#   ggplot() +
#     theme_bw() +
#     geom_point(data = grid_total, aes(lon, lat), colour = "red", size = 0.1) +
#     geom_point(data = filter(nc_df, day == 1), 
#                aes(lon, lat), colour = "black", size = 0.1)
# }

# Filter grid for needed points

# list_dist <- lapply(1:nrow(grid_total), function(i){
#   grid_temp <- grid_total[i,]
#   nc_df_temp <- nc_df %>% 
#     dplyr::select(lon,lat) %>% 
#     filter(grid_temp$lon - 0.2 < lon & grid_temp$lon + 0.2 > lon &
#                                        grid_temp$lat - 0.2 < lat & grid_temp$lat + 0.2 > lat)
#   if (nrow(nc_df_temp) == 0) {
#     grid_temp$diflon <- NA
#     grid_temp$diflat <- NA
#     grid_temp$keep = F
#   } else {
#     index_mindist <- which.min(distHaversine(cbind(nc_df$lon, nc_df$lat),
#                                              c(grid_temp$lon, grid_temp$lat)))
#     grid_temp$diflon <- abs(nc_df[index_mindist,]$lon - grid_temp$lon)
#     grid_temp$diflat <- abs(nc_df[index_mindist,]$lat - grid_temp$lat)
#     grid_temp$keep <- ifelse(grid_temp$diflon <= 0.0125 & grid_temp$diflat <= 0.008333, T,F)
#   }
#   return(grid_temp)
# })
# 
# df_dist_all <- plyr::ldply(list_dist)
# write.csv(df_dist_all, "Data/interim/df_dist_all.csv", row.names = F)
#rm(list_dist)

df_dist_all <- read.csv("Data/interim/df_dist_all.csv", stringsAsFactors = F)
df_dist <- df_dist_all %>% filter(keep == T)

if (flag_plot) {
  ggplot() +
    theme_bw() +
    geom_point(data = filter(nc_df, day == 1), 
               aes(lon, lat), colour = "black", size = 0.1) +
    geom_point(data = df_dist, aes(lon, lat), colour = "red", size = 0.1)
}

# Interpolation of temperature
list_interp_alldays <- lapply(1:n_days, function(daynr){
  nc_df_sub <- nc_df %>% filter(day == daynr)
  temp_interpp_list <- lapply(1:20, function(depth_layer) {
    temp_var <- paste("temp", depth_layer, sep = "")
    interpp_grid <- interpp(nc_df_sub$lon, nc_df_sub$lat, nc_df_sub[[temp_var]],
                            xo= df_dist$lon, yo = df_dist$lat,
                            extrap = F, duplicate = "strip")
    return(interpp_grid[["z"]])
  })
  names(temp_interpp_list) <- paste0("temp", seq(1:20))
  nc_df_interpp <- df_dist %>% 
    dplyr::select(lon, lat) %>% 
    cbind(temp_interpp_list)
  rm(temp_interpp_list)
  print(paste0("Temperature done for day ", daynr))
  
  interpp_grid <- interpp(nc_df_sub$lon, nc_df_sub$lat, nc_df_sub$depth,
                          xo= df_dist$lon, yo = df_dist$lat,
                          extrap = F, duplicate = "strip")
  
  nc_df_interpp <- nc_df_interpp %>% 
    mutate(depth = interpp_grid[["z"]])
  rm(interpp_grid)
  print(paste0("Depth done for day ", daynr))
  nc_df_interpp$day <- daynr
  return(nc_df_interpp)
})
df_interp_alldays <- plyr::ldply(list_interp_alldays)
rm(list_interp_alldays)

if (flag_plot) {
  p1 <- ggplot() +
    coord_quickmap()+
    geom_point(data = filter(df_interp_alldays, day == 1), 
               aes(lon,lat, colour = temp20), size= 0.1) +
    ggtitle("Temperature")
  
  p2 <- ggplot() +
    coord_quickmap()+
    geom_point(data = filter(df_interp_alldays, day == 1),
               aes(lon,lat, colour = depth), size= 0.1) + 
    ggtitle("Depth")
  grid.arrange(p1, p2, nrow = 1)
  rm(p1, p2)
}

#### Create new netcdf files ####
# Define dimensions (other than time)
lon <- unique(df_dist_all$lon)
lat <- rev(unique(df_dist_all$lat))

londim <- ncdim_def("longitude","degrees_east", 
                    longname = "longitude", vals = lon) 
latdim <- ncdim_def("latitude","degrees_north", 
                    longname = "latitude", vals = lat)
leveldim <- ncdim_def("level", "layer", longname = "sigma_level", vals = 1:20)

# Directory setting
directory_out <- "Data/processed/"
timestart <- lubridate::parse_date_time("2011-12-22 00:00:00", orders = "ymd HMS")
datestamp <- timestart + time_seconds
datestamp <- str_replace_all(datestamp, "-", "")

# Create new netcdf files for each day
lapply(1:n_days, function(daynr) {
  # Subset
  df_interp_sub <- df_interp_alldays %>% 
    filter(day == daynr) %>% 
    dplyr::select(-day)
  
  # Rasterize #
  nc_df_interpp_full <- df_dist_all %>% 
    dplyr::select(-diflon, -diflat) %>% 
    full_join(df_interp_sub)
  
  # Temperature
  nc_df_interpp_full_temp <- nc_df_interpp_full %>% 
    dplyr::select(-keep, -depth)
  
  coordinates(nc_df_interpp_full_temp) <- ~ lon + lat
  gridded(nc_df_interpp_full_temp) <- T
  raster_df_temp <- brick(nc_df_interpp_full_temp)
  
  # Store temperature variable
  var_temp_array <- as.array(t(raster_df_temp))
  rm(raster_df_temp)
  
  # Depth
  nc_df_interpp_full_dep <- nc_df_interpp_full %>% 
    dplyr::select(lon, lat, depth)
  coordinates(nc_df_interpp_full_dep) <- ~ lon + lat
  gridded(nc_df_interpp_full_dep) <- T
  raster_df_dep <- raster(nc_df_interpp_full_dep)
  
  # Store depth variable
  var_dep_array <- as.matrix(t(raster_df_dep))
  rm(raster_df_dep)
  
  
  # Define time dimension
  timedim <- ncdim_def("time", "seconds since 2011-12-22 00:00:00", longname = 'time',
                       vals = time_seconds[daynr])
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
  outfile_new <- paste0(directory_out, 
                        "DELFT3D_", datestamp[daynr], 
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


