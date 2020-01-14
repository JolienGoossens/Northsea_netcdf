#### Load packages and adapted code####
library(lubridate)
library(raster)
library(ncdf4)
library(sp)
library(rgdal)
library(plyr)
library(dplyr)

#### Read nc data ####
file_name <- "Data/external/DCSM-FM_0_5nm_0015_map.nc"
nctry <- nc_open(file_name)
lon <-  ncvar_get(nctry, "mesh2d_face_x")
lat <- ncvar_get(nctry, "mesh2d_face_y")

head(lon)
head(lat)
data.frame(lon,lat)
time <- ncvar_get(nctry, "timestep")
dep <- ncvar_get(nctry, "mesh2d_waterdepth")
temp <- ncvar_get(nctry, "mesh2d_tem1")
n_faces <- ncvar_get(nctry, "mesh2d_nFaces")
head(wg)
nc_close(nctry)


plot(lon, lat, dep[,1])

raster(lon, lat, dep[,1])


dep1 <-  dep[,1]
temp1 <- temp[1,,1]
deprast <- data.frame(
  dep1 = dep[,1],
  lon = lon,
  lat = lat,
  temp1 = temp[1,,1],
  temp10 = temp[10,,1]
)
str(deprast)
# coordinates(deprast) <- ~lon+lat
# r <- raster(deprast)
# plot(r)

library(ggplot2)
ggplot() +
  coord_map() +
  geom_point(aes(lon,lat, colour = dep1))

deprast <- filter(deprast, lat < 55)
ggplot(deprast) +
  coord_map() +
  geom_point(aes(lon,lat, colour = temp10))


nchyc <- nc_open("D:/Joligoos/OneDrive - UGent/WEc446/Joligoos/Documents/Shark_explore/Data/external/hycom/Joined/hycom_2014-06-06.nc")
hyctemp <- ncvar_get(nchyc, "water_temp")
hyclat <- ncvar_get(nchyc, "lat")
hyclon <- ncvar_get(nchyc, "lon")
data.frame(hyclat,hyclon)

hyctemp[,,4]
nc_close(nchyc)

file_name <- "Data/external/MARC_F1-MARS3D-ARMOR_20110101T0000Z.nc"
ncmars <- nc_open(file_name)

temp <- ncvar_get(ncmars, "")
nj

lat <- ncvar_get(ncmars, "latitude")
lon <- ncvar_get(ncmars, "longitude")



lat <- lat[,1]
lon <- lon[1,]
tmp(rnorm)
rnorm(8*5)

library(ggplot2)
ggplot() +
  geom_point(aes(lon,lat))
temp1 <- temp[,,1]
