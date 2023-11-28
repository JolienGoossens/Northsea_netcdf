#### Script to create whole year of deltares data ####

#### Load packages ####
library(tidyverse)
library(terra)
library(ncdf4)
library(sf)

flag_plot = FALSE


#### Set directory ####
directory <- "D:/Joligoos/OneDrive - UGent/WEc446/Joligoos/Documents/Doctoraat/Data/Data_Deltares/"

# Specify year
year_id = 2020

# Change directory to the year
directory_year = paste0(directory, year_id)

#### Make dataframe of depth and temperature data of all zones ####
domains = c("02","12", "13", "14")

# Get depth and temperature data of zone
list_nc = lapply(domains, function(zone) {
  file_name = paste0(directory_year, "/DCSM-FM_0_5nm_00", zone, "_map.nc")
  
  nc_input <- nc_open(file_name)
  lon <-  ncvar_get(nc_input, "mesh2d_face_x")
  lat <- ncvar_get(nc_input, "mesh2d_face_y")
  dep <- ncvar_get(nc_input, "mesh2d_waterdepth")
  nc_close(nc_input)
  
  nc_df <- data.frame(
    lon = lon,
    lat = lat,
    depth = as.vector(dep[,2])
  )
  
  print(paste0("Finished zone ", zone))
  return(nc_df)
  
})

# Join the output
nc_df = do.call(rbind, list_nc)
rm(list_nc)
gc()

#### Filter ####
nc_df = nc_df %>% filter(lat > 48.8 & lat < 53.5 & lon > -3.2 & lon < 5)

plot_delft = nc_df %>% 
  select(1,2) %>% 
  ggplot() +
  theme_bw() +
  coord_quickmap() +
  scale_x_continuous(expand = c(-0.01,-0.01), breaks = seq(-3, 4)) +
  scale_y_continuous(expand = c(-0.01,-0.01)) +
  geom_point(aes(lon,lat),
             colour = "palegreen4", size = 0.001) +
  labs(x = "", y = "")

ggsave(filename = "Figures/Methods_delftspatial.jpg", 
       plot = plot_delft,   
       scale = 1, dpi = 600, width = 15, height = 20, units = "cm")



