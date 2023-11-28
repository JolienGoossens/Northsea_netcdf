#### Script to create whole year of deltares data ####

#### Load packages ####
library(tidyverse)
library(terra)
library(ncdf4)
library(sf)

flag_plot = FALSE


#### Get file ####
file_name= "C:/Users/joligoos/geoloc/data/COPERNICUS data/cmems_mod_nws_phy-t_my_7km-3D_P1D-m_2021.nc"

cop = raster::raster("C:/Users/joligoos/geoloc/data/COPERNICUS data/cmems_mod_nws_phy-t_my_7km-3D_P1D-m_2021.nc")

cop_df = raster::rasterToPoints(cop)
cop_df = as.data.frame(cop_df)

plot_cop = cop_df %>% 
  select(1,2) %>% 
  ggplot() +
  theme_bw() +
  coord_quickmap() +
  scale_x_continuous(expand = c(0.01,0.01), breaks = seq(-4, 4,2)) +
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(46, 54, 2)) +
  geom_point(aes(x,y),
             colour = "palegreen4", size = 0.01) +
  labs(x = "", y = "")

ggsave(filename = "Figures/Methods_copspatial.jpg", 
       plot = plot_cop,   
       scale = 1, dpi = 600, width = 15, height = 20, units = "cm")



