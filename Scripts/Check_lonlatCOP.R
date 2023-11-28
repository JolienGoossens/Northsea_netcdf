library(ncdf4)

nc_input <- nc_open("C:/Users/joligoos/geoloc/data/COPERNICUS data/cmems_mod_nws_phy-t_my_7km-3D_P1D-m_2021.nc")
nc_input <- nc_open("C:/Users/joligoos/geoloc/data/COPERNICUS data/cmems_mod_nws_phy-t_my_7km-3D_P1D-m_2016.nc")

lon <-  ncvar_get(nc_input, "longitude")
lat <- ncvar_get(nc_input, "latitude")


range(lon)
range(lat)

#> range(lon)
#[1] -5.88903  6.99973
#> range(lat)
#[1] 45.06692 55.00075