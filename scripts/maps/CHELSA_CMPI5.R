# CHELSA CMPI5 explore script
# Created on 06/03/2023 by Erica

# libraries -----
install.packages("ncdf4")
library(ncdf4)

# download data (from hard drive)
CMPI5_2006_29 <- nc_open("CHELSAcmip5ts_tasmax_ACCESS1-3_rcp85_2006-2029_V1.1.nc") 
CMPI5_2006_29 # explore info

# extracting lat lon
lon <- ncvar_get(CMPI5_2006_29, varid = "longitude")
lat <- ncvar_get(CMPI5_2006_29, varid = "latitude")

summary(lon)
#  Min.    1st Qu.     Median       Mean    3rd Qu. 
#-180.01688  -90.03088   -0.04489   -0.04489   89.94110 
#Max. 
#179.92710 

summary(lat) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-90.018 -46.530  -3.041  -3.041  40.448  83.937 

# exploring time
CMPI5_2006_29$dim$time$units #  "days since 2006-1-15 00:00:00"
CMPI5_2006_29$dim$time$calendar # "standard"

tas <- ncvar_get(CMPI5_2006_29, "air_temperature")


# exploring resolution 
res(CMPI5_2006_29) # resolution 

# exploring projection
projection(CMPI5_2006_29)

# crop to the extent of Katies maps

## CROPPING -----
# Cropping shrub raster to the PCH range 
crop_1 <- crop(CMPI5_2006_29, extent(p50_1985))
crop_CMPI5_2006_29 <- mask(crop_1, p50_1985)
plot(crop_CMPI5_2006_29) # plot cropped raster
