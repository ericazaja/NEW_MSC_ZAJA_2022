# CMPI6 download 
# Created on 06/03/2023 by Erica
# following tutorial:
# https://michaelminn.net/tutorials/r-climate/index.html

# Loading libraries -----
library(ncdf4)
library(raster)
library(tidyr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(RNetCDF)
library(sf)
library(tsbox)

# Download data ------

# Model 1: NOAA-GFDL Institution, experiment ssp585 (8.5 RCP), monthly (mon), tas max (tasmax daily-maximum near-surface (usually, 2 meter) air temperature (K))
nc_NOAA_85 = open.nc("data/CMPI6/tasmax_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012.nc")

# Model 2:  CMCC | ssp585 | mon | tasmax
nc_CMCC_85 = open.nc("data/CMPI6/tasmax_Amon_CMCC-ESM2_ssp585_r1i1p1f1_gn_201501-210012.nc")

# Model 3:  MRI | ssp585 | mon | tasmax 
nc_MRI_85 = open.nc("data/CMPI6/tasmax_Amon_MRI-ESM2-0_ssp585_r1i2p1f1_gn_201501-210012.nc")

# Model 4:   MIROC | ssp585 | mon | tasmax
nc_MIROC_85 = open.nc("data/CMPI6/tasmax_Amon_MIROC-ES2L_ssp585_r2i1p1f2_gn_201501-210012.nc")

# Model 5:  CNRM-CERFACS | ssp585 | mon | tasmax
nc_CNRM_85 = open.nc("data/CMPI6/tasmax_Amon_CNRM-CM6-1_ssp585_r1i1p1f2_gr_201501-210012.nc")

# Model 6:   ACCESS-CM2 | ssp585 | mon | tasmax 
nc_ACCESS_85 = open.nc("data/CMPI6/tasmax_Amon_ACCESS-CM2_ssp585_r2i1p1f1_gn_201501-210012.nc") 

# boundary of Katie's maps
boundary_highest <- st_read("data/shapefiles/katie_map_border_highest.shp")

# Process the Data -----

# Model 1: NOAA ------
tasmax.dates.1= as.Date(var.get.nc(nc_NOAA_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.1 = sapply(1:length(tasmax.dates.1), function(z) {
  grid = var.get.nc(nc_NOAA_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_NOAA_85)

plot(tasmax.scenes.1[[1029]], main="NOAA, RCP 8.5", sub =tasmax.dates.1[[1029]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))


# Model 2: CMCC -------
tasmax.dates.2 = as.Date(var.get.nc(nc_CMCC_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.2 = sapply(1:length(tasmax.dates.2), function(z) {
  grid = var.get.nc(nc_CMCC_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_CMCC_85)

plot(tasmax.scenes.2[[1029]], main="CMCC, RCP 8.5",  sub=tasmax.dates.2[[1029]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# Model 3: MRI (might remove this) ------
tasmax.dates.3 = as.Date(var.get.nc(nc_MRI_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.3 = sapply(1:length(tasmax.dates.3), function(z) {
  grid = var.get.nc(nc_MRI_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_MRI_85)

plot(tasmax.scenes.3[[1027]], main = "MRI, RCP 8.5", sub=tasmax.dates.3[[1027]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# Model 4: MIROC -------
tasmax.dates.4 = as.Date(var.get.nc(nc_MIROC_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.4 = sapply(1:length(tasmax.dates.4), function(z) {
  grid = var.get.nc(nc_MIROC_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_MIROC_85)

plot(tasmax.scenes.4[[1027]],main = "MIROC, RCP 8.5", sub=tasmax.dates.4[[1027]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# Model 5: CNRM -------
tasmax.dates.5 = as.Date(var.get.nc(nc_CNRM_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.5 = sapply(1:length(tasmax.dates.5), function(z) {
  grid = var.get.nc(nc_CNRM_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_CNRM_85)

plot(tasmax.scenes.5[[1027]], main = "CNRM, RCP 8.5",sub=tasmax.dates.5[[1027]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# Model 6: ACCESS ----
tasmax.dates.6 = as.Date(var.get.nc(nc_ACCESS_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.6 = sapply(1:length(tasmax.dates.6), function(z) {
  grid = var.get.nc(nc_ACCESS_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_ACCESS_85)

plot(tasmax.scenes.6[[1027]], main = "ACCESS, RCP 8.5",sub=tasmax.dates.6[[1027]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# EXTRACTING rasters by year ------

# # creating raster with resolution I want
x <- raster()
x <- raster(xmn=-154.8826 , xmx=-127.0697, ymn=59.43991 , ymx=71.49051)
res(x) <- 0.01 # 1 km x 1km 
res(x)
projection(x) <- "+proj=longlat +datum=WGS84 +no_defs"

#.--------

# JULY 2020 ------
# MODEL 1 July 2020 ----

indices.1 = which((tasmax.dates.1 <= as.Date(paste0("2020-07-31"))) & 
                  (tasmax.dates.1 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.1 = tasmax.scenes.1[[indices.1[1]]] 
res(tasmax.2020.1)# 1.25 1.00 degrees
projection(tasmax.2020.1) #"+proj=longlat +datum=WGS84 +no_defs"
tasmax.2020.1.re <- resample(tasmax.2020.1, x) # x is the raster I made 

# crop and mask
r2.1 <- crop(tasmax.2020.1.re, extent(boundary_highest))
r3.1 <- mask(r2.1, boundary_highest)

## Check that it worked
plot(r3.1)
plot(boundary_high, add=TRUE, lwd=2)

plot(r3.1, main="July 2020, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

#  MODEL 2, July 2020 ------

indices.2 = which((tasmax.dates.2 <= as.Date(paste0("2020-07-31"))) & 
                  (tasmax.dates.2 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.2 = tasmax.scenes.2[[indices.2[1]]] 
res(tasmax.2020.2)# 1.2500 0.9375 degrees
projection(tasmax.2020.2) #"+proj=longlat +datum=WGS84 +no_defs"
# make size same as other climate raster (NOAA)
tasmax.2020.2.re <- resample(tasmax.2020.2, x) # hdd cdd is the climate raster
res(tasmax.2020.2.re) 

## crop and mask
r2.2 <- crop(tasmax.2020.2.re, extent(boundary_high))
r3.2 <- mask(r2.2, boundary_high)

## Check that it worked
plot(r3.2)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2, main="July 2020, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

#  MODEL 3, July 2020 ------

indices.3  = which((tasmax.dates.3 <= as.Date(paste0("2020-07-31"))) & 
                  (tasmax.dates.3 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.3 = tasmax.scenes.3[[indices.3[1]]] 
res(tasmax.2020.3) # 1.125 1.125 degrees
projection(tasmax.2020.3) #"+proj=longlat +datum=WGS84 +no_defs"
# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range
# make size same as other climate raster (NOAA)
tasmax.2020.3.re <- resample(tasmax.2020.3, x) # hdd cdd is the climate raster

## crop and mask
r2.3 <- crop(tasmax.2020.3.re, extent(boundary_high))
r3.3 <- mask(r2.3, boundary_high)

## Check that it worked
plot(r3.3)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3, main="July 2020, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2020 ------

indices.4  = which((tasmax.dates.4 <= as.Date(paste0("2020-07-31"))) & 
                     (tasmax.dates.4 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.4 = tasmax.scenes.4[[indices.4[1]]] 
res(tasmax.2020.4)# 2.8125 2.8125 degrees
projection(tasmax.2020.4) #"+proj=longlat +datum=WGS84 +no_defs"
# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range
# make size same as other climate raster (NOAA)
tasmax.2020.4.re <- resample(tasmax.2020.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4 <- crop(tasmax.2020.4.re, extent(boundary_high))
r3.4 <- mask(r2.4, boundary_high)

## Check that it worked
plot(r3.4)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4, main="July 2020, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2020  ------

indices.5  = which((tasmax.dates.5 <= as.Date(paste0("2020-07-31"))) & 
                     (tasmax.dates.5 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.5 = tasmax.scenes.5[[indices.5[1]]] 
res(tasmax.2020.5)# 1.40625 1.40625 degrees
projection(tasmax.2020.5) #"+proj=longlat +datum=WGS84 +no_defs"
# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range
tasmax.2020.5.re <- resample(tasmax.2020.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5 <- crop(tasmax.2020.5.re, extent(boundary_high))
r3.5 <- mask(r2.5, boundary_high)

## Check that it worked
plot(r3.5)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5, main="July 2023, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# MODEL 6, July 2020 ------

indices.6  = which((tasmax.dates.6 <= as.Date(paste0("2020-07-31"))) & 
                     (tasmax.dates.6 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.6 = tasmax.scenes.6[[indices.6[1]]] 
res(tasmax.2020.6)# 1.125 1.125 degrees
projection(tasmax.2020.6) #"+proj=longlat +datum=WGS84 +no_defs"
# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range
tasmax.2020.6.re <- resample(tasmax.2020.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6 <- crop(tasmax.2020.6.re, extent(boundary_high))
r3.6 <- mask(r2.6, boundary_high)

## Check that it worked
plot(r3.6)
plot(boundary_high, add=TRUE, lwd=2)

plot(r3.6, main="July 2023, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2020.1.re, "outputs/CMPI6_rasters/tasmax.2020.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2020.2.re, "outputs/CMPI6_rasters/tasmax.2020.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2020.3.re, "outputs/CMPI6_rasters/tasmax.2020.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2020.4.re, "outputs/CMPI6_rasters/tasmax.2020.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2020.5.re, "outputs/CMPI6_rasters/tasmax.2020.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2020.6.re, "outputs/CMPI6_rasters/tasmax.2020.6.re.tif", overwrite = TRUE)

#.--------

# JULY 2030 -------
# MODEL 1, July 2030 ----
indices.1.1 = which((tasmax.dates.1 <= as.Date(paste0("2030-07-31"))) & 
                    (tasmax.dates.1 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.1 = tasmax.scenes.1[[indices.1.1[1]]] 
tasmax.2030.1.re <- resample(tasmax.2030.1, x) # hdd cdd is the climate raster

# crop and mask
r2.1.1 <- crop(tasmax.2030.1.re, extent(boundary_high))
r3.1.1 <- mask(r2.1.1, boundary_high)

## Check that it worked
plot(r3.1.1)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.1, main="July 2030, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

#  MODEL 2, July 2030 ------

indices.2.2 = which((tasmax.dates.2 <= as.Date(paste0("2030-07-31"))) & 
                    (tasmax.dates.2 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.2 = tasmax.scenes.2[[indices.2.2[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2030.2.re <- resample(tasmax.2030.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2030 <- crop(tasmax.2030.2.re, extent(boundary_high))
r3.2030 <- mask(r2.2030, boundary_high)

## Check that it worked
plot(r3.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2030, main="July 2030, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

#  MODEL 3, July 2030 ------

indices.3.3  = which((tasmax.dates.3 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.3 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.3 = tasmax.scenes.3[[indices.3.3[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2030.3.re <- resample(tasmax.2030.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2030 <- crop(tasmax.2030.3.re, extent(boundary_high))
r3.3.2030 <- mask(r3.2030, boundary_high)

## Check that it worked
plot(r3.3.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2030, main="July 2030, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2030 ------

indices.4.4  = which((tasmax.dates.4 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.4 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.4 = tasmax.scenes.4[[indices.4.4[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2030.4.re <- resample(tasmax.2030.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2030 <- crop(tasmax.2030.4.re, extent(boundary_high))
r3.4.2030 <- mask(r2.4.2030, boundary_high)

## Check that it worked
plot(r3.4.2030)
plot(boundary_high, add=TRUE, lwd=2)

plot(r3.4.2030, main="July 2030, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2030  ------

indices.5.5  = which((tasmax.dates.5 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.5 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.5 = tasmax.scenes.5[[indices.5.5[1]]] 

tasmax.2030.5.re <- resample(tasmax.2030.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2030 <- crop(tasmax.2030.5.re, extent(boundary_high))
r3.5.2030 <- mask(r2.5.2030, boundary_high)

## Check that it worked
plot(r3.5.2030)
plot(boundary_high, add=TRUE, lwd=2)

plot(r3.5.2030, main="July 2030, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# MODEL 6, July 2030 ------

indices.6.6  = which((tasmax.dates.6 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.6 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.6 = tasmax.scenes.6[[indices.6.6[1]]] 
tasmax.2030.6.re <- resample(tasmax.2030.6, x) # hdd cdd is the climate raster
## crop and mask
r2.6.2030 <- crop(tasmax.2030.6.re, extent(boundary_high))
r3.6.2030 <- mask(r2.6.2030, boundary_high)

## Check that it worked
plot(r3.6.2030)
plot(boundary_high, add=TRUE, lwd=2)

plot(r3.6.2030, main="July 2030, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2030.1.re, "outputs/CMPI6_rasters/tasmax.2030.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2030.2.re, "outputs/CMPI6_rasters/tasmax.2030.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2030.3.re, "outputs/CMPI6_rasters/tasmax.2030.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2030.4.re, "outputs/CMPI6_rasters/tasmax.2030.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2030.5.re, "outputs/CMPI6_rasters/tasmax.2030.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2030.6.re, "outputs/CMPI6_rasters/tasmax.2030.6.re.tif", overwrite = TRUE)

#.--------

# JULY 2040 -------
# MODEL 1, July 2040 -----
indices.40 = which((tasmax.dates.1 <= as.Date(paste0("2040-07-31"))) & 
                  (tasmax.dates.1>= as.Date(paste0("2040-07-01"))))

tasmax.2040.1 = tasmax.scenes.1[[indices.40[1]]] 
tasmax.2040.1.re <- resample(tasmax.2040.1, x) # hdd cdd is the climate raster

## crop and mask
r2.1.2040 <- crop(tasmax.2040.1.re, extent(boundary_high))
r3.1.2040 <- mask(r2.1.2040, boundary_high)

## Check that it worked
plot(r3.1.2040)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.2040, main="July 2040, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# MODEL 2, July 2040 -----
indices.2.2040 = which((tasmax.dates.2 <= as.Date(paste0("2040-07-31"))) & 
                      (tasmax.dates.2 >= as.Date(paste0("2040-07-01"))))

tasmax.2040.2 = tasmax.scenes.2[[indices.2.2040[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2040.2.re <- resample(tasmax.2040.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2040 <- crop(tasmax.2040.2.re, extent(boundary_high))
r3.2040 <- mask(r2.2040, boundary_high)

## Check that it worked
plot(r3.2040)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2040, main="July 2040, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


#  MODEL 3, July 2040 ------
indices.3.2040  = which((tasmax.dates.3 <= as.Date(paste0("2040-07-31"))) & 
                       (tasmax.dates.3 >= as.Date(paste0("2040-07-01"))))

tasmax.2040.3 = tasmax.scenes.3[[indices.3.2040[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2040.3.re <- resample(tasmax.2040.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2040 <- crop(tasmax.2040.3.re, extent(boundary_high))
r3.3.2040 <- mask(r3.2040, boundary_high)

## Check that it worked
plot(r3.3.2040)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2040, main="July 2040, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2040 ------

indices.4.2040  = which((tasmax.dates.4 <= as.Date(paste0("2040-07-31"))) & 
                       (tasmax.dates.4 >= as.Date(paste0("2040-07-01"))))

tasmax.2040.4 = tasmax.scenes.4[[indices.4.2040[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2040.4.re <- resample(tasmax.2040.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2040 <- crop(tasmax.2030.4.re, extent(boundary_high))
r3.4.2040 <- mask(r2.4.2040, boundary_high)

## Check that it worked
plot(r3.4.2040)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2040, main="July 2040, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# MODEL 5, July 2040  ------

indices.5.2040  = which((tasmax.dates.5 <= as.Date(paste0("2040-07-31"))) & 
                       (tasmax.dates.5 >= as.Date(paste0("2040-07-01"))))

tasmax.2040.5 = tasmax.scenes.5[[indices.5.2040[1]]] 

tasmax.2040.5.re <- resample(tasmax.2040.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2040 <- crop(tasmax.2040.5.re, extent(boundary_high))
r3.5.2040 <- mask(r2.5.2040, boundary_high)

## Check that it worked
plot(r3.5.2040)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2040, main="July 2040, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# MODEL 6, July 2040 ------

indices.6.2040 = which((tasmax.dates.6 <= as.Date(paste0("2040-07-31"))) & 
                       (tasmax.dates.6 >= as.Date(paste0("2040-07-01"))))

tasmax.2040.6 = tasmax.scenes.6[[indices.6.2040[1]]] 
tasmax.2040.6.re <- resample(tasmax.2040.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2040 <- crop(tasmax.2040.6.re, extent(boundary_high))
r3.6.2040 <- mask(r2.6.2040, boundary_high)

## Check that it worked
plot(r3.6.2040)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2040, main="July 2040, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2040.1.re, "outputs/CMPI6_rasters/tasmax.2040.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2040.2.re, "outputs/CMPI6_rasters/tasmax.2040.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2040.3.re, "outputs/CMPI6_rasters/tasmax.2040.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2040.4.re, "outputs/CMPI6_rasters/tasmax.2040.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2040.5.re, "outputs/CMPI6_rasters/tasmax.2040.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2040.6.re, "outputs/CMPI6_rasters/tasmax.2040.6.re.tif", overwrite = TRUE)

#.--------

# JULY 2050 -------
# MODEL 1, July 2050 ------
indices.1.2050 = which((tasmax.dates.1 <= as.Date(paste0("2050-07-31"))) & 
                  (tasmax.dates.1 >= as.Date(paste0("2050-07-01"))))

tasmax.2050.1 = tasmax.scenes.1[[indices.1.2050[1]]] 
tasmax.2050.1.re <- resample(tasmax.2050.1, x) # hdd cdd is the climate raster

## crop and mask
r1.2.2050 <- crop(tasmax.2050.1.re, extent(boundary_high))
r1.3.2050 <- mask(r1.2.2050, boundary_high)

## Check that it worked
plot(r1.3.2050)
plot(boundary, add=TRUE, lwd=2)
plot(r1.3.2050, main="July 2050, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 2, July 2050 -----
indices.2.2050 = which((tasmax.dates.2 <= as.Date(paste0("2050-07-31"))) & 
                         (tasmax.dates.2 >= as.Date(paste0("2050-07-01"))))

tasmax.2050.2 = tasmax.scenes.2[[indices.2.2050[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2050.2.re <- resample(tasmax.2050.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2050 <- crop(tasmax.2040.2.re, extent(boundary_high))
r3.2050 <- mask(r2.2050, boundary_high)

## Check that it worked
plot(r3.2050)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2050, main="July 2050, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# MODEL 3, July 2050 ------
indices.3.2050  = which((tasmax.dates.3 <= as.Date(paste0("2050-07-31"))) & 
                          (tasmax.dates.3 >= as.Date(paste0("2050-07-01"))))

tasmax.2050.3 = tasmax.scenes.3[[indices.3.2050[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2050.3.re <- resample(tasmax.2050.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2050 <- crop(tasmax.2050.3.re, extent(boundary_high))
r3.3.2050 <- mask(r3.2050, boundary_high)

## Check that it worked
plot(r3.3.2050)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2050, main="July 2050, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2050 ------

indices.4.2050  = which((tasmax.dates.4 <= as.Date(paste0("2050-07-31"))) & 
                          (tasmax.dates.4 >= as.Date(paste0("2050-07-01"))))

tasmax.2050.4 = tasmax.scenes.4[[indices.4.2050[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2050.4.re <- resample(tasmax.2050.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2050 <- crop(tasmax.2050.4.re, extent(boundary_high))
r3.4.2050 <- mask(r2.4.2050, boundary_high)

## Check that it worked
plot(r3.4.2050)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2050, main="July 2050, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2050  ------

indices.5.2050  = which((tasmax.dates.5 <= as.Date(paste0("2050-07-31"))) & 
                          (tasmax.dates.5 >= as.Date(paste0("2050-07-01"))))

tasmax.2050.5 = tasmax.scenes.5[[indices.5.2050[1]]] 

tasmax.2050.5.re <- resample(tasmax.2050.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2050 <- crop(tasmax.2050.5.re, extent(boundary_high))
r3.5.2050 <- mask(r2.5.2050, boundary_high)

## Check that it worked
plot(r3.5.2050)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2050, main="July 2050, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 6, July 2050 ------

indices.6.2050 = which((tasmax.dates.6 <= as.Date(paste0("2050-07-31"))) & 
                         (tasmax.dates.6 >= as.Date(paste0("2050-07-01"))))

tasmax.2050.6 = tasmax.scenes.6[[indices.6.2050[1]]] 
tasmax.2050.6.re <- resample(tasmax.2050.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2050 <- crop(tasmax.2050.6.re, extent(boundary_high))
r3.6.2050 <- mask(r2.6.2050, boundary_high)

## Check that it worked
plot(r3.6.2050)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2050, main="July 2050, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2050.1.re, "outputs/CMPI6_rasters/tasmax.2050.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2050.2.re, "outputs/CMPI6_rasters/tasmax.2050.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2050.3.re, "outputs/CMPI6_rasters/tasmax.2050.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2050.4.re, "outputs/CMPI6_rasters/tasmax.2050.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2050.5.re, "outputs/CMPI6_rasters/tasmax.2050.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2050.6.re, "outputs/CMPI6_rasters/tasmax.2050.6.re.tif", overwrite = TRUE)

#.--------
# JULY 2060 -------

# MODEL 1, July 2060 -----
indices.60 = which((tasmax.dates.1 <= as.Date(paste0("2060-07-31"))) & 
                     (tasmax.dates.1>= as.Date(paste0("2060-07-01"))))

tasmax.2060.1 = tasmax.scenes.1[[indices.60[1]]] 
tasmax.2060.1.re <- resample(tasmax.2060.1, x) # hdd cdd is the climate raster

## crop and mask
r2.1.2060 <- crop(tasmax.2060.1.re, extent(boundary_high))
r3.1.2060 <- mask(r2.1.2060, boundary_high)

## Check that it worked
plot(r3.1.2060)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.2060, main="July 2040, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 2, July 2060 -----
indices.2.2060 = which((tasmax.dates.2 <= as.Date(paste0("2060-07-31"))) & 
                         (tasmax.dates.2 >= as.Date(paste0("2060-07-01"))))

tasmax.2060.2 = tasmax.scenes.2[[indices.2.2060[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2060.2.re <- resample(tasmax.2060.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2060 <- crop(tasmax.2060.2.re, extent(boundary_high))
r3.2060 <- mask(r2.2060, boundary_high)

## Check that it worked
plot(r3.2060)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2060, main="July 2050, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 3, July 2060 ------
indices.3.2060  = which((tasmax.dates.3 <= as.Date(paste0("2060-07-31"))) & 
                          (tasmax.dates.3 >= as.Date(paste0("2060-07-01"))))

tasmax.2060.3 = tasmax.scenes.3[[indices.3.2060[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2060.3.re <- resample(tasmax.2060.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2060 <- crop(tasmax.2060.3.re, extent(boundary_high))
r3.3.2060 <- mask(r3.2060, boundary_high)

## Check that it worked
plot(r3.3.2060)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2060, main="July 2050, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2060 ------

indices.4.2060  = which((tasmax.dates.4 <= as.Date(paste0("2060-07-31"))) & 
                          (tasmax.dates.4 >= as.Date(paste0("2060-07-01"))))

tasmax.2060.4 = tasmax.scenes.4[[indices.4.2060[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2060.4.re <- resample(tasmax.2060.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2060 <- crop(tasmax.2060.4.re, extent(boundary_high))
r3.4.2060 <- mask(r2.4.2060, boundary_high)

## Check that it worked
plot(r3.4.2060)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2060, main="July 2050, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2060  ------

indices.5.2060  = which((tasmax.dates.5 <= as.Date(paste0("2060-07-31"))) & 
                          (tasmax.dates.5 >= as.Date(paste0("2060-07-01"))))

tasmax.2060.5 = tasmax.scenes.5[[indices.5.2060[1]]] 

tasmax.2060.5.re <- resample(tasmax.2060.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2060 <- crop(tasmax.2060.5.re, extent(boundary_high))
r3.5.2060 <- mask(r2.6.2050, boundary_high)

## Check that it worked
plot(r3.6.2050)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2060, main="July 2050, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 6, July 2060 ------

indices.6.2060 = which((tasmax.dates.6 <= as.Date(paste0("2060-07-31"))) & 
                         (tasmax.dates.6 >= as.Date(paste0("2060-07-01"))))

tasmax.2060.6 = tasmax.scenes.6[[indices.6.2060[1]]] 
tasmax.2060.6.re <- resample(tasmax.2060.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2060 <- crop(tasmax.2060.6.re, extent(boundary_high))
r3.6.2060 <- mask(r2.6.2060, boundary_high)

## Check that it worked
plot(r3.6.2060)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2060, main="July 2050, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2060.1.re, "outputs/CMPI6_rasters/tasmax.2060.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2060.2.re, "outputs/CMPI6_rasters/tasmax.2060.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2060.3.re, "outputs/CMPI6_rasters/tasmax.2060.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2060.4.re, "outputs/CMPI6_rasters/tasmax.2060.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2060.5.re, "outputs/CMPI6_rasters/tasmax.2060.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2060.6.re, "outputs/CMPI6_rasters/tasmax.2060.6.re.tif", overwrite = TRUE)

# . ------
# JULY 2070 -------
# MODEL 1, July 2070 -----
indices.70 = which((tasmax.dates.1 <= as.Date(paste0("2070-07-31"))) & 
                     (tasmax.dates.1>= as.Date(paste0("2070-07-01"))))

tasmax.2070.1 = tasmax.scenes.1[[indices.70[1]]] 
tasmax.2070.1.re <- resample(tasmax.2070.1, x) # hdd cdd is the climate raster

## crop and mask
r2.1.2070 <- crop(tasmax.2070.1.re, extent(boundary_high))
r3.1.2070 <- mask(r2.1.2070, boundary_high)

## Check that it worked
plot(r3.1.2070)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.2070, main="July 2070, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 2, July 2070 -----
indices.2.2070 = which((tasmax.dates.2 <= as.Date(paste0("2070-07-31"))) & 
                         (tasmax.dates.2 >= as.Date(paste0("2070-07-01"))))

tasmax.2070.2 = tasmax.scenes.2[[indices.2.2070[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2070.2.re <- resample(tasmax.2070.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2070 <- crop(tasmax.2070.2.re, extent(boundary_high))
r3.2070 <- mask(r2.2070, boundary_high)

## Check that it worked
plot(r3.2070)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2070, main="July 2070, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 3, July 2070 ------
indices.3.2070  = which((tasmax.dates.3 <= as.Date(paste0("2070-07-31"))) & 
                          (tasmax.dates.3 >= as.Date(paste0("2070-07-01"))))

tasmax.2070.3 = tasmax.scenes.3[[indices.3.2070[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2070.3.re <- resample(tasmax.2070.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2070 <- crop(tasmax.2070.3.re, extent(boundary_high))
r3.3.2070 <- mask(r3.2070, boundary_high)

## Check that it worked
plot(r3.3.2070)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2070, main="July 2070, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2070 ------

indices.4.2070  = which((tasmax.dates.4 <= as.Date(paste0("2070-07-31"))) & 
                          (tasmax.dates.4 >= as.Date(paste0("2070-07-01"))))

tasmax.2070.4 = tasmax.scenes.4[[indices.4.2070[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2070.4.re <- resample(tasmax.2070.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2070 <- crop(tasmax.2070.4.re, extent(boundary_high))
r3.4.2070 <- mask(r2.4.2070, boundary_high)

## Check that it worked
plot(r3.4.2070)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2070, main="July 2070, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2070  ------

indices.5.2070  = which((tasmax.dates.5 <= as.Date(paste0("2070-07-31"))) & 
                          (tasmax.dates.5 >= as.Date(paste0("2070-07-01"))))

tasmax.2070.5 = tasmax.scenes.5[[indices.5.2070[1]]] 

tasmax.2070.5.re <- resample(tasmax.2070.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2070 <- crop(tasmax.2070.5.re, extent(boundary_high))
r3.5.2070 <- mask(r2.5.2070, boundary_high)

## Check that it worked
plot(r3.5.2070)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2070, main="July 2070, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 6, July 2070 ------

indices.6.2070 = which((tasmax.dates.6 <= as.Date(paste0("2070-07-31"))) & 
                         (tasmax.dates.6 >= as.Date(paste0("2070-07-01"))))

tasmax.2070.6 = tasmax.scenes.6[[indices.6.2070[1]]] 
tasmax.2070.6.re <- resample(tasmax.2070.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2070 <- crop(tasmax.2070.6.re, extent(boundary_high))
r3.6.2070 <- mask(r2.6.2070, boundary_high)

## Check that it worked
plot(r3.6.2070)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2070, main="July 2070, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2070.1.re, "outputs/CMPI6_rasters/tasmax.2070.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2070.2.re, "outputs/CMPI6_rasters/tasmax.2070.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2070.3.re, "outputs/CMPI6_rasters/tasmax.2070.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2070.4.re, "outputs/CMPI6_rasters/tasmax.2070.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2070.5.re, "outputs/CMPI6_rasters/tasmax.2070.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2070.6.re, "outputs/CMPI6_rasters/tasmax.2070.6.re.tif", overwrite = TRUE)

#.--------

# JULY 2080 -------
# MODEL 1, July 2080 -----
indices.80 = which((tasmax.dates.1 <= as.Date(paste0("2080-07-31"))) & 
                     (tasmax.dates.1>= as.Date(paste0("2080-07-01"))))

tasmax.2080.1 = tasmax.scenes.1[[indices.80[1]]] 
tasmax.2080.1.re <- resample(tasmax.2080.1, x) # hdd cdd is the climate raster

## crop and mask
r2.1.2080 <- crop(tasmax.2080.1.re, extent(boundary_high))
r3.1.2080 <- mask(r2.1.2080, boundary_high)

## Check that it worked
plot(r3.1.2080)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.2080, main="July 2080, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 2, July 2080 -----
indices.2.2080 = which((tasmax.dates.2 <= as.Date(paste0("2080-07-31"))) & 
                         (tasmax.dates.2 >= as.Date(paste0("2080-07-01"))))

tasmax.2080.2 = tasmax.scenes.2[[indices.2.2080[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2080.2.re <- resample(tasmax.2080.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2080 <- crop(tasmax.2080.2.re, extent(boundary_high))
r3.2080 <- mask(r2.2080, boundary_high)

## Check that it worked
plot(r3.2080)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2080, main="July 2080, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 3, July 2080 ------
indices.3.2080  = which((tasmax.dates.3 <= as.Date(paste0("2080-07-31"))) & 
                          (tasmax.dates.3 >= as.Date(paste0("2080-07-01"))))

tasmax.2080.3 = tasmax.scenes.3[[indices.3.2080[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2080.3.re <- resample(tasmax.2080.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2080 <- crop(tasmax.2080.3.re, extent(boundary_high))
r3.3.2080 <- mask(r3.2080, boundary_high)

## Check that it worked
plot(r3.3.2080)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2080, main="July 2080, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2080 ------

indices.4.2080  = which((tasmax.dates.4 <= as.Date(paste0("2080-07-31"))) & 
                          (tasmax.dates.4 >= as.Date(paste0("2080-07-01"))))

tasmax.2080.4 = tasmax.scenes.4[[indices.4.2080[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2080.4.re <- resample(tasmax.2080.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2080 <- crop(tasmax.2080.4.re, extent(boundary_high))
r3.4.2080 <- mask(r2.4.2080, boundary_high)

## Check that it worked
plot(r3.4.2080)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2080, main="July 2080, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2080  ------

indices.5.2080  = which((tasmax.dates.5 <= as.Date(paste0("2080-07-31"))) & 
                          (tasmax.dates.5 >= as.Date(paste0("2080-07-01"))))

tasmax.2080.5 = tasmax.scenes.5[[indices.5.2080[1]]] 

tasmax.2080.5.re <- resample(tasmax.2080.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2080 <- crop(tasmax.2080.5.re, extent(boundary_high))
r3.5.2080 <- mask(r2.5.2080, boundary_high)

## Check that it worked
plot(r3.5.2080)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2080, main="July 2080, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 6, July 2080 ------

indices.6.2080 = which((tasmax.dates.6 <= as.Date(paste0("2080-07-31"))) & 
                         (tasmax.dates.6 >= as.Date(paste0("2080-07-01"))))

tasmax.2080.6 = tasmax.scenes.6[[indices.6.2080[1]]] 
tasmax.2080.6.re <- resample(tasmax.2080.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2080 <- crop(tasmax.2080.6.re, extent(boundary_high))
r3.6.2080 <- mask(r2.6.2080, boundary_high)

## Check that it worked
plot(r3.6.2080)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2080, main="July 2080, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2080.1.re, "outputs/CMPI6_rasters/tasmax.2080.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2080.2.re, "outputs/CMPI6_rasters/tasmax.2080.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2080.3.re, "outputs/CMPI6_rasters/tasmax.2080.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2080.4.re, "outputs/CMPI6_rasters/tasmax.2080.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2080.5.re, "outputs/CMPI6_rasters/tasmax.2080.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2080.6.re, "outputs/CMPI6_rasters/tasmax.2080.6.re.tif", overwrite = TRUE)

# .--------
# JULY 2090 -------
# MODEL 1, July 2090 -----
indices.90 = which((tasmax.dates.1 <= as.Date(paste0("2090-07-31"))) & 
                     (tasmax.dates.1>= as.Date(paste0("2090-07-01"))))

tasmax.2090.1 = tasmax.scenes.1[[indices.90[1]]] 
tasmax.2090.1.re <- resample(tasmax.2090.1, x) # hdd cdd is the climate raster

## crop and mask
r2.1.2090 <- crop(tasmax.2090.1.re, extent(boundary_high))
r3.1.2090 <- mask(r2.1.2090, boundary_high)

## Check that it worked
plot(r3.1.2090)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.2090, main="July 2090, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 2, July 2090 -----
indices.2.2090 = which((tasmax.dates.2 <= as.Date(paste0("2090-07-31"))) & 
                         (tasmax.dates.2 >= as.Date(paste0("2090-07-01"))))

tasmax.2090.2 = tasmax.scenes.2[[indices.2.2090[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2090.2.re <- resample(tasmax.2090.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2090 <- crop(tasmax.2090.2.re, extent(boundary_high))
r3.2090 <- mask(r2.2090, boundary_high)

## Check that it worked
plot(r3.2090)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2090, main="July 2090, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 3, July 2090 ------
indices.3.2090  = which((tasmax.dates.3 <= as.Date(paste0("2090-07-31"))) & 
                          (tasmax.dates.3 >= as.Date(paste0("2090-07-01"))))

tasmax.2090.3 = tasmax.scenes.3[[indices.3.2090[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2090.3.re <- resample(tasmax.2090.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2090 <- crop(tasmax.2090.3.re, extent(boundary_high))
r3.3.2090 <- mask(r3.2090, boundary_high)

## Check that it worked
plot(r3.3.2090)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2090, main="July 2090, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2090 ------

indices.4.2090  = which((tasmax.dates.4 <= as.Date(paste0("2090-07-31"))) & 
                          (tasmax.dates.4 >= as.Date(paste0("2090-07-01"))))

tasmax.2090.4 = tasmax.scenes.4[[indices.4.2090[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2090.4.re <- resample(tasmax.2090.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2090 <- crop(tasmax.2090.4.re, extent(boundary_high))
r3.4.2090 <- mask(r2.4.2090, boundary_high)

## Check that it worked
plot(r3.4.2090)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2090, main="July 2090, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2090  ------

indices.5.2090  = which((tasmax.dates.5 <= as.Date(paste0("2090-07-31"))) & 
                          (tasmax.dates.5 >= as.Date(paste0("2090-07-01"))))

tasmax.2090.5 = tasmax.scenes.5[[indices.5.2090[1]]] 

tasmax.2090.5.re <- resample(tasmax.2090.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2090 <- crop(tasmax.2090.5.re, extent(boundary_high))
r3.5.2090 <- mask(r2.5.2090, boundary_high)

## Check that it worked
plot(r3.5.2090)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2090, main="July 2090, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 6, July 2090 ------

indices.6.2090 = which((tasmax.dates.6 <= as.Date(paste0("2090-07-31"))) & 
                         (tasmax.dates.6 >= as.Date(paste0("2090-07-01"))))

tasmax.2090.6 = tasmax.scenes.6[[indices.6.2090[1]]] 
tasmax.2090.6.re <- resample(tasmax.2090.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2090 <- crop(tasmax.2090.6.re, extent(boundary_high))
r3.6.2090 <- mask(r2.6.2090, boundary_high)

## Check that it worked
plot(r3.6.2090)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2090, main="July 2090, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2090.1.re, "outputs/CMPI6_rasters/tasmax.2090.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2090.2.re, "outputs/CMPI6_rasters/tasmax.2090.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2090.3.re, "outputs/CMPI6_rasters/tasmax.2090.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2090.4.re, "outputs/CMPI6_rasters/tasmax.2090.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2090.5.re, "outputs/CMPI6_rasters/tasmax.2090.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2090.6.re, "outputs/CMPI6_rasters/tasmax.2090.6.re.tif", overwrite = TRUE)


# .-------
# JULY 2100 ------

# MODEL 1, July 2100 -----
indices.10 = which((tasmax.dates.1 <= as.Date(paste0("2100-07-31"))) & 
                     (tasmax.dates.1>= as.Date(paste0("2100-07-01"))))

tasmax.2100.1 = tasmax.scenes.1[[indices.10[1]]] 
tasmax.2100.1.re <- resample(tasmax.2100.1, x) # hdd cdd is the climate raster

## crop and mask
r2.1.2100 <- crop(tasmax.2100.1.re, extent(boundary_high))
r3.1.2100 <- mask(r2.1.2100, boundary_high)

## Check that it worked
plot(r3.1.2100)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.2100, main="July 2100, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 2, July 2100 -----
indices.2.2100 = which((tasmax.dates.2 <= as.Date(paste0("2100-07-31"))) & 
                         (tasmax.dates.2 >= as.Date(paste0("2100-07-01"))))

tasmax.2100.2 = tasmax.scenes.2[[indices.2.2100[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2100.2.re <- resample(tasmax.2100.2, x) # hdd cdd is the climate raster

## crop and mask
r2.2100 <- crop(tasmax.2100.2.re, extent(boundary_high))
r3.2100 <- mask(r2.2100, boundary_high)

## Check that it worked
plot(r3.2100)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2100, main="July 2100, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 3, July 2100 ------
indices.3.2100 = which((tasmax.dates.3 <= as.Date(paste0("2100-07-31"))) & 
                          (tasmax.dates.3 >= as.Date(paste0("2100-07-01"))))

tasmax.2100.3 = tasmax.scenes.3[[indices.3.2100[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2100.3.re <- resample(tasmax.2100.3, x) # hdd cdd is the climate raster

## crop and mask
r3.2100 <- crop(tasmax.2100.3.re, extent(boundary_high))
r3.3.2100 <- mask(r3.2100, boundary_high)

## Check that it worked
plot(r3.3.2100)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2100, main="July 2100, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2100 ------

indices.4.2100  = which((tasmax.dates.4 <= as.Date(paste0("2100-07-31"))) & 
                          (tasmax.dates.4 >= as.Date(paste0("2100-07-01"))))

tasmax.2100.4 = tasmax.scenes.4[[indices.4.2100[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2100.4.re <- resample(tasmax.2100.4, x) # hdd cdd is the climate raster

## crop and mask
r2.4.2100 <- crop(tasmax.2100.4.re, extent(boundary_high))
r3.4.2100 <- mask(r2.4.2100, boundary_high)

## Check that it worked
plot(r3.4.2100)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2100, main="July 2100, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2100  ------

indices.5.2100  = which((tasmax.dates.5 <= as.Date(paste0("2100-07-31"))) & 
                          (tasmax.dates.5 >= as.Date(paste0("2100-07-01"))))

tasmax.2100.5 = tasmax.scenes.5[[indices.5.2100[1]]] 

tasmax.2100.5.re <- resample(tasmax.2100.5, x) # hdd cdd is the climate raster

## crop and mask
r2.5.2100 <- crop(tasmax.2100.5.re, extent(boundary_high))
r3.5.2100 <- mask(r2.5.2100, boundary_high)

## Check that it worked
plot(r3.5.2100)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2100, main="July 2100, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 6, July 2100 ------

indices.6.2100 = which((tasmax.dates.6 <= as.Date(paste0("2100-07-31"))) & 
                         (tasmax.dates.6 >= as.Date(paste0("2100-07-01"))))

tasmax.2100.6 = tasmax.scenes.6[[indices.6.2100[1]]] 
tasmax.2100.6.re <- resample(tasmax.2100.6, x) # hdd cdd is the climate raster

## crop and mask
r2.6.2100 <- crop(tasmax.2100.6.re, extent(boundary_high))
r3.6.2100 <- mask(r2.6.2100, boundary_high)

## Check that it worked
plot(r3.6.2100)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2100, main="July 2100, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# saving all rasters
writeRaster(tasmax.2100.1.re, "outputs/CMPI6_rasters/tasmax.2100.1.re.tif", overwrite = TRUE)
writeRaster(tasmax.2100.2.re, "outputs/CMPI6_rasters/tasmax.2100.2.re.tif", overwrite = TRUE)
writeRaster(tasmax.2100.3.re, "outputs/CMPI6_rasters/tasmax.2100.3.re.tif", overwrite = TRUE)
writeRaster(tasmax.2100.4.re, "outputs/CMPI6_rasters/tasmax.2100.4.re.tif", overwrite = TRUE)
writeRaster(tasmax.2100.5.re, "outputs/CMPI6_rasters/tasmax.2100.5.re.tif", overwrite = TRUE)
writeRaster(tasmax.2100.6.re, "outputs/CMPI6_rasters/tasmax.2100.6.re.tif", overwrite = TRUE)


