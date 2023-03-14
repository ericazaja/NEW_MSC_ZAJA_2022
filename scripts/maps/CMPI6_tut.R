# CMPI6 dowload and average script 
# Created on 06/03/2023 by Erica
# following tutrial:
# https://michaelminn.net/tutorials/r-climate/index.html

# Libraries -----
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

plot(tasmax.scenes.1[[1000]], main="NOAA, RCP 8.5", sub =tasmax.dates.1[[1000]], 
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

plot(tasmax.scenes.2[[1000]], main="CMCC, RCP 8.5",  sub=tasmax.dates.2[[1000]], 
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

plot(tasmax.scenes.3[[1000]], main = "MRI, RCP 8.5", sub=tasmax.dates.3[[1000]], 
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

plot(tasmax.scenes.4[[1000]],main = "MIROC, RCP 8.5", sub=tasmax.dates.4[[1000]], 
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

plot(tasmax.scenes.5[[1000]], main = "CNRM, RCP 8.5",sub=tasmax.dates.5[[1000]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# Model 6: ACCESS ----
tasmax.dates.6 = as.Date(var.get.nc(nc_ACCESS_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes.6 = sapply(1:length(tasmax.dates.6), function(z) {
  grid = var.get.nc(nc_BCC_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15) # converting to celcius
  return(x) })

close.nc(nc_ACCESS_85)

plot(tasmax.scenes.6[[1000]], main = "ACCESS, RCP 8.5",sub=tasmax.dates.6[[1030]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# EXTRACTING rasters by year ------

# JULY 2020 ------

# MODEL 1 July 2020 ----

indices.1 = which((tasmax.dates.1 <= as.Date(paste0("2020-07-31"))) & 
                  (tasmax.dates.1 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.1 = tasmax.scenes.1[[indices.1[1]]] 
res(tasmax.2020.1)# 1.25 1.00 degrees
projection(tasmax.2020.1) #"+proj=longlat +datum=WGS84 +no_defs"

# crop and mask
r2.1 <- crop(tasmax.2020.1, extent(boundary))
r3.1 <- mask(r2.1, boundary)

## Check that it worked
plot(r3.1)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1, main="July 2020, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

#  MODEL 2, July 2020 ------

indices.2 = which((tasmax.dates.2 <= as.Date(paste0("2020-07-31"))) & 
                  (tasmax.dates.2 >= as.Date(paste0("2020-07-01"))))

tasmax.2020.2 = tasmax.scenes.2[[indices.2[1]]] 
res(tasmax.2020.2)# 1.2500 0.9375 degrees
projection(tasmax.2020.2) #"+proj=longlat +datum=WGS84 +no_defs"
# make size same as other climate raster (NOAA)
tasmax.2020.2.re <- resample(tasmax.2020.2, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.2 <- crop(tasmax.2020.2.re, extent(boundary))
r3.2 <- mask(r2.2, boundary)

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
tasmax.2020.3.re <- resample(tasmax.2020.3, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.3 <- crop(tasmax.2020.3.re, extent(boundary))
r3.3 <- mask(r2.3, boundary)

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
tasmax.2020.4.re <- resample(tasmax.2020.4, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.4 <- crop(tasmax.2020.4.re, extent(boundary))
r3.4 <- mask(r2.4, boundary)

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
tasmax.2020.5.re <- resample(tasmax.2020.5, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.5 <- crop(tasmax.2020.5.re, extent(boundary))
r3.5 <- mask(r2.5, boundary)

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
tasmax.2020.6.re <- resample(tasmax.2020.6, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.6 <- crop(tasmax.2020.6.re, extent(boundary))
r3.6 <- mask(r2.6, boundary)

## Check that it worked
plot(r3.6)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6, main="July 2023, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))



# JULY 2030 -------
# MODEL 1, July 2030 ----
indices.1.1 = which((tasmax.dates.1 <= as.Date(paste0("2030-07-31"))) & 
                    (tasmax.dates.1 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.1 = tasmax.scenes.1[[indices.1.1[1]]] 

# crop and mask
r2.1.1 <- crop(tasmax.2030.1, extent(boundary))
r3.1.1 <- mask(r2.1.1, boundary)

## Check that it worked
plot(r3.1.1)
plot(boundary, add=TRUE, lwd=2)

plot(r3.1.1, main="July 2030, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

#  MODEL 2, July 2030 ------

indices.2.2 = which((tasmax.dates.2 <= as.Date(paste0("2030-07-31"))) & 
                    (tasmax.dates.2 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.2 = tasmax.scenes.2[[indices.2.2[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2030.2.re <- resample(tasmax.2030.2, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.2030 <- crop(tasmax.2030.2.re, extent(boundary))
r3.2030 <- mask(r2.2030, boundary)

## Check that it worked
plot(r3.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.2030, main="July 2030, model 2", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


#  MODEL 3, July 2030 ------

indices.3.3  = which((tasmax.dates.3 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.3 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.3 = tasmax.scenes.3[[indices.3.3[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2030.3.re <- resample(tasmax.2030.3, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r3.2030 <- crop(tasmax.2030.3.re, extent(boundary))
r3.3.2030 <- mask(r3.2030, boundary)

## Check that it worked
plot(r3.3.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.3.2030, main="July 2030, model 3", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 4, July 2030 ------

indices.4.4  = which((tasmax.dates.4 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.4 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.4 = tasmax.scenes.4[[indices.4.4[1]]] 
# make size same as other climate raster (NOAA)
tasmax.2030.4.re <- resample(tasmax.2030.4, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.4.2030 <- crop(tasmax.2030.4.re, extent(boundary))
r3.4.2030 <- mask(r2.4.2030, boundary)

## Check that it worked
plot(r3.4.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.4.2030, main="July 2030, model 4", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# MODEL 5, July 2030  ------

indices.5.5  = which((tasmax.dates.5 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.5 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.5 = tasmax.scenes.5[[indices.5.5[1]]] 

tasmax.2030.5.re <- resample(tasmax.2030.5, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.5.2030 <- crop(tasmax.2030.5.re, extent(boundary))
r3.5.2030 <- mask(r2.5.2030, boundary)

## Check that it worked
plot(r3.5.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.5.2030, main="July 2030, model 5", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))



# MODEL 6, July 2030 ------

indices.6.6  = which((tasmax.dates.6 <= as.Date(paste0("2030-07-31"))) & 
                     (tasmax.dates.6 >= as.Date(paste0("2030-07-01"))))

tasmax.2030.6 = tasmax.scenes.6[[indices.6.6[1]]] 
tasmax.2030.6.re <- resample(tasmax.2030.6, tasmax.2020.1) # hdd cdd is the climate raster

## crop and mask
r2.6.2030 <- crop(tasmax.2030.6.re, extent(boundary))
r3.6.2030 <- mask(r2.6.2030, boundary)

## Check that it worked
plot(r3.6.2030)
plot(boundary, add=TRUE, lwd=2)

plot(r3.6.2030, main="July 2030, model 6", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))



# JULY 2040 -------
# July 2040, MODEL 1-----
indices_2040 = which((tasmax.dates <= as.Date(paste0("2040-07-31"))) & 
                  (tasmax.dates>= as.Date(paste0("2040-07-01"))))

tasmax.2040 = tasmax.scenes.2[[indices[1]]] 
res(tasmax.2040)# 1.25 1.00 degrees
projection(tasmax.2040) #"+proj=longlat +datum=WGS84 +no_defs"
# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range

## crop and mask
r2 <- crop(tasmax.2040, extent(boundary))
r3 <- mask(r2, boundary)

## Check that it worked
plot(r3)
plot(boundary, add=TRUE, lwd=2)

plot(r3, main="July 2040, model 1", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))
# df_2023_july_85 <- as.data.frame(r3, xy=TRUE)

# 2050 -------
# July 2050
indices = which((tasmax.dates_2 <= as.Date(paste0("2050-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2050-07-01"))))

tasmax.2050= tasmax.scenes[[indices[1]]] 
# hdd.cdd.2050 = crop(tasmax.2050, p50_2020) # crop to the extent of the PCH range
# df_2050_july_85 <- as.data.frame(hdd.cdd.2050, xy=TRUE)
class(hdd.cdd.2050)

## crop and mask
r2 <- crop(tasmax.2050, extent(boundary))
r3 <- mask(r2, boundary)

## Check that it worked
plot(r3)
plot(boundary, add=TRUE, lwd=2)
plot(r3, main="July 2050", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# 2060 -------

# 2070 -------

# 2080 -------
# July 2080
indices = which((tasmax.dates_2 <= as.Date(paste0("2080-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2080-07-01"))))

tasmax.2080= tasmax.scenes[[indices[1]]] 
#hdd.cdd.2080 = crop(tasmax.2080, p50_2020) # crop to the extent of the PCH range
#df_2080_july_85 <- as.data.frame(hdd.cdd.2080, xy=TRUE)

## crop and mask
r2 <- crop(tasmax.2080, extent(boundary))
r3 <- mask(r2, boundary)

## Check that it worked
plot(r3)
plot(boundary, add=TRUE, lwd=2)
plot(r3, main="July 2080", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# 2090 -------

# 2100 ------

# July 2100
indices = which((tasmax.dates_2 <= as.Date(paste0("2100-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2100-07-01"))))

tasmax.2100= tasmax.scenes[[indices[1]]] 
#hdd.cdd.2100 = crop(tasmax.2100, p50_2020) # crop to the extent of the PCH range
# df_2100_july_85 <- as.data.frame(hdd.cdd.2100, xy=TRUE)

## crop and mask
r2 <- crop(tasmax.2100, extent(boundary))
r3 <- mask(r2, boundary)

## Check that it worked
plot(r3)
plot(boundary, add=TRUE, lwd=2)
plot(r3, main="July 2100", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))
# df_2080_july_85 <- as.data.frame(hdd.cdd.2080, xy=TRUE)

library(gridExtra)
grid.arrange(plot_2023, plot_2050, plot_2080, plot_2100, nrow=2)

# STOP----
# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range
# df_2023_july_85 <- as.data.frame(r3, xy=TRUE)

# Aggregated Temperature

indices = which((tasmax.dates_2 >= as.Date(paste0("2060-01-01"))) &
(tasmax.dates_2 <= as.Date(paste0("2060-12-31"))))

tasmax.2060 = tasmax.scenes[[indices[1]]]

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2060) = pmax(values(tasmax.2060), values(scene)) }

plot(tasmax.2060, main="2060", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# Extract 

weather_station = st_sfc(st_point(c(-88.27778, 40.03972)), crs=4326)

y = sapply(tasmax.scenes, function(scene)  extract(scene, as_Spatial(poly)))

x = 1970 + (as.numeric(tasmax.dates) / 365.25)

tasmax.series = ts(y, start=floor(min(x)), end=floor(max(x)), deltat=1/12)

plot(tasmax.series, col="darkred", ylab="Monthly Mean High Temperatures (F)",
     type="l", lwd=3, bty="n", las=1, fg=NA)

grid(nx=NA, ny=NULL, lty=1)

decomposition = stl(tasmax.series, s.window=240, t.window=120)

plot(decomposition)

# Aggregated Baseline Rasters (2020)

indices = which((tasmax.dates <= as.Date(paste0("2020-12-31"))) & 
                  (tasmax.dates >= as.Date(paste0("2020-01-01"))))

tasmax.2020 = tasmax.scenes[[indices[1]]]


plot(hdd.cdd.2060, main="2020", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2020) = pmax(values(hdd.cdd.2060), values(scene)) }


# STOP ----

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2023) = pmax(values(hdd.cdd.2023), values(scene)) }

indices = which((pr.dates <= as.Date(paste0("2020-12-31"))) & 
                  (pr.dates >= as.Date(paste0("2020-01-01"))))

pr.2020 = pr.scenes[[indices[1]]]

for (scene in pr.scenes[indices[2:length(indices)]]) {
  pr.2020 = pr.2020 + scene }


# extras ------
# nc_26 = open.nc("data/CHELSA_CMPI5/tasmax_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012.nc")

# # polygon of max and min lat long of caribou area
#lon <- c(-135.29,-149.9)
#lat <- c(70.1, 64.8)
#Poly_Coord_df = data.frame(lon, lat)
#poly <- Poly_Coord_df %>% 
# sf::st_as_sf(coords = c("lon", "lat"), 
#crs = 4326) %>% 
#sf::st_bbox() %>% 
#st_as_sfc()
#plot(poly, x = lon, y = lat)
#class(poly) #sfc_POLYGON
#nc_sp <- sf:::as_Spatial(poly) # This works
