# CMPI5 explore script
# Created on 06/03/2023 by Erica
# https://michaelminn.net/tutorials/r-climate/index.html

# libraries -----
library(ncdf4)
library(raster)
library(tidyr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(RNetCDF)

# setting working directory to my hard drive
setwd("/Volumes/Kluane_22/CHELSA_CMIP5")

# download data (from hard drive)
nc = open.nc("tasmax_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012.nc")
nc_85 = open.nc("tasmax_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012.nc")


tasmax.dates = as.Date(var.get.nc(nc, "time"), origin="1850-01-01 00:00:00")
tasmax.dates_2= as.Date(var.get.nc(nc_85, "time"), origin="1850-01-01 00:00:00")

tasmax.scenes = sapply(1:length(tasmax.dates_2), function(z) {
  grid = var.get.nc(nc_85, "tasmax", start=c(NA, NA, z), count=c(NA, NA, 1))
  x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
             crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  x = rotate(flip(t(x), 2))
  x = (x - 273.15)
  return(x) })

close.nc(nc_85)

plot(tasmax.scenes[[1000]], main=tasmax.dates_2[[1000]], 
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(32))

tasmax_df <- as.data.frame(tasmax_df) # doesnt work

# Extract 
library(sf)
library(tsbox)

weather_station = st_sfc(st_point(c(-88.27778, 40.03972)), crs=4326)

y = sapply(tasmax.scenes, function(scene)  extract(scene, as_Spatial(weather_station)))

x = 1970 + (as.numeric(tasmax.dates) / 365.25)

tasmax.series = ts(y, start=floor(min(x)), end=floor(max(x)), deltat=1/12)

plot(tasmax.series, col="darkred", ylab="Monthly Mean High Temperatures (F)",
     type="l", lwd=3, bty="n", las=1, fg=NA)

grid(nx=NA, ny=NULL, lty=1)

decomposition = stl(tasmax.series, s.window=240, t.window=120)

plot(decomposition)

