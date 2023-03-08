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
library(sf)
library(tsbox)

# download data
nc_26 = open.nc("data/CHELSA_CMPI5/tasmax_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012.nc")
nc_85 = open.nc("data/CHELSA_CMPI5/tasmax_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012.nc")

# # polygon of max and min lat long of caribou area
lon <- c(-135.29,-149.9)
lat <- c(70.1, 64.8)
Poly_Coord_df = data.frame(lon, lat)
poly <- Poly_Coord_df %>% 
  sf::st_as_sf(coords = c("lon", "lat"), 
               crs = 4326) %>% 
  sf::st_bbox() %>% 
  st_as_sfc()
plot(poly, x = lon, y = lat)
class(poly) #sfc_POLYGON
nc_sp <- sf:::as_Spatial(poly) # This works


# Process the Data

tasmax.dates = as.Date(var.get.nc(nc_26, "time"), origin="1850-01-01 00:00:00")
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
     col=colorRampPalette(c('navy', 'lightgray', 'red'))(30))

# crop <- crop(tasmax.scenes, extent(nc_sp), xy = TRUE) # cant do this for list
# STOP----

# Aggregated Temperature

indices = which((tasmax.dates_2 >= as.Date(paste0("2060-01-01"))) &
(tasmax.dates_2 <= as.Date(paste0("2060-12-31"))))

tasmax.2060 = tasmax.scenes[[indices[1]]]

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2060) = pmax(values(tasmax.2060), values(scene)) }

plot(tasmax.2060, main="2060", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# b <- brick(tasmax.scenes)

# tasmax_df <- as.data.frame(tasmax.scenes) # doesnt work

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

# START again -----
# July 2023

indices = which((tasmax.dates_2 <= as.Date(paste0("2023-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2023-07-01"))))

tasmax.2023 = tasmax.scenes[[indices[1]]] 
res(tasmax.2023) # 1.25 1.00 degrees
projection(tasmax.2023) #"+proj=longlat +datum=WGS84 +no_defs"
hdd.cdd.2023 = crop(tasmax.2023, p50_2020) # crop to the extent of the PCH range
df_2023_july_85 <- as.data.frame(hdd.cdd.2023, xy=TRUE)

plot(hdd.cdd.2023, main="July 2023", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# july 2050
indices = which((tasmax.dates_2 <= as.Date(paste0("2050-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2050-07-01"))))

tasmax.2050= tasmax.scenes[[indices[1]]] 
hdd.cdd.2050 = crop(tasmax.2050, p50_2020) # crop to the extent of the PCH range
df_2050_july_85 <- as.data.frame(hdd.cdd.2050, xy=TRUE)

plot(hdd.cdd.2050, main="July 2050", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# july 2080
indices = which((tasmax.dates_2 <= as.Date(paste0("2080-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2080-07-01"))))

tasmax.2080= tasmax.scenes[[indices[1]]] 
hdd.cdd.2080 = crop(tasmax.2080, p50_2020) # crop to the extent of the PCH range
df_2080_july_85 <- as.data.frame(hdd.cdd.2080, xy=TRUE)

plot(hdd.cdd.2080, main="July 2080", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

# july 2100
indices = which((tasmax.dates_2 <= as.Date(paste0("2100-07-31"))) & 
                  (tasmax.dates_2 >= as.Date(paste0("2100-07-01"))))

tasmax.2100= tasmax.scenes[[indices[1]]] 
hdd.cdd.2100 = crop(tasmax.2100, p50_2020) # crop to the extent of the PCH range
df_2100_july_85 <- as.data.frame(hdd.cdd.2100, xy=TRUE)

plot(hdd.cdd.2100, main="July 2100", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))
df_2080_july_85 <- as.data.frame(hdd.cdd.2080, xy=TRUE)

library(gridExtra)
grid.arrange(plot_2023, plot_2050, plot_2080, plot_2100, nrow=2)

# STOP ----

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2023) = pmax(values(hdd.cdd.2023), values(scene)) }

indices = which((pr.dates <= as.Date(paste0("2020-12-31"))) & 
                  (pr.dates >= as.Date(paste0("2020-01-01"))))

pr.2020 = pr.scenes[[indices[1]]]

for (scene in pr.scenes[indices[2:length(indices)]]) {
  pr.2020 = pr.2020 + scene }