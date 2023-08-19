# NORTH AMERICA MAP -----

# Libraries -----
library(raster)
library(rworldmap)
library(rgeos)
library(rworldxtra)
library(sf)
library(rasterVis)
library(tidyverse)

# Load boundary of Katie Orndhal maps ------
boundary_highest <- st_read("data/shapefiles/katie_map_border_highest.shp")
p50_2020_resample_highest <- raster("data/maps_data/p50_2020_resample_highest.tiff") 
plot(boundary_highest, fill=NA, color = "purple4")

# raster to practice on
tasmax.2020.1.re <- raster("outputs/CMPI6_rasters/tasmax.2020.1.re.tif")
r2.1 <- crop(tasmax.2020.1.re, extent(boundary_highest))
r3.1 <- mask(r2.1, boundary_highest)

# create extent that i want
r <- raster()
bb <- extent(-170 ,-120, 50 ,75)
extent(r) <- bb
r.new<- setExtent(r, bb, keepres=TRUE)

# load border of USA and CANADA
USA <- raster::getData("GADM", country = "USA", level = 1)
CAN <- raster::getData("GADM", country = "CAN", level = 1)
NA_full <- rbind(USA, CAN)
object.size(NA_full) / 1000000
NA_full_2 <- rgeos::gSimplify(NA_full, tol = 0.001)
NA_full_2 <- SpatialPolygonsDataFrame(NA_full_2, NA_full@data)
object.size(NA_full_2) / 1000000

# this below works
pdf("outputs/plot_test.pdf",paper = "A4")
bb <- extent(-170 ,-110, 50 ,75)
plot(bb, col=NA, xlab = "Longitude", ylab = "Latitude")
plot(p50_2020_resample_highest, add=T, legend=FALSE) # plot(raster, ...)
lines(NA_full_2, add=TRUE)
plot(p50_2020_resample_highest, legend.only=TRUE, legend.width = 0.5,
     smallplot=c(0.8,0.90, 0.2,0.4)); par(mar = par("mar"))
dev.off()
# ggsave()




