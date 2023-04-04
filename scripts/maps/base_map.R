# NORT AMERICA MAP -----
# tutorial:

# Libraries -----

library(raster)
library(rworldmap)
library(rgeos)
library(rworldxtra)
library(sf)
library(rasterVis)
library(tidyverse)

#  boundary of Katie's maps
boundary_highest <- st_read("data/shapefiles/katie_map_border_highest.shp")

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

# crop and mask N.America map to extent I set
#x <- crop(r.new, NA_full_2)
#y <- mask(x, NA_full_2)

# plot
#plot(y) # extent
#lines(NA_full_2, add=TRUE)
#plot(r3.1, add=TRUE)
# my raster
#lines(NA_full_2, add=TRUE)
#plot(NA_full_2, add=T, lwd = 2)

# this below works
pdf("outputs/plot_test.pdf",paper = "A4")
bb <- extent(-170 ,-110, 50 ,75)
plot(bb, col=NA, xlab = "Longitude", ylab = "Latitude")
plot(r3.1, add=T, legend=FALSE)
lines(NA_full_2, add=TRUE)
plot(r3.1, legend.only=TRUE, legend.width = 0.5,
     smallplot=c(0.8,0.90, 0.2,0.3)); par(mar = par("mar"))
dev.off()
# ggsave()



