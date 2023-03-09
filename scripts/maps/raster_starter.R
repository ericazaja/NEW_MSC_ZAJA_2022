#### RASTER script
#### Script by Erica Zaja, created 03/11/22
### Last updated: 12/10/2022

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(raster)
library(rasterVis)
library(gridExtra)
library(terra)

# 2. LOADING DATA ----
# Shapefile of border of katie's map 
boundary <- st_read("data/katie_map_border.shp") 

# Rasters of shrub biomass (g/m2) in the PCH range in 2020 (relevant to me)
# Using the best-estimates: the 50th percentile of the 1,000 permutations
p50_2020 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2020_wgs84.tif") 

# all other rasters 1985-2020 (NB only 1985 and 2020 have right projection)
# p50_1985 <- raster("data/katie_maps/pft_agb_deciduousshrub_p025_1985_wgs84.tif") 
#p50_1990 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1990.tif") 
#p50_1995 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1995.tif") 
#p50_2000 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2000.tif") 
#p50_2005 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2005.tif") 
#p50_2010 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2010.tif") 
#p50_2015 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2015.tif") 
#p50_2020 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2020.tif") 

# Rasters tell us where there is shrub and where there isn't (cover)
# if there is shrub, what is their biomass (g/m2)

# 3. DATA EXPLORE  -----
plot(p50_2020) 
class(p50_2020) # raster 

# exploring resolution 
res(p50_2020) # resolution 0.000595209 0.000595209 degrees

# exploring projection
projection(p50_2020) # "+proj=longlat +datum=WGS84 +no_defs"
# Previous proj was aea: "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
crs(p50_2020)

# extent 
extent(p50_2020)
# class      : Extent 
#xmin       : -154.8826 
#xmax       : -127.0697 
#ymin       : 59.43991 
#ymax       : 71.49051 

# AGGREGATE PIXELS -----
# Aggregate pixels of shrub map p50_2020 to climate raster size (1.25 x 1 degree)
p50_2020_resample <- resample(p50_2020, hdd.cdd.2050) # hdd cdd is the climate raster
plot(p50_2020_resample)
res(p50_2020_resample) #1.25 1.00, same as climate!
# saving raster 
writeRaster(p50_2020_resample, "data/p50_2020_resample.tif")

# BOUNDARY SHP ------
# Extracting boundary of caribou map to make shapefile 
e <- extent(p50_2020_resample)
plot(e)
poly_shrub_2020 <- as(e , 'SpatialPolygons')  
plot(poly_shrub_2020)
plot(p50_2020_resample)
plot(poly_shrub_2020, lwd =3, border = "red", add = TRUE)
r <- p50_2020_resample > -Inf
pp <- rasterToPolygons(r,  dissolve = TRUE)
plot(p50_2020_resample)
plot(poly_shrub_2020, lwd =3, border = "red", add = TRUE)
plot(pp, lwd =2, border = "blue", add = TRUE)

# saving as shapefile
# shapefile(pp, file = "data/katie_map_border.shp")

# AREA -----
# Measuring area of raster
# get sizes of all cells in raster [km2]
cell_size <- raster::area(p50_2020_resample, na.rm = TRUE, weights = FALSE)
# delete NAs from vector of all raster cells
# NAs lie outside of the rastered region, can thus be omitted
cell_size <- cell_size[!is.na(cell_size)] # 0.2815663
#compute area [km2] of all cells in geo_raster
raster_area <-length(cell_size)*median(cell_size)
# print area of shrub map according to raster object
print(paste("Area of PCH Alaskan range (raster)", round(raster_area, digits = 1),"km2"))
# [1] "Area of PCH summer range (raster) is 793385.6 km2"
# NB. PIXELS = CELLS

# EXTRACTION of raster points ------
# Extract points from raster 
extract <- raster::extract(p50_2020_resample, boundary, df = TRUE, cellnumbers = TRUE)

# Order (for checking purposes)
extract <- extract[order(extract$cell),]

# Extract coordinates
xy <- xyFromCell(p50_2020_resample, cell = extract$cell, spatial = FALSE)

# Convert to df and add cellnumber
xy <- as.data.frame(xy)
xy$cell <- extract$cell

# Merge two data frames
extract_end <- merge(extract, xy)
extract_end <- extract_end[order(extract_end$cell),]
view(extract_end)
hist(extract_end$pft_agb_deciduousshrub_p50_2020_wgs84)

# saving data as csv
write.csv(extract_end, file = "data/extract_end.csv")

# DATA VIS ------
# setting a personalised theme 
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# Plotting shrub raster (entire) with ggplot
(gplot_p50_1985 <- gplot(p50_2020_resample) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<500, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 500)),1)}, na.value="white") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass (g/m2) in PCH range (2020)\n") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text


# Cropped map with personalised colour palette (low-mid-high) 
(gplot_p50_1985_test_my_palette <- gplot(p50_2020_resample) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_gradient2(low = "green4", mid = "green3", high = "brown", midpoint = 10,  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass cover (g/m2) of the PCH range (2020)\n") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text

# plotting raster with personalised colours from dataframe 
(raster_my_palette_new <- ggplot(extract_end) + 
    geom_tile(aes(x=x,y=y,fill=pft_agb_deciduousshrub_p50_2020_wgs84)) + 
    # scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient2(name = "Shrub biomass g/m2",high = "green4", mid = "green3", low = "brown", midpoint = 350,  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n"))
   # xlim(-150, -130)+
   # ylim(60,71)+ 
    #theme(plot.title = element_text(hjust = 0.5),      # centres plot title
     #     text = element_text(size=40),	
     #     axis.title.x =element_text(size=40),
      #    axis.title.y =element_text(size=40),
      #    axis.text.x = element_text(size=40, hjust = 1),
      #    axis.text.y = element_text(size=40, hjust = 45),
       #   legend.text = element_text(size=20),
       #   legend.title = element_text(size=40),
        #  legend.position ="right"))

# EXTRAS ----
#p50_2020_resample_df <- extract(p50_2020_resample, xy, cellnumbers = T)
#view(p50_2020_resample_df)

#p50_2020_random <- as.data.frame(sampleRandom(p50_2020_resample, 264, na.rm=TRUE, ext=NULL, 
#                                           cells=TRUE, rowcol=FALSE, xy = TRUE)) 
#view(p50_2020_random)
#hist(p50_2020_random$pft_agb_deciduousshrub_p50_2020_wgs84)

