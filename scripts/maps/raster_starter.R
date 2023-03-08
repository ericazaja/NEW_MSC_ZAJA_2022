#### RASTER STARTER SCRIPT
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
boundary <- st_read("data/katie_map_border.shp") # loading data
plot(boundary)

# Loading rasters of shrub biomass (g/m2) in the PCH range from 1985-2020
# Using the best-estimates: the 50th percentile of the 1,000 permutations

p50_1985 <- raster("data/katie_maps/pft_agb_deciduousshrub_p025_1985_wgs84.tif") 
p50_2020 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2020_wgs84.tif") 

#p50_1990 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1990.tif") 
#p50_1995 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1995.tif") 
#p50_2000 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2000.tif") 
#p50_2005 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2005.tif") 
#p50_2010 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2010.tif") 
#p50_2015 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2015.tif") 
#p50_2020 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2020.tif") 

# Rasters tell us where there is shrub and where there isn't (cover)
# if there is shrub, what is their biomass

# 3. DATA EXPLORE  -----
plot(p50_1985)
plot(p50_2020)

class(p50_1985) # raster 

# exploring resolution 
res(p50_2020) # resolution 0.000595209 0.000595209 degrees

# exploring projection
projection(p50_1985)
projection(p50_2020) # "+proj=longlat +datum=WGS84 +no_defs"
# "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
crs(p50_1985)

extracted_p50_1985 <- extract(p50_1985, xy = TRUE)

# extent 
extent(p50_1985)
# class      : Extent 
#xmin       : -154.8826 
#xmax       : -127.0697 
#ymin       : 59.43991 
#ymax       : 71.49051 
extent(p50_2020)
#class      : Extent 
#xmin       : -154.8826 
#xmax       : -127.0697 
#ymin       : 59.43991 
# ymax       : 71.49051 

# trying to extract boundary of raster 
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

shapefile(pp, file = "data/katie_map_border.shp")

#e <- extent(59.43991 ,71.49051,-154.8826 ,-127.0697 )
#extracted_p50_1985 <- extract(p50_1985, e)

# trying to aggregate pixels p50_2020
# resolution of climate map / resolution of shrub raster
#1/0.000595209 = 1680.082
#1.25/0.000595209 = 2100.103
# p50_2020_agg <- aggregate(p50_2020, fact=c(1680.082,2100.103), fun = mean, expand = TRUE) 
p50_2020_resample <- resample(p50_2020, hdd.cdd.2050)
plot(p50_2020_resample)
writeRaster(p50_2020_resample, "data/p50_2020_resample.tif")
res(p50_2020_resample) #1.25 1.00, same as climate!

#p50_2020_resample_df <- extract(p50_2020_resample, xy, cellnumbers = T)
#view(p50_2020_resample_df)

#p50_2020_random <- as.data.frame(sampleRandom(p50_2020_resample, 264, na.rm=TRUE, ext=NULL, 
 #                                           cells=TRUE, rowcol=FALSE, xy = TRUE)) 
#view(p50_2020_random)
#hist(p50_2020_random$pft_agb_deciduousshrub_p50_2020_wgs84)

# extract points from raster -----
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
    ggtitle("Shrub biomass cover (g/m2) of the PCH range (1985)\n") +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text

(gplot_p50_2020 <- gplot(p50_2020_resample) +
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

grid.arrange(gplot_p50_1985, gplot_p50_2020, nrow=1)



