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

# 2. LOADING DATA ----

# Loading rasters of shrub biomass (g/m2) in the PCH range from 1985-2020
# Using the best-estimates: the 50th percentile of the 1,000 permutations

p50_1985 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1985.tif") 
p50_1990 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1990.tif") 
p50_1995 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_1995.tif") 
p50_2000 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2000.tif") 
p50_2005 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2005.tif") 
p50_2010 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2010.tif") 
p50_2015 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2015.tif") 
p50_2020 <- raster("data/katie_maps/pft_agb_deciduousshrub_p50_2020.tif") 

# Rasters tell us where there is shrub and where there isn't (cover)
# if there is shrub, what is their biomass

# 3. DATA EXPLORE  -----
plot(p50_1985)

class(p50_1985) # raster 

# exploring resolution 
res(p50_1985) # resolution 30m x 30m

# exploring projection
projection(p50_1985)
# "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
crs(p50_1985)

# extent 
extent(p50_1985)
# class      : Extent 
#xmin       : -2250120 
#xmax       : -1499550 
#ymin       : 2904500 
#ymax       : 4038860 

# p50_1985_latlong <- projectRaster(p50_1985, crs="+init=EPSG:4326", xy = TRUE) # doesnt run
spts <- rasterToPoints(p50_1985, spatial = TRUE)

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
(gplot_p50_1985 <- gplot(p50_1985) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    # value is the specific value (of reflectance) each pixel is associated with
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      ifelse(x<500, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 500)),1)}, na.value="white") +
    coord_quickmap()+
    theme_shrub() +  # Remove ugly grey background
    xlab("\nLongitude") +
    ylab("Latitude\n") +
    ggtitle("Shrub biomass (g/m2) in PCH range (1985)\n") +
    theme(plot.title = element_text(hjust = 0.5),     # centres plot title
          text = element_text(size=15),		       	    # font size
          axis.text.x = element_text(angle = 30, hjust = 1)))  # rotates x axis text


# Cropped map with personalised colour palette (low-mid-high) 
(gplot_p50_1985_test_my_palette <- gplot(p50_1985) +
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

(gplot_p50_2020 <- gplot(p50_2020) +
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

