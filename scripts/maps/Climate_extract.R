# EXTRACTION FUTURE CLIMATE PROJECTIONS -----

# libraries
library(raster)
library(tidyverse)

# Load data -----
#shrub_map_extract <- read.csv("data/extract_end.csv") # low res map
#shrub_map_extract_high <- read.csv("data/extract_end_high.csv") # high res map
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map

# Loading climate rasters -------
tasmax.2020.1.re <- raster("outputs/CMPI6_rasters/tasmax.2020.1.re.tif")
tasmax.2020.2.re <- raster("outputs/CMPI6_rasters/tasmax.2020.2.re.tif")
tasmax.2020.3.re <- raster("outputs/CMPI6_rasters/tasmax.2020.3.re.tif")
tasmax.2020.4.re <- raster("outputs/CMPI6_rasters/tasmax.2020.4.re.tif")
tasmax.2020.5.re <- raster("outputs/CMPI6_rasters/tasmax.2020.5.re.tif")
tasmax.2020.6.re<- raster("outputs/CMPI6_rasters/tasmax.2020.6.re.tif")

tasmax.2030.1.re <- raster("outputs/CMPI6_rasters/tasmax.2030.1.re.tif")

tasmax.2030.2.re <- raster("outputs/CMPI6_rasters/tasmax.2030.2.re.tif")
tasmax.2030.3.re <- raster("outputs/CMPI6_rasters/tasmax.2030.3.re.tif")
tasmax.2030.4.re <- raster("outputs/CMPI6_rasters/tasmax.2030.4.re.tif")
tasmax.2030.5.re <- raster("outputs/CMPI6_rasters/tasmax.2030.5.re.tif")
tasmax.2030.6.re<- raster("outputs/CMPI6_rasters/tasmax.2030.6.re.tif")

tasmax.2040.1.re <- raster("outputs/CMPI6_rasters/tasmax.2040.1.re.tif")
tasmax.2040.2.re <- raster("outputs/CMPI6_rasters/tasmax.2040.2.re.tif")
tasmax.2040.3.re <- raster("outputs/CMPI6_rasters/tasmax.2040.3.re.tif")
tasmax.2040.4.re <- raster("outputs/CMPI6_rasters/tasmax.2040.4.re.tif")
tasmax.2040.5.re <- raster("outputs/CMPI6_rasters/tasmax.2040.5.re.tif")
tasmax.2040.6.re<- raster("outputs/CMPI6_rasters/tasmax.2040.6.re.tif")

tasmax.2050.1.re <- raster("outputs/CMPI6_rasters/tasmax.2050.1.re.tif")
tasmax.2050.2.re <- raster("outputs/CMPI6_rasters/tasmax.2050.2.re.tif")
tasmax.2050.3.re <- raster("outputs/CMPI6_rasters/tasmax.2050.3.re.tif")
tasmax.2050.4.re <- raster("outputs/CMPI6_rasters/tasmax.2050.4.re.tif")
tasmax.2050.5.re <- raster("outputs/CMPI6_rasters/tasmax.2050.5.re.tif")
tasmax.2050.6.re <- raster("outputs/CMPI6_rasters/tasmax.2050.6.re.tif")

tasmax.2060.1.re <- raster("outputs/CMPI6_rasters/tasmax.2060.1.re.tif")
tasmax.2060.2.re <- raster("outputs/CMPI6_rasters/tasmax.2060.2.re.tif")
tasmax.2060.3.re <- raster("outputs/CMPI6_rasters/tasmax.2060.3.re.tif")
tasmax.2060.4.re <- raster("outputs/CMPI6_rasters/tasmax.2060.4.re.tif")
tasmax.2060.5.re <- raster("outputs/CMPI6_rasters/tasmax.2060.5.re.tif")
tasmax.2060.6.re <- raster("outputs/CMPI6_rasters/tasmax.2060.6.re.tif")

tasmax.2070.1.re <- raster("outputs/CMPI6_rasters/tasmax.2070.1.re.tif")
tasmax.2070.2.re <- raster("outputs/CMPI6_rasters/tasmax.2070.2.re.tif")
tasmax.2070.3.re <- raster("outputs/CMPI6_rasters/tasmax.2070.3.re.tif")
tasmax.2070.4.re <- raster("outputs/CMPI6_rasters/tasmax.2070.4.re.tif")
tasmax.2070.5.re <- raster("outputs/CMPI6_rasters/tasmax.2070.5.re.tif")
tasmax.2070.6.re <- raster("outputs/CMPI6_rasters/tasmax.2070.6.re.tif")

tasmax.2080.1.re <- raster("outputs/CMPI6_rasters/tasmax.2080.1.re.tif")
tasmax.2080.2.re <- raster("outputs/CMPI6_rasters/tasmax.2080.2.re.tif")
tasmax.2080.3.re <- raster("outputs/CMPI6_rasters/tasmax.2080.3.re.tif")
tasmax.2080.4.re <- raster("outputs/CMPI6_rasters/tasmax.2080.4.re.tif")
tasmax.2080.5.re <- raster("outputs/CMPI6_rasters/tasmax.2080.5.re.tif")
tasmax.2080.6.re <- raster("outputs/CMPI6_rasters/tasmax.2080.6.re.tif")

tasmax.2090.1.re <- raster("outputs/CMPI6_rasters/tasmax.2090.1.re.tif")
tasmax.2090.2.re <- raster("outputs/CMPI6_rasters/tasmax.2090.2.re.tif")
tasmax.2090.3.re <- raster("outputs/CMPI6_rasters/tasmax.2090.3.re.tif")
tasmax.2090.4.re <- raster("outputs/CMPI6_rasters/tasmax.2090.4.re.tif")
tasmax.2090.5.re <- raster("outputs/CMPI6_rasters/tasmax.2090.5.re.tif")
tasmax.2090.6.re <- raster("outputs/CMPI6_rasters/tasmax.2090.6.re.tif")

tasmax.2100.1.re <- raster("outputs/CMPI6_rasters/tasmax.2100.1.re.tif")
tasmax.2100.2.re <- raster("outputs/CMPI6_rasters/tasmax.2100.2.re.tif")
tasmax.2100.3.re <- raster("outputs/CMPI6_rasters/tasmax.2100.3.re.tif")
tasmax.2100.4.re <- raster("outputs/CMPI6_rasters/tasmax.2100.4.re.tif")
tasmax.2100.5.re <- raster("outputs/CMPI6_rasters/tasmax.2100.5.re.tif")
tasmax.2100.6.re <- raster("outputs/CMPI6_rasters/tasmax.2100.6.re.tif")





# EXTRACTION ------
# Loading the coordinates of the cropped shrub map
coords <- shrub_map_extract_highest %>% 
  dplyr::select(x, y) # keeping lat and long

# Creating SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

# stack rasters of climate ? tasmax.2100.1, tasmax.2100.2, tasmax.2100.3...
stack.2020 <- stack(tasmax.2020.1.re, tasmax.2020.2.re,tasmax.2020.3.re,
                    tasmax.2020.4.re, tasmax.2020.5.re, tasmax.2020.6.re)

stack.2030 <- stack(tasmax.2030.1.re, tasmax.2030.2.re,tasmax.2030.3.re,
                    tasmax.2030.4.re, tasmax.2030.5.re, tasmax.2030.6.re)

stack.2040 <- stack(tasmax.2040.1.re, tasmax.2040.2.re,tasmax.2040.3.re,
                    tasmax.2040.4.re, tasmax.2040.5.re, tasmax.2040.6.re)

stack.2050 <- stack(tasmax.2050.1.re, tasmax.2050.2.re,tasmax.2050.3.re,
                    tasmax.2050.4.re, tasmax.2050.5.re, tasmax.2050.6.re)

stack.2060 <- stack(tasmax.2060.1.re, tasmax.2060.2.re,tasmax.2060.3.re,
                    tasmax.2060.4.re, tasmax.2060.5.re, tasmax.2060.6.re)

stack.2070 <- stack(tasmax.2070.1.re, tasmax.2070.2.re,tasmax.2070.3.re,
                    tasmax.2070.4.re, tasmax.2070.5.re, tasmax.2070.6.re)

stack.2080 <- stack(tasmax.2080.1.re, tasmax.2080.2.re,tasmax.2080.3.re,
                    tasmax.2080.4.re, tasmax.2080.5.re, tasmax.2080.6.re)

stack.2090 <- stack(tasmax.2090.1.re, tasmax.2090.2.re,tasmax.2090.3.re,
                    tasmax.2090.4.re, tasmax.2090.5.re, tasmax.2090.6.re)

stack.2100 <- stack(tasmax.2100.1.re, tasmax.2100.2.re,tasmax.2100.3.re,
                    tasmax.2100.4.re, tasmax.2100.5.re, tasmax.2100.6.re)


# Extracting variables values for each pair of coordinates
chelsa.extract.2020 <- raster::extract(stack.2020, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2020)

chelsa.extract.2030 <- raster::extract(stack.2030, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2030)

chelsa.extract.2040 <- raster::extract(stack.2040, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2040)

chelsa.extract.2050 <- raster::extract(stack.2050, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2050)

chelsa.extract.2060 <- raster::extract(stack.2060, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2060)

chelsa.extract.2070 <- raster::extract(stack.2070, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2070)

chelsa.extract.2080 <- raster::extract(stack.2080, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2080)

chelsa.extract.2090 <- raster::extract(stack.2090, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2090)

chelsa.extract.2100 <- raster::extract(stack.2100, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2100)

# Combining dataframes:
# Converting the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassigning the 'ID' to the coordinates dataframe
coord.df$ID <- row.names(coord.df)
coord.df$ID <- as.numeric(coord.df$ID) # Make numeric

# Merging the two dataframes: extracted CHELSA variables and the coordinates
coord.chelsa.combo.2020 <- left_join(chelsa.extract.2020, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2030 <- left_join(chelsa.extract.2030, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2040 <- left_join(chelsa.extract.2040, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2050 <- left_join(chelsa.extract.2050, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2060 <- left_join(chelsa.extract.2060, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2070 <- left_join(chelsa.extract.2070, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2080 <- left_join(chelsa.extract.2080, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2090 <- left_join(chelsa.extract.2090, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2100 <- left_join(chelsa.extract.2100, coord.df, by = c("ID" = "ID"))

# Loading the shrub biomass df
biomass.df <- shrub_map_extract_highest %>%
  dplyr::select(-ID)%>%
  dplyr::rename("ID" = "X", 
        "biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84") %>%
  dplyr::select(ID, biomass_per_m2, cell)

hist(biomass.df$biomass_per_m2) # right skewed distribution
range(biomass.df$biomass_per_m2) # 0 2126

# Merging biomass df with climate df
coord.chelsa.combo.a.2020 <- left_join(coord.chelsa.combo.2020, biomass.df, by = c("ID" = "ID")) # only 2020 biomass data
coord.chelsa.combo.a.2030 <- left_join(coord.chelsa.combo.2030, biomass.df, by = c("ID" = "ID")) # only 2020 biomass data
coord.chelsa.combo.a.2040 <- left_join(coord.chelsa.combo.2040, biomass.df, by = c("ID" = "ID"))  
  #mutate(biomass_per_m2 = rep(NA)) # remove 2020 biomass data?
coord.chelsa.combo.a.2050 <- left_join(coord.chelsa.combo.2050, biomass.df, by = c("ID" = "ID")) 
 # mutate(biomass_per_m2 = rep(NA))
coord.chelsa.combo.a.2060 <- left_join(coord.chelsa.combo.2060, biomass.df, by = c("ID" = "ID")) 
  #mutate(biomass_per_m2 = rep(NA))
coord.chelsa.combo.a.2070 <- left_join(coord.chelsa.combo.2070, biomass.df, by = c("ID" = "ID")) 
  #mutate(biomass_per_m2 = rep(NA))
coord.chelsa.combo.a.2080 <- left_join(coord.chelsa.combo.2080, biomass.df, by = c("ID" = "ID")) 
  #mutate(biomass_per_m2 = rep(NA))
coord.chelsa.combo.a.2090 <- left_join(coord.chelsa.combo.2090, biomass.df, by = c("ID" = "ID")) 
  #mutate(biomass_per_m2 = rep(NA))
coord.chelsa.combo.a.2100 <- left_join(coord.chelsa.combo.2100, biomass.df, by = c("ID" = "ID")) 
  #mutate(biomass_per_m2 = rep(NA))

coord.chelsa.combo.a.2100.solo <- left_join(coord.chelsa.combo.2100, biomass.df, by = c("ID" = "ID"))
glimpse(coord.chelsa.combo.a.2100.solo)

#making a year column
coord.chelsa.combo.b.2020 <- coord.chelsa.combo.a.2020 %>%
  mutate(year = rep(2020))

coord.chelsa.combo.b.2030 <- coord.chelsa.combo.a.2030 %>%
  mutate(year = rep(2030))

coord.chelsa.combo.b.2040 <- coord.chelsa.combo.a.2040 %>%
  mutate(year = rep(2040))

coord.chelsa.combo.b.2050 <- coord.chelsa.combo.a.2050 %>%
  mutate(year = rep(2050))

coord.chelsa.combo.b.2060 <- coord.chelsa.combo.a.2060 %>%
  mutate(year = rep(2060))

coord.chelsa.combo.b.2070 <- coord.chelsa.combo.a.2070 %>%
  mutate(year = rep(2070))

coord.chelsa.combo.b.2080 <- coord.chelsa.combo.a.2080 %>%
  mutate(year = rep(2080))

coord.chelsa.combo.b.2090 <- coord.chelsa.combo.a.2090 %>%
  mutate(year = rep(2090))

coord.chelsa.combo.b.2100 <- coord.chelsa.combo.a.2100 %>%
  mutate(year = rep(2100))

coord.chelsa.combo.b.2100.solo <- coord.chelsa.combo.a.2100.solo%>%
  mutate(year = rep(2100))

# explore
hist(coord.chelsa.combo.b.2020$layer.1, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.2, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.3, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.4, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.5, breaks = 20)

# Mean of all temps 
coord.chelsa.combo.c.2020 <- coord.chelsa.combo.b.2020 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2030 <- coord.chelsa.combo.b.2030 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2040 <- coord.chelsa.combo.b.2040 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2050 <- coord.chelsa.combo.b.2050 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2060 <- coord.chelsa.combo.b.2060 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2070 <- coord.chelsa.combo.b.2070 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2080 <- coord.chelsa.combo.b.2080 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2090 <- coord.chelsa.combo.b.2090 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2100 <- coord.chelsa.combo.b.2100 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

coord.chelsa.combo.c.2100.solo <- coord.chelsa.combo.b.2100.solo %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

# rbind all data 
coord.chelsa.combo.c.all <- rbind(coord.chelsa.combo.c.2020, coord.chelsa.combo.c.2030, 
                                  coord.chelsa.combo.c.2040, coord.chelsa.combo.c.2050,
                                  coord.chelsa.combo.c.2060, coord.chelsa.combo.c.2070,  
                                  coord.chelsa.combo.c.2080,coord.chelsa.combo.c.2090,
                                  coord.chelsa.combo.c.2100)

coord.chelsa.combo.c.all$year <- as.factor(coord.chelsa.combo.c.all$year)
unique(coord.chelsa.combo.c.all$year)

coord.chelsa.combo.c.all.2020.2100 <- rbind(coord.chelsa.combo.c.2020,coord.chelsa.combo.c.2100.solo)

write.csv(coord.chelsa.combo.c.all, "data/coord.chelsa.combo.c.all.csv")
write.csv(coord.chelsa.combo.c.all.2020.2100, "data/coord.chelsa.combo.c.all.2020.2100.csv")

# calculate difference between temps in 2020 and 2030 
coord.chelsa.combo.c.delta.2030 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2030)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta = mean_temp_C[year == 2030] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2040 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2040)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.1 = mean_temp_C[year == 2040] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2050 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2050)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.2 = mean_temp_C[year == 2050] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2060 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2060)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.3 = mean_temp_C[year == 2060] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2070 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2070)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.4 = mean_temp_C[year == 2070] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2080 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2080)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.5 = mean_temp_C[year == 2080] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2090 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2090)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.6 = mean_temp_C[year == 2090] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2100 <- coord.chelsa.combo.c.all %>%
  filter(year %in% c(2020, 2100)) %>%
  group_by(cell) %>%
  dplyr::mutate(delta.7 = mean_temp_C[year == 2100] - mean_temp_C[year == 2020])

coord.chelsa.combo.c.delta.2100.solo <- coord.chelsa.combo.c.all.2020.2100 %>%
  # filter(year %in% c(2020, 2100)) %>%
  group_by(cell) %>%
  mutate(delta.7.solo = mean_temp_C[year == 2100] - mean_temp_C[year == 2020])

# write.csv(coord.chelsa.combo.c.delta, "data/coord.chelsa.combo.c.delta.1.csv")
write.csv(coord.chelsa.combo.c.delta.2100.solo, "data/coord.chelsa.combo.c.delta.2100.solo.csv")
write.csv(coord.chelsa.combo.c.delta.2030, "data/coord.chelsa.combo.c.delta.2030.csv")
write.csv(coord.chelsa.combo.c.delta.2040, "data/coord.chelsa.combo.c.delta.2040.csv")
write.csv(coord.chelsa.combo.c.delta.2050, "data/coord.chelsa.combo.c.delta.2050.csv")
write.csv(coord.chelsa.combo.c.delta.2060, "data/coord.chelsa.combo.c.delta.2060.csv")
write.csv(coord.chelsa.combo.c.delta.2070, "data/coord.chelsa.combo.c.delta.2070.csv")
write.csv(coord.chelsa.combo.c.delta.2080, "data/coord.chelsa.combo.c.delta.2080.csv")
write.csv(coord.chelsa.combo.c.delta.2090, "data/coord.chelsa.combo.c.delta.2090.csv")
write.csv(coord.chelsa.combo.c.delta.2100, "data/coord.chelsa.combo.c.delta.2100.csv")

#coord.chelsa.combo.c.delta <- coord.chelsa.combo.c.delta %>%
 #  mutate(delta_2 = case_when(year == 2030 ~ delta,
   #                           year == 2040 ~ delta.1,
    #                          year == 2050 ~ delta.2,
     #                         year == 2060 ~ delta.3,
       #                       year == 2070 ~ delta.4,
        #                      year == 2080 ~ delta.5,
        #                      year == 2090 ~ delta.6,
         #                     year == 2100 ~ delta.7,
          #                    FALSE ~ NA)) %>%
 # dplyr::select(- delta, -delta.1, -delta.2,  -delta.3,  -delta.4,  -delta.5, -delta.6, -delta.7)

# save big dataset
write.csv(coord.chelsa.combo.c.delta, "data/coord.chelsa.combo.c.delta.csv")

# Static biomass in july 2020 -----
# filter only 2020
coord.chelsa.combo.c.biom.2020 <- coord.chelsa.combo.c.delta.2030 %>%
  filter(year == 2020)

c_mean_2020 <- c(coord.chelsa.combo.c.biom.2020$biomass_per_m2)
mean(c_mean_2020) # 228.2723 g/m2
range(coord.chelsa.combo.c.biom.2020$biomass_per_m2) # 0 2126 g/m2

c_meantemp_2020 <- c(coord.chelsa.combo.c.biom.2020$mean_temp_C)
mean(c_meantemp_2020) # 15.12132

write.csv(coord.chelsa.combo.c.biom.2020, "data/coord.chelsa.combo.c.biom.2020.csv")

# NB ignore below ------
# July 2030 projection -------

# multiply by biomass increase
coord.chelsa.combo.c.biom.2030 <- coord.chelsa.combo.c.delta.2030 %>%
  # filter(year == 2030) %>% 
  mutate(biomass_per_m2_2030 = biomass_per_m2 + (124.5*delta)) %>%
  filter(year == 2030)%>% 
  dplyr::select(-biomass_per_m2)

c_mean_2030<- c(coord.chelsa.combo.c.biom.2030$biomass_per_m2_2030)
mean(c_mean_2030) # 289.4231 g/m2
range(coord.chelsa.combo.c.biom.2030$biomass_per_m2_2030) # -12.25956 2212.50246

# percentage difference
( 289.4231 - 228.2723)/228.2723 
# = 0.2678853
# mean 27% increase in biomass between 2020-2030 

# July 2040 projection -------

# multiply by biomass increase
coord.chelsa.combo.c.biom.2040 <- coord.chelsa.combo.c.delta.2040 %>%
  filter(year == 2040)

coord.chelsa.combo.c.biom.2040.1 <- coord.chelsa.combo.c.biom.2040 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2030 %>% 
                   dplyr::select(biomass_per_m2_2030,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2040.2 <-coord.chelsa.combo.c.biom.2040.1 %>%
  mutate(biomass_per_m2_2040 = biomass_per_m2_2030 + (124.5*delta.1)) %>%
 dplyr::select(-biomass_per_m2_2030) 
 # rename(biomass_per_m2=biomass_per_m2_2040)

c_mean_2040 <- c(coord.chelsa.combo.c.biom.2040.2$biomass_per_m2_2040)
mean(c_mean_2040) # 282.363 g/m2
range(coord.chelsa.combo.c.biom.2040.2$biomass_per_m2_2040) # -2.012014 2181.518968

# percentage difference
(388.644- 354.4224)/354.4224 
# =  0.09655597
# mean 9.6% increase in biomass between 2030-2040

# I STOPPED HERE

# July 2050 projection -------

# multiply by biomass increase
coord.chelsa.combo.c.biom.2050 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2050)

coord.chelsa.combo.c.biom.2050.1 <- coord.chelsa.combo.c.biom.2050 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2040.2 %>% 
                     dplyr::select(biomass_per_m2_2040,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2050.2 <-coord.chelsa.combo.c.biom.2050.1 %>%
  mutate(biomass_per_m2_2050 = biomass_per_m2_2040 + (124.5*delta_2)) %>%
  dplyr::select(-biomass_per_m2_2040)
  #rename(biomass_per_m2=biomass_per_m2_2050)

c_mean_2050 <- c(coord.chelsa.combo.c.biom.2050.2$biomass_per_m2_2050)
mean(c_mean_2050) # 430.9143 g/m2
range(coord.chelsa.combo.c.biom.2050.2$biomass_per_m2_2050) #230.5347 729.7709

# percentage difference
(430.9143-388.644)/388.644 
# =   0.1087635
# 10.8% increase in biomass between 2040-2050

# July 2060 projection -------
# multiply by biomass increase
coord.chelsa.combo.c.biom.2060 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2060)

coord.chelsa.combo.c.biom.2060.1 <- coord.chelsa.combo.c.biom.2060 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2050.2 %>% 
                     dplyr::select(biomass_per_m2_2050,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2060.2 <-coord.chelsa.combo.c.biom.2060.1 %>%
  mutate(biomass_per_m2_2060 = biomass_per_m2_2050 + (124.5*delta_2)) %>%
dplyr::select(-biomass_per_m2_2050)

c_mean_2060 <- c(coord.chelsa.combo.c.biom.2060.2$biomass_per_m2_2060)
mean(c_mean_2060) # 415.2491 g/m2
range(coord.chelsa.combo.c.biom.2060.2$biomass_per_m2_2060) #224.3679 724.0134

# percentage difference
(415.2491-430.9143)/430.9143 
# -3.6% difference between 2050-2060 

# July 2070 projection -------

# multiply by biomass increase
coord.chelsa.combo.c.biom.2070 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2070) 

coord.chelsa.combo.c.biom.2070.1 <- coord.chelsa.combo.c.biom.2070 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2060.2 %>% 
                     dplyr::select(biomass_per_m2_2060,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2070.2 <-coord.chelsa.combo.c.biom.2070.1 %>%
  mutate(biomass_per_m2_2070 = biomass_per_m2_2060 + (124.5*delta_2)) %>%
dplyr::select(-biomass_per_m2_2060)

c_mean_2070 <- c(coord.chelsa.combo.c.biom.2070.2$biomass_per_m2_2070)
mean(c_mean_2070) #704.3320 g/m2
range(coord.chelsa.combo.c.biom.2070.2$biomass_per_m2_2070) #520.6172 1053.3688

# percentage difference
(704.3320-415.2491)/415.2491 
# 0.6961674, 69% increase

# July 2080 projection ------
# multiply by biomass increase
coord.chelsa.combo.c.biom.2080 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2080) 

coord.chelsa.combo.c.biom.2080.1 <- coord.chelsa.combo.c.biom.2080 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2070.2 %>% 
                     dplyr::select(biomass_per_m2_2070,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2080.2 <-coord.chelsa.combo.c.biom.2080.1 %>%
  mutate(biomass_per_m2_2080 = biomass_per_m2_2070 + (124.5*delta_2)) %>%
  dplyr::select(-biomass_per_m2_2070)

c_mean_2080 <- c(coord.chelsa.combo.c.biom.2080.2$biomass_per_m2_2080)
mean(c_mean_2080) #629.9409 g/m2
range(coord.chelsa.combo.c.biom.2080.2$biomass_per_m2_2080) #442.5663 946.8586

# percentage difference
(629.9409 -704.3320)/704.3320
# -10% 

# July 2090 projection ------
# multiply by biomass increase
coord.chelsa.combo.c.biom.2090 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2090) 

coord.chelsa.combo.c.biom.2090.1 <- coord.chelsa.combo.c.biom.2090 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2080.2 %>% 
                     dplyr::select(biomass_per_m2_2080,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2090.2 <-coord.chelsa.combo.c.biom.2090.1 %>%
  mutate(biomass_per_m2_2090 = biomass_per_m2_2080 + (124.5*delta_2)) %>%
  dplyr::select(-biomass_per_m2_2080)

c_mean_2090 <- c(coord.chelsa.combo.c.biom.2090.2$biomass_per_m2_2090)
mean(c_mean_2090) #779.509 g/m2
range(coord.chelsa.combo.c.biom.2090.2$biomass_per_m2_2090) #542.4439 1057.6583

# percentage difference
(779.509 -629.9409 )/629.9409
# 24%  increase

# July 2100 projection ------
# multiply by biomass increase
coord.chelsa.combo.c.biom.2100 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2100) 

coord.chelsa.combo.c.biom.2100.1 <- coord.chelsa.combo.c.biom.2100 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2090.2 %>% 
                     dplyr::select(biomass_per_m2_2090,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2100.2 <-coord.chelsa.combo.c.biom.2100.1 %>%
  mutate(biomass_per_m2_2100 = biomass_per_m2_2090 + (124.5*delta_2)) %>%
  dplyr::select(-biomass_per_m2_2090)

c_mean_2100 <- c(coord.chelsa.combo.c.biom.2100.2$biomass_per_m2_2100)
mean(c_mean_2100) # 895.5671g/m2
range(coord.chelsa.combo.c.biom.2100.2$biomass_per_m2_2100) #681.9673 1212.9567

# percentage difference
(895.5671- 779.509 )/779.509
#15%  increase

c_meantemp_2100 <- c(coord.chelsa.combo.c.biom.2100$mean_temp_C)
mean(c_meantemp_2100) # 18.63242

# OVERALL SHRUB INCREASE 2020-2100 (aka 2100 solo map)----
# multiply by biomass increase
coord.chelsa.combo.c.biom.2100.solo <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (124.5*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(coord.chelsa.combo.c.biom.2100.solo$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) # 857.2242 g/m2
range(coord.chelsa.combo.c.biom.2100.solo$biomass_per_m2_2100_solo) # 510.2024 2830.4307

c_mean_2100_temp_solo <- c(coord.chelsa.combo.c.biom.2100.solo$mean_temp_C)
mean(c_mean_2100_temp_solo) # 20.17315 C
range(coord.chelsa.combo.c.biom.2100.solo$mean_temp_C) #15.84172 22.42099


# bind 2020 and 2100
coord.chelsa.combo.c.biom.2100.solo.bind <- coord.chelsa.combo.c.biom.2100.solo %>%
  rename(biomass_per_m2 = biomass_per_m2_2100_solo)

coord.chelsa.combo.c.biom.2100.solo.bind$year <- as.factor(coord.chelsa.combo.c.biom.2100.solo.bind$year)

coord.chelsa.combo.c.final.2100 <- rbind(coord.chelsa.combo.c.biom.2020, coord.chelsa.combo.c.biom.2100.solo.bind)

write.csv(coord.chelsa.combo.c.final.2100, "data/coord.chelsa.combo.c.final.2100.csv")

# mean in 2020: 228.2723
# mean in 2100:  857.2242
(857.2242-228.2723)/228.2723
# 275.527 % increase in shrub biomass !

# OVERALL TEMP INCREASE ----
# mean temp 2020:15.12132
# mean temp 2100: 20.17315
(20.17315-15.12132)/15.12132
#  33.41 % temp difference
# 5 degrees warming difference

# merge all data-----
# rename all specific biomass_per_m2_YEAR to biomass_per_m2 
coord.chelsa.combo.c.biom.2030.3 <- coord.chelsa.combo.c.biom.2030 %>%
  rename(biomass_per_m2 = biomass_per_m2_2030)

coord.chelsa.combo.c.biom.2040.3 <- coord.chelsa.combo.c.biom.2040.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2040)

coord.chelsa.combo.c.biom.2050.3 <- coord.chelsa.combo.c.biom.2050.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2050)

coord.chelsa.combo.c.biom.2060.3 <- coord.chelsa.combo.c.biom.2060.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2060)

coord.chelsa.combo.c.biom.2070.3 <- coord.chelsa.combo.c.biom.2070.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2070)

coord.chelsa.combo.c.biom.2080.3 <- coord.chelsa.combo.c.biom.2080.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2080)

coord.chelsa.combo.c.biom.2090.3 <- coord.chelsa.combo.c.biom.2090.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2090)

coord.chelsa.combo.c.biom.2100.3 <- coord.chelsa.combo.c.biom.2100.2 %>%
  rename(biomass_per_m2 = biomass_per_m2_2100)

# rebind data: and bind correct dataframes
coord.chelsa.combo.c.all.biom <- rbind(coord.chelsa.combo.c.biom.2020, coord.chelsa.combo.c.biom.2030.3, coord.chelsa.combo.c.biom.2040.3,
                                       coord.chelsa.combo.c.biom.2050.3,coord.chelsa.combo.c.biom.2060.3, coord.chelsa.combo.c.biom.2070.3, 
                                       coord.chelsa.combo.c.biom.2080.3, coord.chelsa.combo.c.biom.2090.3, coord.chelsa.combo.c.biom.2100.3)

# Exporting the dataframe to csv 
write.csv(coord.chelsa.combo.c.all.biom, "data/coord.chelsa.combo.c.all.biom.csv")

# Data visualisation -------
# trying to plot 

# plotting facet biomass (yellow-green)
(raster_test_temp <- ggplot(coord.chelsa.combo.c.final.2100) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "green4", low = "yellow1",  na.value="white",
                        breaks = c(0, 400,800,  1200,1600, 2000, 2400, 2800)) +
    coord_quickmap()+
    theme_shrub() +  
   theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
   theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# ggsave(raster_test_temp, "outputs/map_2.pdf") # doesnt save too heavy

# plotting facet climate (blue red)
(raster_test_temp <- ggplot(coord.chelsa.combo.c.final.2100) + 
    geom_tile(aes(x=x,y=y,fill=mean_temp_C)) + 
    facet_wrap(~year,  nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Mean july temperature (degC)",high = 'red', low = "blue4",  na.value="white",
                        breaks = c(7.5, 9.5, 11.5,  13.5,15.5, 17.5, 19.5, 21.5))+
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# plotting facet biomass!
(raster_test_temp <- ggplot(coord.chelsa.combo.c.all.biom) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_grid(~year) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient2(name = "Shrub biomass g/m2",high = "green4", mid = "yellow4", low = "yellow1", midpoint = 400,  na.value="white",
                         breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n"))


# plotting biomass in 2030
(raster_test_temp <- ggplot(coord.chelsa.combo.c.biom) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    # scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient2(name = "Shrub biomass g/m2",high = "green4", mid = "yellow4", low = "yellow1", midpoint = 400,  na.value="white", 
                         breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n"))
