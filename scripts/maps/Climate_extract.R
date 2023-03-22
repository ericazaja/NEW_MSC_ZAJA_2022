# EXTRACTION FUTURE CLIMATE PROJECTIONS -----

# Load data -----
shrub_map_extract <- read.csv("data/extract_end.csv") 

# EXTRACTION ------
# Loading the coordinates of the cropped shrub map
coords <- shrub_map_extract %>% 
  dplyr::select(x, y) # keeping lat and long

# Creating SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

# stack rasters of climate ? tasmax.2100.1, tasmax.2100.2, tasmax.2100.3...
stack.2020 <- stack(tasmax.2020.1, tasmax.2020.2.re,tasmax.2020.3.re,
                    tasmax.2020.4.re, tasmax.2020.5.re, tasmax.2020.6.re)

stack.2030 <- stack(tasmax.2030.1, tasmax.2030.2.re,tasmax.2030.3.re,
                    tasmax.2030.4.re, tasmax.2030.5.re, tasmax.2030.6.re)

stack.2040 <- stack(tasmax.2040.1, tasmax.2040.2.re,tasmax.2040.3.re,
                    tasmax.2040.4.re, tasmax.2040.5.re, tasmax.2040.6.re)

stack.2050 <- stack(tasmax.2050.1, tasmax.2050.2.re,tasmax.2050.3.re,
                    tasmax.2050.4.re, tasmax.2050.5.re, tasmax.2050.6.re)

stack.2060 <- stack(tasmax.2060.1, tasmax.2060.2.re,tasmax.2060.3.re,
                    tasmax.2060.4.re, tasmax.2060.5.re, tasmax.2060.6.re)


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

# Loading the shrub biomass df
biomass.df <- shrub_map_extract %>%
  dplyr::select(-ID)%>%
  rename("ID" = "X", 
        "biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84") %>%
  dplyr::select(ID, biomass_per_m2, cell)

hist(biomass.df$biomass_per_m2) # normal distribution

# Merging biomass df with climate df
coord.chelsa.combo.a.2020 <- left_join(coord.chelsa.combo.2020, biomass.df, by = c("ID" = "ID")) # only 2020 biomass data
coord.chelsa.combo.a.2030 <- left_join(coord.chelsa.combo.2030, biomass.df, by = c("ID" = "ID")) # only 2020 biomass data
coord.chelsa.combo.a.2040 <- left_join(coord.chelsa.combo.2040, biomass.df, by = c("ID" = "ID")) %>%
  mutate(biomass_per_m2 = rep(NA))# remove 2020 biomass data
coord.chelsa.combo.a.2050 <- left_join(coord.chelsa.combo.2050, biomass.df, by = c("ID" = "ID")) %>%
  mutate(biomass_per_m2 = rep(NA))# remove 2020 biomass data
coord.chelsa.combo.a.2060 <- left_join(coord.chelsa.combo.2060, biomass.df, by = c("ID" = "ID")) # only 2020 biomass data


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

# rbind all data 
coord.chelsa.combo.c.all <- rbind(coord.chelsa.combo.c.2020, coord.chelsa.combo.c.2030, 
                                  coord.chelsa.combo.c.2040, coord.chelsa.combo.c.2050,
                                  coord.chelsa.combo.c.2060)
coord.chelsa.combo.c.all$year <- as.factor(coord.chelsa.combo.c.all$year)

# calculate difference between temps in 2020 and 2030
coord.chelsa.combo.c.delta <- coord.chelsa.combo.c.all %>%
  group_by(cell) %>%
  mutate(delta = mean_temp_C[year == 2030] - mean_temp_C[year == 2020])%>%
  mutate(delta.1 = mean_temp_C[year == 2040] - mean_temp_C[year == 2030])%>%
  mutate(delta.2 = mean_temp_C[year == 2050] - mean_temp_C[year == 2040])%>%
  mutate(delta.3 = mean_temp_C[year == 2060] - mean_temp_C[year == 2050])%>%
   mutate(delta_2 = case_when(year == 2030 ~ delta,
                              year == 2040 ~ delta.1,
                              year == 2050 ~ delta.2,
                              year == 2060 ~ delta.3,
                              FALSE ~ NA)) %>%
  dplyr::select(- delta, -delta.1, -delta.2,  -delta.3)

# do this for all years til 2100
view(coord.chelsa.combo.c.delta)

# filter only 2020
coord.chelsa.combo.c.biom.2020 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2020)

c_mean_2020 <- c(coord.chelsa.combo.c.biom.2020$biomass_per_m2)
mean(c_mean_2020) # 225.4707 g/m2
range(coord.chelsa.combo.c.biom.2020$biomass_per_m2) # 4.766095 495.623108

# multiply by biomass increase
coord.chelsa.combo.c.biom.2030 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2030) %>% 
  mutate(biomass_per_m2_2030 = biomass_per_m2 + (124.5*delta_2)) 
  
c_mean <- c(coord.chelsa.combo.c.biom.2030$biomass_per_m2_2030)
mean(c_mean) # 354.4224 g/m2
range(coord.chelsa.combo.c.biom.2030$biomass_per_m2_2030) # 104.1922 722.9597
coord.chelsa.combo.c.biom.2030 <- coord.chelsa.combo.c.biom.2030 %>%
  dplyr::select(-biomass_per_m2)

# percentage difference
(354.4224 - 225.4707)/225.4707 
# =  0.5719222 
# 57% increase in biomass

# multiply by biomass increase
coord.chelsa.combo.c.biom.2040 <- coord.chelsa.combo.c.delta %>%
  filter(year ==2040) 

coord.chelsa.combo.c.biom.2040.1 <- coord.chelsa.combo.c.biom.2040 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2030 %>% 
                     dplyr::select(biomass_per_m2_2030,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2040.2 <-coord.chelsa.combo.c.biom.2040.1 %>%
  mutate(biomass_per_m2_2040 = biomass_per_m2_2030 + (124.5*delta_2)) 
 # dplyr::select(-biomass_per_m2_2030) %>%
 # rename(biomass_per_m2=biomass_per_m2_2040)

c_mean <- c(coord.chelsa.combo.c.biom.2040.2$biomass_per_m2)
mean(c_mean) # 388.644 g/m2
range(coord.chelsa.combo.c.biom.2040.2$biomass_per_m2) # 162.2563 735.7610

# percentage difference
(388.644- 354.4224)/354.4224 
# =  0.09655597
# 9.6% increase in biomass

# multiply by biomass increase
coord.chelsa.combo.c.biom.2050 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2050) 

coord.chelsa.combo.c.biom.2050.1 <- coord.chelsa.combo.c.biom.2050 %>% 
  dplyr::left_join(coord.chelsa.combo.c.biom.2040.2 %>% 
                     dplyr::select(biomass_per_m2_2040,by = c("ID" = "ID")))%>% 
  dplyr::select(-biomass_per_m2)

coord.chelsa.combo.c.biom.2050.2 <-coord.chelsa.combo.c.biom.2050.1 %>%
  mutate(biomass_per_m2_2050 = biomass_per_m2_2040 + (124.5*delta_2)) 
  #dplyr::select(-biomass_per_m2_2040) %>%
  #rename(biomass_per_m2=biomass_per_m2_2050)

c_mean <- c(coord.chelsa.combo.c.biom.2050.2$biomass_per_m2)
mean(c_mean) # 430.9143 g/m2
range(coord.chelsa.combo.c.biom.2050.2$biomass_per_m2) #230.5347 729.7709

# percentage difference
(430.9143-388.644)/388.644 
# =   0.1087635
# 10.8% increase in biomass

# multiply by biomass increase
coord.chelsa.combo.c.biom.2060 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2060) %>% 
  mutate(biomass_per_m2 = biomass_per_m2 + (124.5*delta_2))

c_mean <- c(coord.chelsa.combo.c.biom.2060$biomass_per_m2)
mean(c_mean) # 415.2491 g/m2
range(coord.chelsa.combo.c.biom.2060$biomass_per_m2) #224.3679 724.0134

# percentage difference
(430.9143-388.644)/388.644 

# rebind data: rename all specific biomass_per_m2_YEAR to biomass_per_m2 and bind correct dataframes
coord.chelsa.combo.c.all.biom <- rbind(coord.chelsa.combo.c.biom.2020, coord.chelsa.combo.c.biom.2030, coord.chelsa.combo.c.biom.2040,
                                       coord.chelsa.combo.c.biom.2050,coord.chelsa.combo.c.biom.2060)

# Exporting the dataframe to csv 
write.csv(coord.chelsa.combo.c, "data/coord_chelsa_combo_c.csv")

# Data visualisation -------
# trying to plot 

# plotting facet biomass (yellow-green)
(raster_test_temp <- ggplot(coord.chelsa.combo.c.all.biom) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_grid(~year) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "green4", low = "yellow1",  na.value="white",
                        breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# plotting facet climate (blue red)
(raster_test_temp <- ggplot(coord.chelsa.combo.c.all.biom) + 
    geom_tile(aes(x=x,y=y,fill=mean_temp_C)) + 
    facet_grid(~year) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Mean july temperature (degC)",high = 'red', low = "blue4",  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
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
