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

# Extracting variables values for each pair of coordinates
chelsa.extract.2020 <- raster::extract(stack.2020, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2020)

chelsa.extract.2030 <- raster::extract(stack.2030, coords_sp, df = TRUE) # extract coords 
view(chelsa.extract.2030)

# Combining dataframes:
# Converting the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassigning the 'ID' to the coordinates dataframe
coord.df$ID <- row.names(coord.df)
coord.df$ID <- as.numeric(coord.df$ID) # Make numeric

# Merging the two dataframes: extracted CHELSA variables and the coordinates
coord.chelsa.combo.2020 <- left_join(chelsa.extract.2020, coord.df, by = c("ID" = "ID"))
coord.chelsa.combo.2030 <- left_join(chelsa.extract.2030, coord.df, by = c("ID" = "ID"))

# Loading the shrub biomass df
biomass.df <- shrub_map_extract %>%
  dplyr::select(-ID)%>%
  rename("ID" = "X", 
        "biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84") %>%
  dplyr::select(ID, biomass_per_m2, cell)

hist(biomass.df$biomass_per_m2) # normal distribution

# Merging biomass df with climate df
coord.chelsa.combo.a.2020 <- left_join(coord.chelsa.combo.2020, biomass.df, by = c("ID" = "ID"))
coord.chelsa.combo.a.2030 <- left_join(coord.chelsa.combo.2030, biomass.df, by = c("ID" = "ID"))

#making a year column
coord.chelsa.combo.b.2020 <- coord.chelsa.combo.a.2020 %>%
  mutate(year = rep(2020))

#making a year column
coord.chelsa.combo.b.2030 <- coord.chelsa.combo.a.2030 %>%
  mutate(year = rep(2030))
 

hist(coord.chelsa.combo.b.2020$layer.1, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.2, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.3, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.4, breaks = 20)
hist(coord.chelsa.combo.b.2020$layer.5, breaks = 20)

# Mean of all temps 
coord.chelsa.combo.c.2020 <- coord.chelsa.combo.b.2020 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

# Mean of all temps 
coord.chelsa.combo.c.2030 <- coord.chelsa.combo.b.2030 %>%
  mutate(mean_temp_C = (layer.1 + layer.2 +layer.3 +layer.4+layer.5+layer.6)/6) # good! 

# rbind 2020 and 2030 data
coord.chelsa.combo.c.2020.2030 <- rbind(coord.chelsa.combo.c.2020, coord.chelsa.combo.c.2030)
coord.chelsa.combo.c.2020.2030$year <- as.factor(coord.chelsa.combo.c.2020.2030$year)

# calculate difference between temps in 2020 and 2030
coord.chelsa.combo.c.delta <- coord.chelsa.combo.c.2020.2030 %>%
  group_by(cell) %>%
  mutate(delta = mean_temp_C[year == 2030] - mean_temp_C[year == 2020])%>%
  mutate(delta_2 = case_when(year == 2030 ~ delta, 
                              FALSE ~ NA)) %>%
  dplyr::select(- delta)

# do this for all years til 2100
view(coord.chelsa.combo.c.delta)

coord.chelsa.combo.c.biom.2020 <- coord.chelsa.combo.c.delta %>%
  filter(year == 2020)

# multiply by biomass increase
coord.chelsa.combo.c.biom <- coord.chelsa.combo.c.delta %>%
  filter(year == 2030) %>% 
  mutate(biomass_per_m2 = biomass_per_m2 + (124.5*delta_2))

# rebind with 2020 data
coord.chelsa.combo.c.2030.biom <- rbind(coord.chelsa.combo.c.biom.2020, coord.chelsa.combo.c.biom)

# Exporting the dataframe to csv 
write.csv(coord.chelsa.combo.c, "data/coord_chelsa_combo_c.csv")

# trying to plot 
# plotting biomass in 2020 
(raster_test_temp <- ggplot(coord.chelsa.combo.c.biom.2020) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    # scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient2(name = "Shrub biomass g/m2",high = "green4", mid = "yellow4", low = "brown", midpoint = 290,  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# plotting biomass in 2030
(raster_test_temp <- ggplot(coord.chelsa.combo.c.biom) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    # scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient2(name = "Shrub biomass g/m2",high = "green4", mid = "yellow4", low = "brown", midpoint = 290,  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
    xlab("\nLongitude") +
    ylab("Latitude\n"))
