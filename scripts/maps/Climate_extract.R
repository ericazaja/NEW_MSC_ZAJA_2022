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
# Extracting variables values for each pair of coordinates
chelsa.extract <- raster::extract(tasmax.2100, coords_sp, df = TRUE) # extract coords 

# Combining dataframes:
# Converting the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassigning the 'ID' to the coordinates dataframe
coord.df$ID <- row.names(coord.df)
coord.df$ID <- as.numeric(coord.df$ID) # Make numeric

# Merging the two dataframes: extracted CHELSA variables and the coordinates
coord.chelsa.combo <- left_join(chelsa.extract, coord.df, by = c("ID" = "ID"))

# Loading the shrub biomass df
biomass.df <- shrub_map_extract %>%
  dplyr::select(-ID)%>%
  rename("ID" = "X", 
        "biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84") %>%
  dplyr::select(ID, biomass_per_m2, cell)

hist(biomass.df$biomass_per_m2)

# Merging biomass df with climate df
coord.chelsa.combo.a <- left_join(coord.chelsa.combo, biomass.df, by = c("ID" = "ID"))

#renaming and making a year column
coord.chelsa.combo.b <- coord.chelsa.combo.a %>%
  rename("july_temp" = "layer")%>%
  mutate(year = rep(2100))

unique(coord.chelsa.combo.b$july_temp)
range(coord.chelsa.combo.b$july_temp) # 2.158533 14.562433
hist(coord.chelsa.combo.b$july_temp, breaks = 20)

# Exporting the dataframe to csv
# write.csv(coord.chelsa.combo.b, "data/coord_chelsa_combo_b.csv")
