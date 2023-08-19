# BASE MAP: 2020 Katie Orndhal raster ----

# Download extracted data -----
shrub_map_extract_highest <- read.csv("data/maps_data/extract_end_highest.csv") # high res map

shrub_map_2020 <- shrub_map_extract_highest %>%
  rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84") 

# Plot base raster
(base_map <- ggplot(shrub_map_2020) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    #facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "green4", low = "yellow1",  na.value="white",
                        breaks = c(400,800,  1200,1600, 2000, 2400, 2800)) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


# Find quantiles
quantiles <- quantile(shrub_map_2020$biomass_per_m2)
quantiles
#0%        25%        50%        75%       100% 
# 0.00000   93.19772  174.44204  307.52018 2126.00000 

# setting biomass level thresholds using quantiles
threshold_avg_base <- shrub_map_2020 %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 93.19772     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 93.19772    & biomass_per_m2 < 307.52018 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 307.52018 ~ 'High')) # 75%

# ordering factor levels
threshold_avg_base$biomass_level <- factor(threshold_avg_base$biomass_level,levels=c("Low", "Medium", "High"),
                                      labels = c("Low", "Medium", "High"),
                                      ordered = T)

# plot with my palette
(raster_my_palette_new <- ggplot(threshold_avg_base) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# threshold map
threshold_avg_base_2 <- shrub_map_2020 %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 307.52018     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 307.52018 ~ 'High')) # 75%

# ordering factor levels
threshold_avg_base_2$biomass_level <- factor(threshold_avg_base_2$biomass_level,levels=c("Low", "High"),
                                        labels = c("Low", "High"),
                                        ordered = T)

(raster_my_palette_new <- ggplot(threshold_avg_base_2) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

