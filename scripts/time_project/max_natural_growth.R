# TIME PROJECTIONS: Max natural growth scenario

# Height data: In raw QHI data, extract max values and plot the line → take slope
# Cover: In raw ITEX data, extract max values and plot the line → take slope


# Height max: 0.70
# Cover max: 0.011

# Salpul allom equation = 
# Biomass =  (1.1*0.70 +-  5.0 ) + (18.1 *0.011 +-  8.2)
# 0.9691

# When cover is 100%
# Biomass =  (1.1*0.70 +-  5.0 ) + (18.1 *1 +-  8.2)
# 18.87*80 years = 1509.6

# 2100 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
  dplyr::rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")%>%
  mutate(year = rep(2020))

shrub_map_project_max <- shrub_map_2020 %>%
  mutate(biomass_per_m2_new = biomass_per_m2 + ( 0.9691*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2100))

range(shrub_map_project_max$biomass_per_m2_2100)
shrub_map_2020 <- shrub_map_2020 %>%
  dplyr::rename("biomass_per_m2_new" = "biomass_per_m2")

# bind data so that I can facet plot
shrub_natural_max <- rbind(shrub_map_2020, shrub_map_project_max)
unique(shrub_natural_max$year)

(raster_test_max <- ggplot(shrub_natural_max) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2_new))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "green4", low = "yellow1",  na.value="white") +
    coord_quickmap()+
    #theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


quantiles <- quantile(shrub_natural_max$biomass_per_m2_new)
quantiles
#  0%       25%       50%       75%      100% 
# 0.0000  127.5591  217.1520  350.0838 2203.5280 


# setting biomass level thresholds using quantiles
threshold_max <- shrub_natural_max %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 127.5591     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 127.5591    & biomass_per_m2_new < 350.0838 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 350.0838 ~ 'High')) # 75%

# ordering factor levels
threshold_max$biomass_level <- factor(threshold_max$biomass_level,levels=c("Low", "Medium", "High"),
                                      labels = c("Low", "Medium", "High"),
                                      ordered = T)

#threshold_avg_base <- threshold_avg_base %>% 
#rename(biomass_per_m2_new = biomass_per_m2)%>% 
#mutate(year = rep(2020))

#threshold_compare_avg <- rbind(threshold_avg_base, threshold_avg)

(raster_max <- ggplot(threshold_max) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
    #theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# binary threshold map
threshold_max_2 <- shrub_natural_max %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 1509.6     ~ 'Low', # threshold quant.
                                    biomass_per_m2_new > 1509.6 ~ 'High')) # threshold

# ordering factor levels
threshold_max_2$biomass_level <- factor(threshold_avg_2$biomass_level,levels=c("Low", "High"),
                                        labels = c("Low", "High"),
                                        ordered = T)

(raster_max_2 <- ggplot(threshold_max_2) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


