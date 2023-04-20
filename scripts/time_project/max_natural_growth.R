# TIME PROJECTIONS: Max natural growth scenario

# Height data: In raw QHI data, extract max values and plot the line → take slope
# Cover: In raw ITEX data, extract max values and plot the line → take slope

# Height max: 0.70 +- 0.09 cm/year
# Cover max: 1.14 +-0.74 %/year

#No error:
# Salpul allom equation = 
# Biomass =  (1.1*0.70 ) + (18.1 *1.14 )
(1.1*0.70 ) + (18.1 *1.14 )
# 21.404 g/m2

# Positive error:
# Biomass =  (1.1*0.70) + 5.0+0.09 ) + (18.1 *1.14) +  8.2 + 0.74)
((1.1*0.70) + 5.0+0.09 ) + ((18.1 *1.14) +  8.2 + 0.74)
#35.434 g/m2

# Negative error:
((1.1*0.70) + 5.0-0.09 ) + ((18.1 *1.14) +  8.2 - 0.74)
#33.774

# When cover is 100%
(1.1*0.70 ) + (18.1 *100 )
#1810.77

# 2100 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
  dplyr::rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")%>%
  mutate(year = rep(2020))

shrub_map_project_max <- shrub_map_2020 %>%
  mutate(biomass_per_m2_new = biomass_per_m2 + ( 21.404*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep("2100_max"))

mean_2100_natural_max <- c(shrub_map_project_max$biomass_per_m2_new)
mean(mean_2100_natural_max)# 1940.592 g/m2

range(shrub_map_project_max$biomass_per_m2_2100)
shrub_map_2020 <- shrub_map_2020 %>%
  dplyr::rename("biomass_per_m2_new" = "biomass_per_m2")
mean_2020_natural_max <- c(shrub_map_2020$biomass_per_m2_new)
mean(mean_2020_natural_max)# 228.2723 g/m2

# %diff
(1940.592-228.2723)/228.2723
# 7.501215
# 750.1215%

# times larger
(1940.592/228.2723)
#8.501215

# bind data so that I can facet plot
shrub_natural_max <- rbind(shrub_map_2020, shrub_map_project_max)
unique(shrub_natural_max$year)

(raster_test_max <- ggplot(shrub_natural_max) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2_new))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


quantiles <- quantile(shrub_natural_max$biomass_per_m2_new)
quantiles
#  0%       25%       50%       75%      100% 
# 0.0000  174.4421 1712.3200 1886.7672 3838.3200 


# setting biomass level thresholds using quantiles
threshold_max <- shrub_natural_max %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 174.4421    & biomass_per_m2_new < 1886.7672 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 1886.7672 ~ 'High')) # 75%

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
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# binary threshold map
threshold_max_2 <- shrub_natural_max %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 1810.77     ~ 'Low', # threshold quant.
                                    biomass_per_m2_new > 1810.77 ~ 'High')) # threshold

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


