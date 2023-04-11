# TIME PROJECTIONS: Average natural growth scenario
#  Mean amount of growth, what has been happening on average 
# in real world (~baseline)

# libraries -----
library(tidyverse)

# Data 
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map

# Data needed: 
# 1. Height data: QHI monitoring 1999 -2022. Height over time → slope (cm per year). 
# see script: bayes_height_over_time.R (QHI heights analysis)

# 2. Cover data: ITEX QHI, TOOLIK, ANWR. Cover over time → slope (prop. cover per year). 
# See script bayes_ITEX_cover.R (ITEX covers over time)

# Put these two SLOPES into the allometric equations to obtain biomass
# Multiply the obtained biomass by 80 years and add to existing 2022 biomass

# SLOPES:
# MEAN Height slope  for S pulchra  = 0.34 cm/year
# MEAN Cover slope for S pulchra =  0.011 /year

# Put these into the Salpul allometric equation = 
# Biomass =  (1.1*0.34 +-  5.0 ) + (18.1 *0.011 +-  8.2)
# add + high error ? 
# 0.5731 g/m2

# When COVER is 1 (100%) 
# Biomass =  (1.1*0.34 +-  5.0 ) + (18.1 *1 +-  8.2)
# 18.474 
# 18.474*80 years = 1477.92 threshold of connectivity 

# 2100 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
  dplyr::rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")%>%
  mutate(year = rep(2020))

shrub_map_project_mean <- shrub_map_2020 %>%
  dplyr::mutate(biomass_per_m2_new = biomass_per_m2 + (0.5731*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2100))

range(shrub_map_project_mean$biomass_per_m2_new) #  45.848 2171.848
mean_2100_natural <- c(shrub_map_project_mean$biomass_per_m2_new)
mean(mean_2100_natural)# 274.1203 g/m2

shrub_map_2020 <- shrub_map_2020 %>%
  dplyr::rename("biomass_per_m2_new" = "biomass_per_m2")
mean_2020_natural <- c(shrub_map_2020$biomass_per_m2_new)
mean(mean_2020_natural) # 228.2723 g/m2

# %diff 
(274.1203 -228.2723)/228.2723
# 0.2008478

# how many times bigger
#274.1203/228.2723
# 1.200848 times bigger

# bind data so that I can facet plot
shrub_natural_mean <- rbind(shrub_map_2020, shrub_map_project_mean)
unique(shrub_natural_mean$year)

(raster_test_avg <- ggplot(shrub_natural_mean) + 
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



# THRESHOLD MAPS -----

# Find quantiles
quantiles <- quantile(shrub_natural_mean$biomass_per_m2_new)
quantiles
#  0%       25%       50%       75%      100% 
# 0.0000  114.5488  198.7810  331.7651 2171.8480 

# setting biomass level thresholds using quantiles
threshold_avg <- shrub_natural_mean %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 114.5488     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 114.5488    & biomass_per_m2_new < 331.7651 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 331.7651 ~ 'High')) # 75%

hist(shrub_natural_mean$biomass_per_m2_new)

# ordering factor levels
threshold_avg$biomass_level <- factor(threshold_avg$biomass_level,levels=c("Low", "Medium", "High"),
                                          labels = c("Low", "Medium", "High"),
                                          ordered = T)

#threshold_avg_base <- threshold_avg_base %>% 
  #rename(biomass_per_m2_new = biomass_per_m2)%>% 
  #mutate(year = rep(2020))

#threshold_compare_avg <- rbind(threshold_avg_base, threshold_avg)

(raster_my_palette_new <- ggplot(threshold_avg) + 
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
threshold_avg_2 <- shrub_natural_mean %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 1477.92     ~ 'Low', # 75% quant.
                                    biomass_per_m2_new > 1477.92 ~ 'High')) # 75%

# ordering factor levels
threshold_avg_2$biomass_level <- factor(threshold_avg_2$biomass_level,levels=c("Low", "High"),
                                      labels = c("Low", "High"),
                                      ordered = T)

(raster_my_palette_new <- ggplot(threshold_avg_2) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# only 2100 raster
(raster_2100 <- ggplot(shrub_map_project_mean) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2_new))) + 
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white") +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))



# Save the plot
ggsave(plot=raster_2100, "my_ggplot.tiff", device = "tiff")
