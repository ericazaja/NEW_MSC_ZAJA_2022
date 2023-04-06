# TIME PROJECTIONS: Average natural growth scenario
#  Mean amount of growth, what has been happening on average 
# in real world (~baseline)

library(tidyverse)
# Data 
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map

# Data needed: 
# 1. Height data: QHI monitoring 1999 -2022. Height over time → slope (cm per year). 
# see script: bayes_height_over_time.R (QHI heights analysis)

# 2. Cover data: ITEX QHI, TOOLIK, ANWR. Cover over time → slope (% cover per year). 
# See script bayes_ITEX_cover.R (ITEX covers over time)

# Put these two SLOPES into the allometric equations to obtain biomass
# Multiply the obtained biomass by 80 years and add to existing 2022 biomass

# SLOPES:
# # MEAN Height slope  for S pulchra  = 0.34
# Cover slope for S pulchra = 0

# Salpul allom equation = 
# Biomass =  (1.1*0.34 +-  5.0 ) + (18.1 *cover +-  8.2)
# add + high error ? 

# 2100 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
  rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")%>%
  mutate(year = rep(2020))

shrub_map_project_mean <- shrub_map_2020 %>%
  mutate(biomass_per_m2_new = biomass_per_m2 + (0.374*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2100))

range(shrub_map_project_mean$biomass_per_m2_2100)
shrub_map_2020 <- shrub_map_2020 %>%
  rename("biomass_per_m2_new" = "biomass_per_m2")

# bind data so that I can facet plot
shrub_natural_mean <- rbind(shrub_map_2020, shrub_map_project_mean)
unique(shrub_natural_mean$year)

(raster_test_avg <- ggplot(shrub_natural_mean) + 
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

# THRESHOLD MAPS -----

# Find quantiles
quantiles <- quantile(shrub_natural_mean$biomass_per_m2_new)
quantiles
#0%       25%       50%       75%      100% 
#0.0000  174.4421  449.3872  605.1821 2534.8000 

# setting biomass level thresholds using quantiles
threshold_avg <- shrub_natural_mean %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 174.4421    & biomass_per_m2_new < 605.1821 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 605.1821 ~ 'High')) # 75%

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
  mutate(biomass_level = case_when (biomass_per_m2_new < 605.1821     ~ 'Low', # 75% quant.
                                    biomass_per_m2_new > 605.1821 ~ 'High')) # 75%

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
