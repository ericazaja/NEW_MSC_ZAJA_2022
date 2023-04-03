# TIME PROJECTIONS: Average natural growth scenario
#  Mean amount of growth, what has been happening on average 
# in real world (~baseline)

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
# # Height slope  for S pulchra  = ???
# Cover slope for S pulchra = 0

# Salpul allom equation = 
# Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)
# add + high error ? 

# 2100 projection
shrub_map_2022 <- shrub_map_extract_highest %>%
  rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84") 

shrub_map_project_mean <- shrub_map_2022 %>%
  mutate(biomass_per_m2_2100 = biomass_per_m2 + (5.11*80))%>%
  dplyr::select(-biomass_per_m2)

range(shrub_map_project_mean$biomass_per_m2_2100)

(raster_test_avg <- ggplot(shrub_map_project_mean) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2_2100))) + 
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

# THRESHOLD MAPS -----

# Find quantiles
quantiles <- quantile(shrub_map_project_mean$biomass_per_m2_2100)
quantiles
#  0%       25%       50%       75%      100% 
# 408.8000  501.9977  583.2420  716.3202 2534.8000 

# setting biomass level thresholds using quantiles
threshold_avg <- shrub_map_project_mean %>%
  mutate(biomass_level = case_when (biomass_per_m2_2100 < 501.9977     ~ 'Low', # 25% quant.
                                    biomass_per_m2_2100> 501.9977    & biomass_per_m2_2100 < 716.3202 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_2100 > 716.3202 ~ 'High')) # 75%

# ordering factor levels
threshold_avg$biomass_level <- factor(threshold_avg$biomass_level,levels=c("Low", "Medium", "High"),
                                          labels = c("Low", "Medium", "High"),
                                          ordered = T)

(raster_my_palette_new <- ggplot(threshold_avg) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# threshold map
threshold_avg_2 <- shrub_map_project_mean %>%
  mutate(biomass_level = case_when (biomass_per_m2_2100 < 716.3202     ~ 'Low', # 75% quant.
                                    biomass_per_m2_2100 > 716.3202 ~ 'High')) # 75%

# ordering factor levels
threshold_avg_2$biomass_level <- factor(threshold_avg_2$biomass_level,levels=c("Low", "High"),
                                      labels = c("Low", "High"),
                                      ordered = T)

(raster_my_palette_new <- ggplot(threshold_avg_2) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))
