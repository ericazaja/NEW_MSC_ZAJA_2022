# TIME PROJECTIONS: Novel growth scenario
# We brought shrubs from the boreal forest into the common garden 
# and gave them 9 years to grow. How much do they grow? 
# NB only using southern shrubs because based on our common garden study 
# we assume that the northern shrubs have weird phenology because of photoperiod 
# adaptation so we won’t consider their growth rates.

shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map

# Height: height over time (script:bayes_CG_height_elong.R)
# Cover: cover over time (script: )
# → get biomass 

# height slope richardsonii = 1.74 cm/year
# cover slope richardsonii = 0.66
# RICHARDSONII FINAL EQUATION: Biomass =  (18.0*height +- 5.1) + (11.9 *cover +-  18.0)
(18.0*1.74  ) + (11.9 *0.7 ) 
#  39.65

# if cover is 100%
(18.0*1.74  ) + (11.9 *1) 
# 43.22

# height slope pulchra = 1
# cover slope pulchra = 0.24
# PULCHRA FINAL EQUATION:  Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)
(1.1*1 ) + (18.1 *0.24 )
#  5.444

# if cover is 100%
(1.1*1 ) + (18.1 *1 )
# 19.2

# average biomass for s rich + s pul
(39.65 +5.444)/2
# 22.547 g/m2

# with cover 100%, average biomass for s rich + s pul
(43.22 +19.2)/2
# 31.21 g/m2
31.21*80
# 2496.8 threshold?


# 2100 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
  dplyr::rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")  %>%
  mutate(year = rep(2020))

shrub_map_project_novel <- shrub_map_2020 %>%
  dplyr::mutate(biomass_per_m2_new = biomass_per_m2 + (22.547*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2100))

mean_2100_novel <- c(shrub_map_project_novel$biomass_per_m2_new)
mean(mean_2100_novel)# 2032.032 g/m2

hist(shrub_map_project_novel$biomass_per_m2_new)

shrub_map_2020 <- shrub_map_2020 %>%
  dplyr::rename("biomass_per_m2_new" = "biomass_per_m2")

# % diff
(2032.032-228.2723)/228.2723

# times larger
(2032.032/228.2723)
# 8.90179

# bind data so that I can facet plot
shrub_novel <- rbind(shrub_map_2020, shrub_map_project_novel)

(raster_test_novel <- ggplot(shrub_novel) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2_new))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white")+
    coord_quickmap()+
   theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


# THRESHOLD MAPS -----
quantiles_novel <- quantile(shrub_novel$biomass_per_m2_new)
quantiles_novel
#  0%       25%       50%       75%      100% 
# 0.0000  174.4421 1803.7600 1978.2025 3929.7600 

# setting biomass level thresholds using quantiles
threshold_novel <- shrub_novel %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 174.4421    & biomass_per_m2_new < 1978.2025 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 1978.2025 ~ 'High')) # 75%

# ordering factor levels
threshold_novel$biomass_level <- factor(threshold_novel$biomass_level,levels=c("Low", "Medium", "High"),
                                      labels = c("Low", "Medium", "High"),
                                      ordered = T)

(threshold_novel_levels <- ggplot(threshold_novel) + 
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
threshold_novel_bi <- shrub_novel %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 2496.8     ~ 'Low', # 75% quant.
                                    biomass_per_m2_new > 2496.8 ~ 'High')) # 75%

# ordering factor levels
threshold_novel_bi$biomass_level <- factor(threshold_novel_bi$biomass_level,levels=c("Low", "High"),
                                        labels = c("Low", "High"),
                                        ordered = T)

(treshold_novel_bi <- ggplot(threshold_novel_bi) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
   theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

