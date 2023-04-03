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

# height slope richardsonii = 1.74 
# cover slope richardsonii = 66
# RICHARDSONII FINAL EQUATION: Biomass =  (18.0*height +- 5.1) + (11.9 *cover +-  18.0)
(18.0*1.74  ) + (11.9 *66 ) 
# 816.72

# height slope pulchra = 1
# cover slope pulchra = 24
# PULCHRA FINAL EQUATION:  Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)
(1.1*1 ) + (18.1 *24 )
# 435.5

# average biomass for s rich + s pul
(816.72 +435.5)/2
# 626.11 g/m2

# 2100 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
  rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")  %>%
  mutate(year = rep(2020))

shrub_map_project_novel <- shrub_map_2020 %>%
  mutate(biomass_per_m2_new = biomass_per_m2 + (626.11*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2100))

hist(shrub_map_project_novel$biomass_per_m2_new)

shrub_map_2020 <- shrub_map_2020 %>%
  rename("biomass_per_m2_new" = "biomass_per_m2")

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
#   0%       25%       50%       75%      100% 
#  0.0000   174.4421 26107.4000 50263.2420 52214.8000 

# setting biomass level thresholds using quantiles
threshold_novel <- shrub_novel %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 174.4421    & biomass_per_m2_new < 50263.2420 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 50263.2420 ~ 'High')) # 75%

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
  mutate(biomass_level = case_when (biomass_per_m2_new < 50263.2420     ~ 'Low', # 75% quant.
                                    biomass_per_m2_new > 50263.2420 ~ 'High')) # 75%

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

