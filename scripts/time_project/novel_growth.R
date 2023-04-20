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

# height slope richardsonii = 1.74 cm/year +- 1.071
# cover slope richardsonii = 66% +- 8%
# RICHARDSONII FINAL EQUATION: Biomass =  (18.0*height +- 5.1) + (11.9 *cover +-  18.0)
(18.0*1.74  ) + (11.9 *66 ) 
#  816.72

# if cover is 100% and max heights
(18.0*1.74  ) + (11.9 *100) 
# 1221.32

# height slope pulchra = 1 +-  1.05 cm/year
# cover slope pulchra = 24% +- 7%
# PULCHRA FINAL EQUATION:  Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)
(1.1*1 ) + (18.1 *24 )
#  435.5

# if cover is 100%
(1.1*129 ) + (18.1 *100 )
# 1951.9

# average biomass for s rich + s pul
(435.5 +816.72)/2
# 626.11 g/m2
#0.00062611 g/km2

# with cover 100%, average biomass for s rich + s pul
(1221.32 +1811.1)/2
# 1516.21 g/m2


# 2030 projection
shrub_map_2020 <- shrub_map_extract_highest %>%
dplyr::rename("biomass_per_m2" = "pft_agb_deciduousshrub_p50_2020_wgs84")%>%
mutate(year = rep(2020)) 

shrub_map_project_novel_2050 <- shrub_map_2020 %>%
  dplyr::mutate(biomass_per_m2_new = biomass_per_m2 + (435.5*30))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2050))

shrub_map_project_novel_2030 <- shrub_map_2020 %>%
  dplyr::mutate(biomass_per_m2_new = biomass_per_m2 + (435.5*10))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2030))

shrub_map_project_novel_2100 <- shrub_map_2020 %>%
  dplyr::mutate(biomass_per_m2_new = biomass_per_m2 + (435.5*80))%>%
  dplyr::select(-biomass_per_m2)%>%
  mutate(year = rep(2100))

mean_2030_novel <- c(shrub_map_project_novel_2030$biomass_per_m2_new)
mean(mean_2030_novel)# 4583.272 g/m2

mean_2050_novel <- c(shrub_map_project_novel_2050$biomass_per_m2_new)
mean(mean_2050_novel)# 13293.27 g/m2

hist(shrub_map_project_novel$biomass_per_m2_new)

shrub_map_2020 <- shrub_map_2020 %>%
  dplyr::rename("biomass_per_m2_new" = "biomass_per_m2")

# % diff
(6489.372-228.2723)/228.2723
# 2742.821%
# times larger
(6489.372/228.2723)
#  28.42821 times larger 

# bind data so that I can facet plot
shrub_novel <- rbind(shrub_map_2020, shrub_map_project_novel)
shrub_novel_all <- rbind(shrub_map_2020, shrub_map_project_novel_2030,shrub_map_project_novel_2050, shrub_map_project_novel_2100)

(raster_test_novel <- ggplot(shrub_novel_all) + 
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
quantiles_novel_all <- quantile(shrub_novel_all$biomass_per_m2_new)

quantiles_novel
quantiles_novel_all
#    0%        25%        50%        75%       100% 
#  0.0000  174.4421 2628.2750 3304.9920 5256.5500 
#  0.000  5227.325 13585.200 28204.175 52214.800 
# .00  3797.75  9773.00 20103.25 36966.00 

# setting biomass level thresholds using quantiles
threshold_novel <- shrub_novel %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 174.4421    & biomass_per_m2_new < 18957.7420 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 18957.7420 ~ 'High')) # 75%

threshold_novel_all <- shrub_novel_all %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 3797.75     ~ 'Low', # 25% quant.
                                    biomass_per_m2_new> 3797.75    & biomass_per_m2_new < 20103.25 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2_new > 20103.25 ~ 'High')) # 75%

# ordering factor levels
threshold_novel$biomass_level <- factor(threshold_novel$biomass_level,levels=c("Low", "Medium", "High"),
                                      labels = c("Low", "Medium", "High"),
                                      ordered = T)

threshold_novel_all$biomass_level <- factor(threshold_novel_all$biomass_level,levels=c("Low", "Medium", "High"),
                                        labels = c("Low", "Medium", "High"),
                                        ordered = T)
(threshold_novel_levels <- ggplot(threshold_novel_all) + 
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
  mutate(biomass_level = case_when (biomass_per_m2_new < 18957.7420     ~ 'Low', # 
                                    biomass_per_m2_new > 18957.7420 ~ 'High')) #

threshold_novel_bi_all <- shrub_novel_all %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 1951.9     ~ 'Low', # 
                                    biomass_per_m2_new > 1951.9 ~ 'High')) #

# ordering factor levels
threshold_novel_bi$biomass_level <- factor(threshold_novel_bi$biomass_level,levels=c("Low", "High"),
                                        labels = c("Low", "High"),
                                        ordered = T)
threshold_novel_bi_all$biomass_level <- factor(threshold_novel_bi_all$biomass_level,levels=c("Low", "High"),
                                           labels = c("Low", "High"),
                                           ordered = T)

(treshold_novel_bi <- ggplot(threshold_novel_bi_all) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
   theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

