# WARMING PROJECTIONS: Novel growth with warming scenario

# data
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/coord.chelsa.combo.c.delta.2100.solo")

# Get biomass over full time period. Divide biomass by 6 deg warming (difference between KP and CG temps). Get biomass per degree of warming
# multiply this by the 5 degrees projected warming.
# NB we basically gave the KP shrubs in the CG the warming that they will experience naturally in the future.

# height slope richardsonii = 1.74 --> 
# height over full 9 year time period = 1.74*9 = 15.66
# cover slope richardsonii = 0.66 -->
# cover over 9 years = 0.66*9 = 5.94
# RICHARDSONII FINAL EQUATION: Biomass =  (18.0*15.66 +- 5.1) + (11.9 *5.94 +-  18.0)
(18.0*15.66 ) + (11.9 *5.94 )
#  352.566 g/m2

# height slope pulchra = 1 --> 1*9= 9
# cover slope pulchra = 0.24 --> 0.24*9 =2.16
# PULCHRA FINAL EQUATION:  Biomass =  (1.1*9 +-  5.0 ) + (18.1 *2.16 +-  8.2)
(1.1*9  ) + (18.1 *2.16 )# 48.996

# Mean
(352.566+48.996)/2
#200.781

# Biomass over 9 year period divided by the 6.4 diference in temp beterrn KP and CG
200.781/6.4
# 31.37203 g/m2/degC

# multiply by biomass increase
novel_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (31.37203*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(novel_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #386.7582 g/m2
range(novel_warm$biomass_per_m2_2100_solo) # 128.5629 2303.5054

c_mean_2100_temp_solo <- c(novel_warm$mean_temp_C)
mean(c_mean_2100_temp_solo) # 20.17315 C

# bind 2020 and 2100
novel_warm_bind <- novel_warm %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo)

novel_warm_bind$year <- as.factor(novel_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)

novel_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, novel_warm_bind)

# plotting facet biomass (yellow-green)
(raster_test_temp <- ggplot(novel_warm_to_plot) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white")+
    coord_quickmap()+
    #theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# TRESHOLD MAPS-----
quant_novel_warm <- quantile(novel_warm_to_plot$biomass_per_m2)
quant_novel_warm

# 0%       25%       50%       75%      100% 
# 0.0000  164.5013  268.6543  401.6677 2303.5054 

# setting biomass level thresholds using quantiles
threshold_novel_warm <- novel_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 164.5013     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 164.5013    & biomass_per_m2 < 401.6677 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 401.6677 ~ 'High')) # 75%

# ordering factor levels
threshold_novel_warm$biomass_level <- factor(threshold_novel_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                        labels = c("Low", "Medium", "High"),
                                        ordered = T)

(threshold_novel_warm_levels <- ggplot(threshold_novel_warm) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
   # theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


# binary threshold map
threshold_novel_warm_bi <- shrub_novel %>%
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
