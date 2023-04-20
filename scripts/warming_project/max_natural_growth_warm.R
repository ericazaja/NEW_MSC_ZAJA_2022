# WARMING PROJECTIONS: Max natural growth with warming scenario

# data -----
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/coord.chelsa.combo.c.delta.2100.solo")
coord.chelsa.combo.c.delta.2020 <- read.csv("data/coord.chelsa.combo.c.biom.2020.csv")

# MAX HEIGHT and COVER SLOPES:
# # Height slope  for S pulchra for full time period = 0.70 +- 0.09 * 23 years = 16.1
# Cover slope for S pulchra for full time period =   1.14 +-0.74 * 23 years =  26.22

# no error
# Salpul allom equation = 
# Biomass =  (1.1*16.1 ) + (18.1 *26.22 ) 
(1.1*16.1 ) + (18.1 *26.22 )
# 492.292

# if cover is 100
(1.1*129) + (18.1 *100)
# 1951.9

# TEMP SLOPES:
# mean = 3.350266 over full time period

# biomass/temp over full time = 492.292/3.350266 = 146.9412/degC


# multiply by biomass increase
max_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (146.9412*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(max_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #970.5932 g/m2
range(max_warm$biomass_per_m2_2100_solo) #  602.1667 2957.4048 

# %diff
(970.5932-228.2723)/228.2723
# 3.25191

# times larger
970.5932/228.2723
# 4.25191

# bind 2020 and 2100
max_warm_bind <- max_warm %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo)%>%
  mutate(year= rep("2100_max"))

max_warm_bind$year <- as.factor(max_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)
max_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, max_warm_bind)
mean_max_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind, max_warm_bind, novel_warm_bind)

# plotting facet biomass (yellow-green)
(max_test_temp <- ggplot(mean_max_warm_to_plot) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "yellow1",  na.value="white")+
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# TRESHOLD MAPS-----
quant_max_warm <- quantile(max_warm_to_plot$biomass_per_m2)
quant_maxmean_warm <- quantile(mean_max_warm_to_plot$biomass_per_m2)
quant_max_warm
quant_maxmean_warm

#0%       25%       50%       75%      100% 
# 0.0000  174.4421  957.9852 1238.5743 3305.8807 
# 0.0000  307.5213  631.0684  878.5136 2957.4048 

# setting biomass level thresholds using quantiles
threshold_max_warm <- max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 174.4421    & biomass_per_m2 < 1238.5743 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 1238.5743 ~ 'High')) # 75%

threshold_maxmean_warm <- mean_max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 307.5213     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 307.5213    & biomass_per_m2 < 878.5136 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 878.5136 ~ 'High')) # 75%

# ordering factor levels
threshold_max_warm$biomass_level <- factor(threshold_max_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = T)

threshold_maxmean_warm$biomass_level <- factor(threshold_maxmean_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                           labels = c("Low", "Medium", "High"),
                                           ordered = T)

(threshold_max_warm_levels <- ggplot(threshold_maxmean_warm) + 
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
threshold_max_warm_bi <- max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 1827.71     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 1827.71 ~ 'High')) # 75%

# binary threshold map
threshold_maxmean_warm_bi <- mean_max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 1951.9     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 1951.9 ~ 'High')) # 75%

# ordering factor levels
threshold_max_warm_bi$biomass_level <- factor(threshold_max_warm_bi$biomass_level,levels=c("Low", "High"),
                                              labels = c("Low", "High"),
                                              ordered = T)

# ordering factor levels
threshold_maxmean_warm_bi$biomass_level <- factor(threshold_maxmean_warm_bi$biomass_level,levels=c("Low", "High"),
                                              labels = c("Low", "High"),
                                              ordered = T)

(treshold_maxmean_bi <- ggplot(threshold_maxmean_warm_bi) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


