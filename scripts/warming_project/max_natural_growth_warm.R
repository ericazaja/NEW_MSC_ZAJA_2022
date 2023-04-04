# WARMING PROJECTIONS: Max natural growth with warming scenario

# data -----
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/coord.chelsa.combo.c.delta.2100.solo")

# MAX HEIGHT and COVER SLOPES:
# # Height slope  for S pulchra for full time period = ??? * 23 years
# Cover slope for S pulchra for full time period =  -0.02 * 33 years = 0


# Salpul allom equation = 
# Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)

# TEMP SLOPES:
# QHI = 0.06
# ANWR = -0.00
# TOOLIK = -0.00
# mean = 0.02 * 20 years = 0.4 degC over full time period

# biomass/temp over full time = /0.02 =  g/degC

# multiply by biomass increase
max_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (5.5*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(max_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #256.0573 g/m2
range(max_warm$biomass_per_m2_2100_solo) #  22.53906 2157.11943

# bind 2020 and 2100
max_warm_bind <- max_warm %>%
  rename(biomass_per_m2 = biomass_per_m2_2100_solo)

max_warm_bind$year <- as.factor(max_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)
max_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, max_warm_bind)

# plotting facet biomass (yellow-green)
(max_test_temp <- ggplot(max_warm_to_plot) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white")+
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# TRESHOLD MAPS-----
quant_max_warm <- quantile(max_warm_to_plot$biomass_per_m2)
quant_max_warm
#   0%       25%       50%       75%      100% 
# 0.0000  106.4514  188.7321  321.7873 2157.1194 


# setting biomass level thresholds using quantiles
threshold_max_warm <- max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 106.4514     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 106.4514    & biomass_per_m2 < 321.7873 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 321.7873 ~ 'High')) # 75%

# ordering factor levels
threshold_max_warm$biomass_level <- factor(threshold_max_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = T)

(threshold_max_warm_levels <- ggplot(threshold_max_warm) + 
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
  mutate(biomass_level = case_when (biomass_per_m2 < 321.7873     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 321.7873 ~ 'High')) # 75%

# ordering factor levels
threshold_max_warm_bi$biomass_level <- factor(threshold_max_warm_bi$biomass_level,levels=c("Low", "High"),
                                              labels = c("Low", "High"),
                                              ordered = T)

(treshold_max_bi <- ggplot(threshold_max_warm_bi) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


