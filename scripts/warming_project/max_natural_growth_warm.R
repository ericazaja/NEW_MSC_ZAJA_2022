# WARMING PROJECTIONS: Max natural growth with warming scenario

# data -----
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/coord.chelsa.combo.c.delta.2100.solo")
coord.chelsa.combo.c.delta.2020 <- read.csv("data/coord.chelsa.combo.c.biom.2020.csv")

# MAX HEIGHT and COVER SLOPES:
# # Height slope  for S pulchra for full time period = 0.70 * 23 years = 16.1
# Cover slope for S pulchra for full time period =   0.011 * 33 years =  0.363


# Salpul allom equation = 
# Biomass =  (1.1*16.1 +-  5.0 ) + (18.1 *0.363 +-  8.2) = 24.2803

# TEMP SLOPES:
# mean = 0.2 * 20 years = 4 degC over full time period

# biomass/temp over full time = 24.2803/4 =  6.070075 g/degC

# multiply by biomass increase
max_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (6.070075*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(max_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #258.9373 g/m2
range(max_warm$biomass_per_m2_2100_solo) #  22.53906 2157.11943

# bind 2020 and 2100
max_warm_bind <- max_warm %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo)
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
#0%       25%       50%       75%      100% 
#0.0000  107.7868  190.2687  323.3229 2160.3450 

# setting biomass level thresholds using quantiles
threshold_max_warm <- max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 107.7868     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 107.7868    & biomass_per_m2 < 323.3229 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 323.3229 ~ 'High')) # 75%

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


