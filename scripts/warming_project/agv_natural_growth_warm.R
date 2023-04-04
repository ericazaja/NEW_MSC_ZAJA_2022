# WARMING PROJECTIONS: Average natural growth with warming scenario

# STEP 1. Height with warming: 
# Height over time S. pulchra on QHI: 0.1 cm per year. = 2.4 cm height over 24 years (1999-2022, 24 years) . 
# STEP 2. Cover over time. Multiply by full time period.
# STEP 3. BIOMASS: Put the height over the full time period and the cover over full time period in the allometric equations, derive biomass. 

# STEP 4 Get temp over time slope: times the slope per year times 24 years => 1.2 degrees C over 24 years. 
# Divide the biomass over the full time period by the temp over the full time period 1.2 = to get 2.0 g/m2 per degree
# THEN Multiply the 2.0 g/m2 per degree x 5 degrees warming (future proj. Difference between temp in 2020-2100)  = 10cm

# data -----
shrub_map_extract_highest <- read.csv("data/extract_end_highest.csv") # high res map
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/coord.chelsa.combo.c.delta.2100.solo")

# HEIGHT and COVER SLOPES:
# # Height slope  for S pulchra for full time period = ??? * 23 years
# Cover slope for S pulchra for full time period = 0 * 33 years = 0

# Salpul allom equation = 
# Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)
# add + high error ? 
# 0.11 g/m2/degC

# TEMP SLOPES:
# QHI = 0.06
# ANWR = -0.00
# TOOLIK = -0.00
# mean = 0.02 * 20 years = 0.4 degC over full time period

# biomass/temp over full time = 0.11/0.02 =  5.5 g/degC

# multiply by biomass increase
avg_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (5.5*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(avg_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #256.0573 g/m2
range(avg_warm$biomass_per_m2_2100_solo) #  22.53906 2157.11943

c_mean_2100_temp_solo <- c(avg_warm$mean_temp_C)
mean(c_mean_2100_temp_solo) # 20.17315 C

# bind 2020 and 2100
avg_warm_bind <- avg_warm %>%
  rename(biomass_per_m2 = biomass_per_m2_2100_solo)

avg_warm_bind$year <- as.factor(avg_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)
avg_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind)

# plotting facet biomass (yellow-green)
(avg_test_temp <- ggplot(avg_warm_to_plot) + 
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
quant_avg_warm <- quantile(avg_warm_to_plot$biomass_per_m2)
quant_avg_warm
#   0%       25%       50%       75%      100% 
# 0.0000  106.4514  188.7321  321.7873 2157.1194 


# setting biomass level thresholds using quantiles
threshold_avg_warm <- avg_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 106.4514     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 106.4514    & biomass_per_m2 < 321.7873 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 321.7873 ~ 'High')) # 75%

# ordering factor levels
threshold_novel_warm$biomass_level <- factor(threshold_novel_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = T)

(threshold_novel_warm_levels <- ggplot(threshold_novel_warm) + 
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
threshold_avg_warm_bi <- avg_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 321.7873     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 321.7873 ~ 'High')) # 75%

# ordering factor levels
threshold_avg_warm_bi$biomass_level <- factor(threshold_avg_warm_bi$biomass_level,levels=c("Low", "High"),
                                           labels = c("Low", "High"),
                                           ordered = T)

(treshold_avg_bi <- ggplot(threshold_avg_warm_bi) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

