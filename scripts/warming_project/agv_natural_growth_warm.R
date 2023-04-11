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
coord.chelsa.combo.c.biom.2020 <- read.csv("data/coord.chelsa.combo.c.biom.2020.csv")

# HEIGHT and COVER SLOPES:
# # Height slope  for S pulchra for full time period =  0.34 * 23 years = 7.82
# Cover slope for S pulchra for full time period = 0.011 * 33 years = 0.363

# Salpul allom equation = 
# Biomass =  (1.1*7.82 +-  5.0 ) + (18.1 *0.363 +-  8.2)
# add + high error ? 
# 15.1723 g/m2/degC

# when cover is 1 (100%)...
# Biomass =  (1.1*7.82 +-  5.0 ) + (18.1 *1 +-  8.2)
# 26.702*80 = 2136.16

# TEMP SLOPES:
# QHI = 0.18858300 +0.0610727508 = 0.2496558
# ANWR = 0.12682429 -0.0006859617 =0.1261383
# TOOLIK = 0.12712801- 0.0003822406= 0.1267458
# mean: (0.2496558+0.1261383+0.1267458)/3 = 0.2
# mean = 0.2 * 20 years = 4 degC over full time period

# biomass/temp over full time = 15.1723/4 =  3.793075 g/degC
# 3.793075*80 years? = 303.446

# multiply by biomass increase
avg_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (303.446 *delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(avg_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #247.4343 g/m2
range(avg_warm$biomass_per_m2_2100_solo) #  210.5148 2416.6555

c_mean_2100_temp_solo <- c(avg_warm$mean_temp_C)
mean(c_mean_2100_temp_solo) # 20.17315 C

# bind 2020 and 2100
avg_warm_bind <- avg_warm %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo)
avg_warm_bind$year <- as.factor(avg_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)
avg_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind)
glimpse(avg_warm_to_plot)

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
#  0%       25%       50%       75%      100% 
# 0.0000  174.4421 1320.9807 1725.7870 3842.9211 


# setting biomass level thresholds using quantiles
threshold_avg_warm <- avg_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 174.4421    & biomass_per_m2 < 1725.7870 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 1725.7870 ~ 'High')) # 75%

# ordering factor levels
threshold_avg_warm$biomass_level <- factor(threshold_avg_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = T)

(threshold_avg_warm_levels <- ggplot(threshold_avg_warm) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))

# binary threshold map: using same tresholds as time maps?
threshold_avg_warm_bi <- avg_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 2136.16     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 2136.16 ~ 'High')) # 75%

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

