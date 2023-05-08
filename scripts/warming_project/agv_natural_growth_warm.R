# WARMING PROJECTIONS: Average natural growth with warming scenario

# NB this below is just the method, not accurate numbers. Calculatins after data
# STEP 1. Height with warming: 
# Height over time S. pulchra on QHI: 0.34 cm per year. = 8.16 cm height over 24 years (1999-2022, 24 years) . 
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
# # Height slope  for S pulchra for full time period =  0.34 * 23 years = 7.82 +- 0.04
# Cover slope for S pulchra for full time period =  0.65% * 23 years =14.95 % +-0.02

# no error: 
# Salpul allom equation = 
# Biomass =  (1.1*7.82) + (18.1 *14.95)
(1.1*7.82) + (18.1 *14.95)
#  279.197 g/m2


# when cover is 100 and max height avg pulchra 129cm
# Biomass =  (1.1*7.82) + (18.1 *100)
(1.1*129) + (18.1 *100)
# 1951.9

# TEMP SLOPES:
# QHI = 0.18858300 +0.0610727508 = 0.2496558+- 0.1062264
# ANWR = 0.12682429 -0.0006859617 =0.1261383 +- 0.08487509
# TOOLIK = 0.12712801- 0.0003822406= 0.1267458 +-0.07488695
# mean: (0.2496558+0.1261383+0.1267458)/3 = 0.1675133
# mean error:(0.1062264+0.08487509+0.07488695)/3= 0.08866281
# MEAN SLOPE = 0.1675133 +- 0.08866281
# mean = 0.1675133 * 20 years = 3.350266 +- 0.08866281 degC over full time period

# biomass/temp over full time = 279.197/3.350266 = 83.33577 g/m2/degC

# multiply by biomass increase
avg_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (83.33577 *delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100_solo <- c(avg_warm$biomass_per_m2_2100_solo)
mean(c_mean_2100_solo) #649.2699 g/m2
range(avg_warm$biomass_per_m2_2100_solo) # 351.6952 2611.5816

c_mean_2100_temp_solo <- c(avg_warm$mean_temp_C)
mean(c_mean_2100_temp_solo) # 20.17315 C

# %diff
(661.8245-228.2723)/228.2723
# 1.899276
#189%

# times larger
(661.8245/228.2723)
# 2.899276

# temperatre difference
20.17315-15.12132

# bind 2020 and 2100
avg_warm_bind <- avg_warm %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo) %>%
  mutate(year= rep("2100_avg"))

avg_warm_bind$year <- as.factor(avg_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)
avg_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind)
glimpse(avg_warm_to_plot)

mean_2020_temp <- c(coord.chelsa.combo.c.biom.2020$mean_temp_C)
mean(mean_2020_temp) # 15.12132

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
# 0%       25%       50%       75%      100% 
# 0.0000  174.4421  617.4858  805.4790 2816.3879 


# setting biomass level thresholds using quantiles
threshold_avg_warm <- avg_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 174.4421    & biomass_per_m2 < 805.4790 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 805.4790 ~ 'High')) # 75%

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
  mutate(biomass_level = case_when (biomass_per_m2 < 1818.602     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 1818.602 ~ 'High')) # 75%

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

