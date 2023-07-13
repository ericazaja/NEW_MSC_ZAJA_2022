# WARMING PROJECTIONS: Max natural growth with warming scenario

# data -----
shrub_map_extract_highest <- read.csv("data/maps_data/extract_end_highest.csv") # high res map
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/climate_data/coord.chelsa.combo.c.delta.2100.solo")
coord.chelsa.combo.c.delta.2020 <- read.csv("data/climate_data/coord.chelsa.combo.c.biom.2020.csv")

# MAX HEIGHT and COVER SLOPES:
# # Height slope  for S pulchra for full time period = 0.70 +- 0.09 * 23 years = 16.1
# Cover slope for S pulchra for full time period =   0.8 +-0.03 * 23 years =  18.4

# no error
# Salpul allom equation = 
# Biomass =  (1.1*16.1 ) + (18.1 *18.4 ) 
(1.1*16.1 ) + (18.1 *18.4 )
# 350.75

# if cover is 100
(1.1*129) + (18.1 *100)
# 1951.9

# TEMP SLOPES:
# mean = 3.350266 over full time period

# biomass/temp over full time = 350.75/3.350266 = 104.6932/degC


# multiply by biomass increase
max_warm <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (29.21128*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_max_2100_solo <- c(max_warm$biomass_per_m2_2100_solo)
mean(c_max_2100_solo) #375.8425 g/m2
sd(c_max_2100_solo)#201.2416
201.2416/1007.543
# 0.199735
range(max_warm$biomass_per_m2_2100_solo) #  119.7082 2291.2797

# %diff
(375.8425-228.2723)/228.2723
# 0.6464656

# times larger
375.8425/228.2723
# 1.646466

# bind 2020 and 2100
max_warm_bind <- max_warm %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo)%>%
  mutate(year= rep("2100_max"))

max_warm_bind$year <- as.factor(max_warm_bind$year)
coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)
max_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, max_warm_bind)
mean_max_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind, max_warm_bind)
all_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind, max_warm_bind, novel_warm_bind)

write.csv(all_warm_to_plot, file = "data/all_warm_to_plot.csv")
# run this code if I want them all on same scale
#novel_warm_to_plot_2100 <- novel_warm_to_plot %>%
 # filter(year == 2100) %>%
 # mutate(year= rep("2100_novel"))
#novel_warm_bind_2030_2 <- novel_warm_bind_2030 %>%
#  mutate(year= rep("2030_novel"))
#novel_warm_bind_2050_2 <- novel_warm_bind_2050 %>%
 # mutate(year= rep("2050_novel"))

#mean_max_novel_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, avg_warm_bind, max_warm_bind, novel_warm_bind_2030_2, novel_warm_bind_2050_2)

# plotting facet biomass (yellow-green)
(max_test_temp <- ggplot(all_warm_to_plot) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white", 
                        breaks = seq(0, 2500, 400))+
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=13, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=13, colour = "black")) + 
    theme(strip.text.x = element_text(size = 14, face = "bold.italic")) +
    xlab("\nLongitude") +
    ylab("Latitude\n") + theme(text=element_text(family="Helvetica Light")) )


ggsave(max_test_temp, filename ="output/final_maps/all_scenarios_temp.png", width = 11.7, height = 8.3, units = "in")
# ggsave(max_test_temp, filename ="output/final_maps/max_test_temp_2.png", width = 11.7, height = 8.3, units = "in", dpi = 300)


# TRESHOLD MAPS-----
quant_max_warm <- quantile(max_warm_to_plot$biomass_per_m2)
quant_maxmean_warm <- quantile(mean_max_warm_to_plot$biomass_per_m2)
quant_all_warm <- quantile(all_warm_to_plot$biomass_per_m2)
quant_max_warm
quant_maxmean_warm
quant_all_warm

#0%       25%       50%       75%      100% 
# 0.0000  174.4421  957.9852 1238.5743 3305.8807 
#    0.0000  157.4943  251.7672  385.4468 2291.2797
# 0.0000  185.9591  322.8674  477.7382 2477.4851 


# setting biomass level thresholds using quantiles
threshold_max_warm <- max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 157.4943     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 157.4943    & biomass_per_m2 < 385.4468 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 385.4468 ~ 'High')) # 75%

threshold_maxmean_warm <- mean_max_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 185.9591     ~ 'Open', # 25% quant.
                                    biomass_per_m2> 185.9591    & biomass_per_m2 < 385.4468 ~ 'Dense', # between 25 and 75 
                                    biomass_per_m2 > 385.4468 ~ 'Very dense')) # 75%

threshold_all_warm <- all_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 185.9591     ~ 'Open', # 25% quant.
                                    biomass_per_m2> 185.9591    & biomass_per_m2 < 477.7382 ~ 'Dense', # between 25 and 75 
                                    biomass_per_m2 > 477.7382 ~ 'Very dense')) # 75%

# ordering factor levels
threshold_max_warm$biomass_level <- factor(threshold_max_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = T)

threshold_maxmean_warm$biomass_level <- factor(threshold_maxmean_warm$biomass_level,levels=c("Open", "Dense", "Very dense"),
                                           labels = c("Open", "Dense", "Very dense"),
                                           ordered = T)

threshold_all_warm$biomass_level <- factor(threshold_all_warm$biomass_level,levels=c("Open", "Dense", "Very dense"),
                                               labels = c("Open", "Dense", "Very dense"),
                                               ordered = T)
summary <- threshold_all_warm %>% 
  #filter(year == "2100_max")%>% 
  group_by(biomass_level, year) %>% 
  summarise(percent_biomass_level = n()) %>% 
  mutate(total = rep(1015142))%>% 
  mutate(division = 100/total)%>% 
  mutate(percent = (percent_biomass_level*division))
# finding the ratio of 100% using method here: https://www.mathswithmum.com/calculate-ratio-3-numbers/ 

(threshold_max_warm_levels <- ggplot(threshold_all_warm) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=13, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=13, colour = "black")) + 
    theme(strip.text.x = element_text(size = 14, face = "bold.italic")) +
    xlab("\nLongitude") +
    ylab("Latitude\n")+ theme(text=element_text(family="Helvetica Light")) )

ggsave(threshold_max_warm_levels, filename ="output/final_maps/all_scenarios_thresh.png", width = 11.7, height = 8.3, units = "in")

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


