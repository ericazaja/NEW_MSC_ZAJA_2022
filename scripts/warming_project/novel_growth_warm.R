# WARMING PROJECTIONS: Novel growth with warming scenario

# data
coord.chelsa.combo.c.delta.2100.solo <- read.csv("data/coord.chelsa.combo.c.delta.2100.solo")
coord.chelsa.combo.c.delta.2030 <- read.csv("data/coord.chelsa.combo.c.delta.2030.csv")
coord.chelsa.combo.c.delta.2050 <- read.csv("data/coord.chelsa.combo.c.delta.2050.csv")
coord.chelsa.combo.c.biom.2020 <- read.csv("data/coord.chelsa.combo.c.biom.2020.csv")
# Get biomass over full time period. Divide biomass by 6 deg warming (difference between KP and CG temps). Get biomass per degree of warming
# multiply this by the 5 degrees projected warming.
# NB we basically gave the KP shrubs in the CG the warming that they will experience naturally in the future.

# height slope richardsonii = 1.271249 --> 
# height over full 9 year time period = 1.271249*23 = 29.23873
# cover slope richardsonii = 66% -->
# cover over 9 years = 66*23 = 1518%
# RICHARDSONII FINAL EQUATION: Biomass =  (18.0*29.23873 +- 5.1) + (11.9 *1518 +-  18.0)
(18.0*29.23873 ) + (11.9 *1518 )
#  18590.5 g/m2

# times pulchra rates by 23 to keep years consistent.
# log height slope pulchra = 0.003288811 --> 0.003288811*23=exp(0.07564265)= 1.078577
# cover slope pulchra = 0.24 --> 24% * 23 =552
# PULCHRA FINAL EQUATION:  Biomass =  (1.1*23 +-  5.0 ) + (18.1 *552 +-  8.2)
(1.1*1.078577  ) + (18.1 *552)
# 9992.386

# Mean
(18590.5+10016.5)/2
#14303.5

# Biomass over 23 year period divided by the 6.4 difference in temp between KP and CG
9992.386/6.4
#  1561.31 g/m2/degC


# multiply by biomass increase
novel_warm_2100 <- coord.chelsa.combo.c.delta.2100.solo %>%
  filter(year == 2100) %>% 
  mutate(biomass_per_m2_2100_solo = biomass_per_m2 + (1565.078*delta.7.solo)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2100 <- c(novel_warm_2100$biomass_per_m2_2100_solo)
mean(c_mean_2100)  #8134.769
range(novel_warm_2100$biomass_per_m2_2100_solo) # 6413.707 11624.328

# 2030, multiply by biomass increase
novel_warm_2030 <- coord.chelsa.combo.c.delta.2030 %>%
  filter(year == 2030) %>% 
  mutate(biomass_per_m2_2030 = biomass_per_m2 + (1565.078*delta)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2030 <- c(novel_warm_2030$biomass_per_m2_2030)
mean(c_mean_2030) # 996.9929 g/m2
range(novel_warm_2030$biomass_per_m2_2030) # -247.4458 4658.8445

# 2050
novel_warm_2050 <- coord.chelsa.combo.c.delta.2050 %>%
  filter(year == 2050) %>% 
  mutate(biomass_per_m2_2050 = biomass_per_m2 + (1565.078*delta.2)) %>%
  dplyr::select(-biomass_per_m2)

c_mean_2050 <- c(novel_warm_2050$biomass_per_m2_2050)
mean(c_mean_2050) #2266.216 g/m2
range(novel_warm_2050$biomass_per_m2_2050) #  313.9614 6095.8091

c_mean_2100_temp_solo <- c(novel_warm$mean_temp_C)
mean(c_mean_2100_temp_solo) # 20.17315 C

# %diff
(12907.15-228.2723)/228.2723
#55.54278
# 5554.278

# times larger
2266.216/228.2723
# 56.54278

# bind 2020 and 2100
novel_warm_bind <- novel_warm_2100 %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2100_solo, 
                delta = delta.7.solo)

novel_warm_bind_2030 <- novel_warm_2030 %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2030)

novel_warm_bind_2050 <- novel_warm_2050 %>%
  dplyr::rename(biomass_per_m2 = biomass_per_m2_2050, 
                delta = delta.2)


novel_warm_bind$year <- as.factor(novel_warm_bind$year)
novel_warm_bind_2030$year <- as.factor(novel_warm_bind_2030$year)
novel_warm_bind_2050$year <- as.factor(novel_warm_bind_2050$year)

coord.chelsa.combo.c.biom.2020$year <- as.factor(coord.chelsa.combo.c.biom.2020$year)

novel_warm_to_plot <- rbind(coord.chelsa.combo.c.biom.2020, novel_warm_bind)
novel_warm_to_plot_2030 <- rbind(coord.chelsa.combo.c.biom.2020, novel_warm_bind_2030)
novel_warm_to_plot_2050 <- rbind(coord.chelsa.combo.c.biom.2020, novel_warm_bind_2030, novel_warm_bind_2050)
novel_warm_to_plot_all <- rbind(coord.chelsa.combo.c.biom.2020, novel_warm_bind_2030, novel_warm_bind_2050, novel_warm_bind)

# plotting facet biomass (yellow-green)
(raster_test_temp <- ggplot(novel_warm_to_plot_2050) + 
    geom_tile(aes(x=x,y=y,fill=(biomass_per_m2))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Shrub biomass g/m2",high = "#013220", low = "lightyellow1",  na.value="white", 
                        breaks = seq(0, 4600, 800))+
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=13, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=13, colour = "black")) + 
    theme(strip.text.x = element_text(size = 14, face = "bold.italic")) +
    xlab("\nLongitude") +
    ylab("Latitude\n")+ theme(text=element_text(family="Helvetica Light")) )

ggsave(raster_test_temp, filename ="output/final_maps/novel_test_temp.png", width = 11.7, height = 8.3, units = "in")

# TRESHOLD MAPS-----
quant_novel_warm_2100 <- quantile(novel_warm_to_plot$biomass_per_m2)
quant_novel_warm_2030 <- quantile(novel_warm_to_plot_2030$biomass_per_m2)
quant_novel_warm_2050 <- quantile(novel_warm_to_plot_2050$biomass_per_m2)
quant_novel_warm_all <- quantile(novel_warm_to_plot_all$biomass_per_m2)

quant_novel_warm
quant_novel_warm_2030
quant_novel_warm_2050
quant_novel_warm_all
#   0%        25%        50%        75%       100% 
# 0.0000   174.4421  6205.5164 12930.1861 18640.7937 
# -247.4458  166.2585  536.7945 1425.6429 4658.8445 
# 0.0000  174.4421  998.2743 3535.3238 6095.8091 
# -154.1138  276.7149  964.7357 2010.3582 4586.0554 
# -154.1138   466.2626  1579.7166  5042.9683 11624.3278 

# setting biomass level thresholds using quantiles
threshold_novel_warm <- novel_warm_to_plot %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 174.4421     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 174.4421    & biomass_per_m2 < 12930.1861 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 12930.1861 ~ 'High')) # 75%

threshold_novel_warm_2030 <- novel_warm_to_plot_2030 %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 166.2585     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 166.2585    & biomass_per_m2 < 1425.6429 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 1425.6429 ~ 'High')) # 75%

threshold_novel_warm_2050 <- novel_warm_to_plot_2050 %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 276.7149     ~ 'Open', # 25% quant.
                                    biomass_per_m2> 276.7149    & biomass_per_m2 < 2010.3582 ~ 'Dense', # between 25 and 75 
                                    biomass_per_m2 > 2010.3582 ~ 'Very dense')) # 75%

threshold_novel_warm_all <- novel_warm_to_plot_all %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 466.2626     ~ 'Low', # 25% quant.
                                    biomass_per_m2> 466.2626    & biomass_per_m2 < 5042.9683 ~ 'Medium', # between 25 and 75 
                                    biomass_per_m2 > 5042.9683 ~ 'High')) # 75%

# ordering factor levels
threshold_novel_warm$biomass_level <- factor(threshold_novel_warm$biomass_level,levels=c("Low", "Medium", "High"),
                                        labels = c("Low", "Medium", "High"),
                                        ordered = T)

threshold_novel_warm_2030$biomass_level <- factor(threshold_novel_warm_2030$biomass_level,levels=c("Low", "Medium", "High"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = T)

threshold_novel_warm_2050$biomass_level <- factor(threshold_novel_warm_2050$biomass_level,levels=c("Open", "Dense", "Very dense"),
                                                  labels = c("Open", "Dense", "Very dense"),
                                                  ordered = T)

threshold_novel_warm_all$biomass_level <- factor(threshold_novel_warm_all$biomass_level,levels=c("Open", "Dense", "Very dense"),
                                                  labels = c("Low", "Medium", "High"),
                                                  ordered = T)

threshold_novel_warm_2050_summary <- threshold_novel_warm_2050 %>% 
  filter(year == "2050")%>% 
  group_by(biomass_level) %>% 
  summarise(percent_biomass_level = n())
# finding the ratio of 100% using method here: https://www.mathswithmum.com/calculate-ratio-3-numbers/ 

(threshold_novel_warm_levels <- ggplot(threshold_novel_warm_2050) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) + 
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass density", values=c( "#F0E442", "#E69F00", "#009E73")) +
    coord_quickmap()+
   theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=13, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=13, colour = "black")) + 
    theme(strip.text.x = element_text(size = 14, face = "bold.italic")) +
    xlab("\nLongitude") +
    ylab("Latitude\n") + theme(text=element_text(family="Helvetica Light")) )

ggsave(threshold_novel_warm_levels, filename ="output/final_maps/threshold_novel_warm_levels.pdf", width = 11.7, height = 8.3, units = "in")



# binary threshold map
threshold_novel_warm_bi <- shrub_novel %>%
  mutate(biomass_level = case_when (biomass_per_m2_new < 50263.2420     ~ 'Low', # 75% quant.
                                    biomass_per_m2_new > 50263.2420 ~ 'High')) # 75%

threshold_novel_warm_bi_2030 <- novel_warm_to_plot_2030 %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 1425.6429     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 1425.6429 ~ 'High')) # 75%

threshold_novel_warm_bi_2050 <- novel_warm_to_plot_2050 %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 1951.9     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 1951.9 ~ 'High')) # 75%

threshold_novel_warm_bi_all <- novel_warm_to_plot_all %>%
  mutate(biomass_level = case_when (biomass_per_m2 < 1951.9     ~ 'Low', # 75% quant.
                                    biomass_per_m2 > 1951.9 ~ 'High')) # 75%

# ordering factor levels
threshold_novel_bi$biomass_level <- factor(threshold_novel_bi$biomass_level,levels=c("Low", "High"),
                                           labels = c("Low", "High"),
                                           ordered = T)
threshold_novel_warm_bi_2030$biomass_level <- factor(threshold_novel_warm_bi_2030$biomass_level,levels=c("Low", "High"),
                                           labels = c("Low", "High"),
                                           ordered = T)

threshold_novel_warm_bi_2050$biomass_level <- factor(threshold_novel_warm_bi_2050$biomass_level,levels=c("Low", "High"),
                                                     labels = c("Low", "High"),
                                                     ordered = T)

threshold_novel_warm_bi_all$biomass_level <- factor(threshold_novel_warm_bi_all$biomass_level,levels=c("Low", "High"),
                                                     labels = c("Low", "High"),
                                                     ordered = T)


(treshold_novel_bi <- ggplot(threshold_novel_warm_bi_all) + 
    geom_tile(aes(x=x,y=y,fill=biomass_level)) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#009E73")) +
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))


# plotting facet biomass (yellow-green)
(raster_temps <- ggplot(novel_warm_to_plot_all) + 
    geom_tile(aes(x=x,y=y,fill=(mean_temp_C))) + 
    facet_wrap(~year, nrow = 1) +
    #scale_fill_manual(name = "Biomass level", values=c( "#F0E442", "#E69F00", "#009E73")) +
    scale_fill_gradient(name = "Temperature g/m2",high = "red", low = "blue4",  na.value="white")+
    coord_quickmap()+
    theme_shrub() +  
    theme(axis.text.x  = element_text(vjust=0.5, size=10, colour = "black", angle = 45)) +
    theme(axis.text.y  = element_text(vjust=0.5, size=10, colour = "black")) + 
    xlab("\nLongitude") +
    ylab("Latitude\n"))
