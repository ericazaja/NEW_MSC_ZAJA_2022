# CG stem elongation vs annual temperature
#### Script by Erica Zaja, created 30/01/23
### Last updated: 30/01/23

# Workflow:
# model annual stem elongation as a function of annual temperature
# INFORMATIVE analysis, not used in the allom. equations. 
# Just to see if we see more growth in warmer years

# 1. Libraries -------
library(tidyverse)
library(readr)
library(lme4)

# 2. Loading data -------
all_CG_growth <- read_csv("data/common_garden_shrub_data/all_CG_growth.csv")
CG_HOBO_monthly_means_season <- read_csv("data/hobo/CG_HOBO_monthly_means_season.csv")

# 3. Data wrangling -----

# keeping annual july temps only
CG_HOBO_july_annual <- CG_HOBO_monthly_means_season %>%
  filter(month == "7")

CG_HOBO_july_annual <- CG_HOBO_july_annual[,-1]

# adding 2022 tomst data to the hobo dataset
year <- "2022"
month <- 7
mean_soil_moist_month <- 40.04
mean_ground_temp_month <- 12.055 # (14.54 + 9.57)/2 mean of surface and top sensor 
mean_soil_temp_month <- 13.63
mean_air_temp_month <- "NA"

# making into vector of zeros
tomst_2022 <- data.frame(year, month, mean_soil_moist_month,
                         mean_ground_temp_month, mean_soil_temp_month,
                         mean_air_temp_month)

# merging into the HOBO dataset
CG_TOMST_HOBO_july <- rbind(CG_HOBO_july_annual, tomst_2022)

write.csv(CG_TOMST_HOBO_july, "data/hobo/CG_TOMST_HOBO_july.csv")

# adding temp to the common garden dataset
all_CG_growth_temps <- all_CG_growth %>%
  mutate(mean_ground_temp = case_when(Year == "2017" ~ 12.71875,
                                      Year == "2019" ~ 15.69672,
                                      Year == "2020" ~ 10.05658,
                                      Year == "2021" ~ 13.98771,
                                      Year == "2022" ~ 12.05500),
         mean_soil_temp = case_when(Year == "2017" ~ 12.07936,
                                      Year == "2019" ~ 14.57302,
                                      Year == "2020" ~ 13.40346,
                                      Year == "2021" ~ 14.54086,
                                      Year == "2022" ~ 13.63000),
         mean_soil_moist = case_when(Year == "2017" ~ 6.248524,
                                    Year == "2019" ~  8.079126,
                                    Year == "2020" ~ 6.237782,
                                    Year == "2021" ~ 3.306678,
                                    Year == "2022" ~ 40.04))

# 4. Modelling ------
# surface temperature  -----
elong_ground_temp_mod <- lmer(mean_stem_elong ~ mean_ground_temp + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_growth_temps)
tab_model(elong_ground_temp_mod)

# with interaction
elong_ground_temp_mod_interact <- lmer(mean_stem_elong ~ mean_ground_temp*Species  + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_growth_temps)
tab_model(elong_ground_temp_mod)

# soil temperature ------
elong_soil_temp_mod<- lmer(mean_stem_elong ~ mean_soil_temp  + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_growth_temps)
tab_model(elong_soil_temp_mod)

# with interaction
elong_soil_temp_mod_interact <- lmer(mean_stem_elong ~ mean_soil_temp*Species + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_growth_temps)
tab_model(elong_soil_temp_mod_interact)

# soil moist ----
elong_soil_moist_mod<- lmer(mean_stem_elong ~ mean_soil_moist  + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_growth_temps)
tab_model(elong_soil_moist_mod)

# with interaction
elong_soil_moist_mod_interact <- lmer(mean_stem_elong ~ mean_soil_moist*Species + (1|Year/Sample_age/SampleID_standard), data = all_CG_growth_temps)
tab_model(elong_soil_moist_mod_interact)

# soil temperature*soil moisture ------
elong_soil_temp_moist<- lmer(mean_stem_elong ~ mean_soil_temp*mean_soil_moist  + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_growth_temps)
tab_model(elong_soil_temp_moist)

elong_soil_temp_moist_interact<- lmer(mean_stem_elong ~ mean_soil_temp*mean_soil_moist*Species  + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_growth_temps)
tab_model(elong_soil_temp_moist_interact)

# DATA VISUALISATION -----

(plot_elong_ground_temp <- ggplot() +
    geom_point(aes(x = mean_ground_temp , y= mean_stem_elong, colour = Species, fill = Species), size = 3, alpha = 0.5, data = all_CG_growth_temps) +
    geom_smooth(aes(x = mean_ground_temp, y= mean_stem_elong, colour = Species, fill = Species), method = "lm", data = all_CG_growth_temps) +
    ylab("Stem elongation (mm)") +
    xlab("\nSurface temperature (degC)") +
    facet_wrap(~Species, scales = "free") +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

(plot_elong_soil_temp <- ggplot() +
    geom_point(aes(x = mean_soil_temp , y= mean_stem_elong, colour = Species, fill = Species), size = 3, alpha = 0.5, data = all_CG_growth_temps) +
    geom_smooth(aes(x = mean_soil_temp, y= mean_stem_elong, colour = Species, fill = Species), method = "lm", data = all_CG_growth_temps) +
    ylab("Stem elongation (mm)") +
    xlab("\nSoil temperature (degC)") +
    facet_wrap(~Species, scales = "free") +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

(plot_elong_soil_moist <- ggplot() +
    geom_point(aes(x = mean_soil_moist , y= mean_stem_elong, colour = Species, fill = Species), size = 3, alpha = 0.5, data = all_CG_growth_temps) +
    geom_smooth(aes(x = mean_soil_moist, y= mean_stem_elong, colour = Species, fill = Species), method = "lm", data = all_CG_growth_temps) +
    ylab("Stem elongation (mm)") +
    xlab("\nMean july soil moisture (%)") +
    facet_wrap(~Species, scales = "free") +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

