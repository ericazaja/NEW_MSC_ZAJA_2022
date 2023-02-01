# Stem elongation vs mean july temperature
#### Script by Erica Zaja, created 01/02/23
### Last updated: 01/02/23

# Common garden july mean surface temp: 17.97°C 
# Based on 6 years data (TOMST+ HOBO)

# KP july mean surface temp: 9.25 °C 
# based on 2 years of data (TOMST)

# QHI july mean surface temp: 9.10 °C 
# based on 2022 TOMST and 2017 HOBO data 

# libraries
library(tidyverse)
library(readr)
library(lme4)
library(sjPlot)
# 1. Loading data ----
all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")

# 2. Data wrangling -----
# add temp column
all_CG_source_growth_temp <- all_CG_source_growth %>%
  mutate(july_mean_temp = case_when(Site == "Common_garden" ~ rep(17.97),
                                    Site == "Kluane" ~ rep(9.25),
                                    Site == "Qikiqtaruk" ~ rep(9.10))) %>%
   filter(population %in% c("Southern", "source_north", "source_south"))       

unique(all_CG_source_growth_temp$population) # "Southern"     "source_south" "source_north"
unique(all_CG_source_growth_temp$july_mean_temp) # 17.97  9.25  9.10
all_CG_source_growth_temp$Species <- as.factor(all_CG_source_growth_temp$Species)
all_CG_source_growth_temp$Site <- as.factor(all_CG_source_growth_temp$Site)
all_CG_source_growth_temp$july_mean_temp <- as.factor(all_CG_source_growth_temp$july_mean_temp)

# remove first few years of stem elongation data
unique(all_CG_source_growth_temp$Sample_age) # 17.97  9.25  9.10

all_CG_source_growth_temp_edit_1 <- all_CG_source_growth_temp %>%
  filter(Site %in% c("Kluane", "Qikiqtaruk"))

all_CG_source_growth_temp_edit_2 <- all_CG_source_growth_temp %>%
   filter(Sample_age %in% c("3", "4", "5", "6"))

all_CG_source_growth_temp_edit <- rbind(all_CG_source_growth_temp_edit_2, all_CG_source_growth_temp_edit_1)
unique(all_CG_source_growth_temp_edit$Sample_age) # 3-6
unique(all_CG_source_growth_temp_edit$Site)

# 3. Data visualisation -----

(scatter_elong_temp <- ggplot(all_CG_source_growth_temp) +
   geom_point(aes(x = july_mean_temp, y= mean_stem_elong, colour = Site, fill = Site), size = 3, alpha = 0.1, data = all_CG_source_growth_temp) +
   # geom_smooth(aes(x = july_mean_temp, y= mean_stem_elong, colour = Site, fill = Site, method = "glm"),  size = 3, alpha = 0.5, data = all_CG_source_growth_temp) +
    geom_boxplot(aes(x = july_mean_temp, y= mean_stem_elong, colour = Site, fill = Site), size = 0.5, alpha = 0.5) +
   ylab("Mean stem elongation (mm)") +
   xlab("\nMean july temperature (degC)") +
   facet_wrap(~Species, scales = "free") +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
   theme_shrub() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black"))) 


# Modelling -----
model_elong_temp <- lmer(mean_stem_elong ~ july_mean_temp + Species + (1|Site) , data = all_CG_source_growth_temp)
tab_model(model_elong_temp)
plot(model_elong_temp)

model_elong_temp_interaction <- lm(mean_stem_elong ~ july_mean_temp + Species*Site , data = all_CG_source_growth_temp)
tab_model(model_elong_temp_interaction)
plot(model_elong_temp_interaction)
                        