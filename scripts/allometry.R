### Making allometric equations for Salix spp.
### Script by Erica Zaja
### Last updated: 05/12/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(readxl)
library(sjPlot)

# 2. LOADING DATA ----

# Biomass harvests (grams) and heights from Cunliffe et al 2020: QHI 
Andy_biomass <- read_csv("data/allometry/Andy_paper/biomass.csv")
Andy_heights <- read_excel("data/allometry/Andy_paper/heights.xlsx")
Andy_main_database <- read_excel("data/allometry/Andy_paper/main_database.xlsx")

# Biomass harvests and heights from Myers-Smith PhD: Pika  
Biomass_harvest_Pika <- read_excel("data/allometry/Isla_phd/Biomass_harvest_Pika.xlsx")
Heights_Regression_Pika <- read_excel("data/allometry/Isla_phd/Heights_Regression_Pika.xlsx")

# Biomass harvests and heights from Berner's 2015 paper
Logan_data_biomass <- read_excel("data/allometry/Berner/Logan-data-biomass.xlsx")

# 3. DATA WRANGLING -----
Andy_biomass <- Andy_biomass[-1,] # removing row with words

Andy_heights_salix <- Andy_heights %>%
  filter(Species %in% c("Salix arctica", "Salix richardsonii" ))

Andy_heights_salix$Height <-as.numeric(Andy_heights_salix$Height)

Andy_heights_salix <- Andy_heights_salix %>% na.omit()  

Andy_heights_richardsonii <- Andy_heights_salix %>%
  filter(Species == "Salix richardsonii" )

Logan_salix_pulchra <- Logan_data_biomass %>% 
  filter(Genus == "Salix" & Species == "pulchra") %>%
  select(Genus, Species, Region, Ecosystem, "Height (cm)", "AGB (g)") %>%
  na.omit() %>%
  rename("AGB_g" = "AGB (g)",
        "Height_cm" = "Height (cm)")

# merge biomass and heights data  by plot N 

# 4. MODELLING: regression

# biomass ~ height  

# Logan
logan_model <- lm(AGB_g ~ Height_cm, data = Logan_salix_pulchra)
summary(logan_model)
tab_model(logan_model)

(plot_logan_model <- ggplot(Logan_salix_pulchra) +
    geom_point(aes(x = Height_cm, y= AGB_g), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = Height_cm, y= AGB_g), colour = "brown",method = "lm") +
    ylab("AGB (g)") +
    xlab("\nHeight (cm)") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))
