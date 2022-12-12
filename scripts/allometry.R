### Making allometric equations for Salix spp.
### Script by Erica Zaja
### Last updated: 05/12/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(readxl)
library(sjPlot)

# 2. LOADING DATA ----

# Biomass harvests (grams) and heights from Cunliffe et al 2020: QHI 
# NB data only for 50 x 50 cm plots (aka might not be all of the shrub)
Andy_biomass <- read_csv("data/allometry/Andy_paper/biomass.csv")
Andy_heights <- read_excel("data/allometry/Andy_paper/heights.xlsx")
Andy_main_database <- read_excel("data/allometry/Andy_paper/main_database.xlsx")

# Biomass harvests and heights from Myers-Smith PhD: Pika  
# NB data only for 50 x 50 cm plots (aka might not be all of the shrub)
Biomass_harvest_Pika <- read_excel("data/allometry/Isla_phd/Biomass_harvest_Pika.xlsx")
Heights_Regression_Pika <- read_excel("data/allometry/Isla_phd/Heights_Regression_Pika.xlsx")
Pika_biomass_heights_edit <- read_excel("data/allometry/Isla_phd/Pika_biomass_heights_edit.xlsx")
Biomass_Calculations_Pika <- read_excel("data/allometry/Isla_phd/Biomass Calculations Pika.xlsx")

# Biomass harvests and heights from Berner's 2015 paper
# N.B. data for the full shrub 
Logan_data_biomass <- read_excel("data/allometry/Berner/Logan-data-biomass.xlsx")

# 3. DATA WRANGLING -----

## do the maximum height for Andy’s data - he used a point framing grid and 
# did a bunch of heights,but the most relevant thing is the maximum height for 
# what you want to do.

# Then the shrubs are always the tallest thing in those plots if there are shrubs there, 
# so you don’t really need to worry about knowing what species it is.  
# The shrub is always Salix richardsonii or Salix arctica.  
# Only use the Sal arc data if there is no richardsonii in the plot.

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

Biomass_Pika <- Biomass_Calculations_Pika %>%
  select(Plot...2, `Tall Shrubs...12`) # merge with pika heights 

# merge biomass and heights data  by plot N 

# 4. MODELLING: regression

# biomass ~ height  

# Isla: Salix pulchra and richardsonii
isla_model <- lm(Biomass_g ~ Shrub_Height_cm, data = Pika_biomass_heights_edit)
summary(isla_model)
tab_model(isla_model)

# **ADD a 0, 0 value to the data from my PhD (like Logan's)**

(plot_isla_model <- ggplot(Pika_biomass_heights_edit) +
    geom_point(aes(x = Shrub_Height_cm, y= Biomass_g), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = Shrub_Height_cm, y= Biomass_g), colour = "brown",method = "lm") +
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


# Logan: Salix pulchra
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
