### Making allometric equations for Salix spp.
### Script by Erica Zaja
### Last updated: 05/12/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(readxl)

# 2. LOADING DATA ----

# Biomass harvests (grams) and heights from Cunliffe et al 2020: QHI 
Andy_biomass <- read_csv("data/allometry/Andy_paper/biomass.csv")
Andy_heights <- read_excel("data/allometry/Andy_paper/heights.xlsx")

# Biomass harvests and heights from Myers-Smith PhD: Pika  
Biomass_harvest_Pika <- read_excel("data/allometry/Isla_phd/Biomass_harvest_Pika.xlsx")
Heights_Regression_Pika <- read_excel("data/allometry/Isla_phd/Heights_Regression_Pika.xlsx")

# Biomass harvests and heights from Berner's 2015 paper
Logan_data_biomass <- read_csv("data/allometry/Berner/Logan-data-biomass.csv")

# 3. DATA WRANGLING -----
Andy_biomass <- Andy_biomass[-1,] # removing row with words

Andy_heights_salix <- Andy_heights %>%
  filter(Species %in% c("Salix arctica", "Salix pulchra", "Salix richardsonii" ))

Andy_heights_salix$Height <-as.numeric(Andy_heights_salix$Height)

Andy_heights_salix <- Andy_heights_salix %>% na.omit()

# merge biomass and heights data  by plot N 

# 4. MODELLING: regression
# biomass ~ height  