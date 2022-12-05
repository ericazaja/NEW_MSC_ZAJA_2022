### Making allometric equations for Salix spp.
### Script by Erica Zaja
### Last updated: 05/12/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(readxl)

# 1. LOADING DATA ----

# Biomass harvests and heights from Cunliffe et al 2020: QHI 
Andy_biomass <- read_csv("data/allometry/Andy_paper/biomass.csv")
Andy_heights <- read_csv("data/allometry/Andy_paper/heights.csv")

# Biomass harvests and heights from Myers-Smith PhD: Pika  
Biomass_harvest_Pika <- read_excel("data/allometry/Isla_phd/Biomass_harvest_Pika.xlsx")
Heights_Regression_Pika <- read_excel("data/allometry/Isla_phd/Heights_Regression_Pika.xlsx")

# Biomass harvests and heights from Berner's 2015 paper
Logan_data_biomass <- read_csv("data/allometry/Berner/Logan-data-biomass.csv")
