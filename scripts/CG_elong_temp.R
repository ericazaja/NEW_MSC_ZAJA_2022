# CG stem elongation vs annual temperature
#### Script by Erica Zaja, created 30/01/23
### Last updated: 30/01/23

# Workflow:
# model annual stem elongation as a function of annual temperature
# INFORMATIVE analysis, not used in the allom. equations. 
# Just to see if we see more growth in warmer years
#
# 1. Libraries -------
library(tidyverse)
library(readr)

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




