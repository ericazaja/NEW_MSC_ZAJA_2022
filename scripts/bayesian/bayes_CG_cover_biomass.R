# BAYESIAN: CG cover and biomass calculation and visualisation script
#### Script by Erica Zaja, created 23/02/23
### Last updated: 23/02/23


# LIBRARIES -----
library(tidyverse)
library(brms)

# DATA -----
all_CG_growth_cover <- read_csv("data/all_CG_growth_cover.csv")

# BIOMASS CG ------
# calculate biomass for each species based on BAYESIAN allometric equations

# Salix richardsonii ------
# Equation: Biomass =  (18.05*height +- 5.11) + (11.55 *cover +-  18.07)
CG_ric_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix richardsonii" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (18.05*Canopy_Height_cm) + (11.55*cover_percent), 
         biomass_error_high = biomass_per_m2 + 5.11 + 18.07,
         biomass_error_low = biomass_per_m2 - 5.11 - 18.07)

range(CG_ric_cover_biomass$biomass_per_m2) # 142.8825 2699.6420
write.csv(CG_ric_cover_biomass, "data/common_garden_shrub_data/CG_ric_cover_biomass.csv")

# Salix pulchra ------
# Equation: Biomass =  (1.08*height +-  5.17 ) + (18.16 *cover +-  8.42)
CG_pul_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix pulchra" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (1.08*Canopy_Height_cm) + (18.16*cover_percent), 
         biomass_error_high = biomass_per_m2 + 5.17 + 8.42,
         biomass_error_low = biomass_per_m2 - 5.17 - 8.42)

range(CG_pul_cover_biomass$biomass_per_m2) # 4.017768 1847.963440
write.csv(CG_pul_cover_biomass, "data/common_garden_shrub_data/CG_pul_cover_biomass.csv")

# Salix arctica -----
# Biomass =  ( 1.51 *height +-  22.32) + (14.87 *cover +-  19.22)
CG_arc_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix arctica" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (1.51*Canopy_Height_cm) + (14.87*cover_percent), 
         biomass_error_high = biomass_per_m2 + 22.32 + 19.22,
         biomass_error_low = biomass_per_m2 - 22.32 - 19.22)

range(CG_arc_cover_biomass$biomass_per_m2) # 9.60174 358.80240
write.csv(CG_arc_cover_biomass, "data/common_garden_shrub_data/CG_arc_cover_biomass.csv")

