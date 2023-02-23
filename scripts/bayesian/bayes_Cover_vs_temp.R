# Cover vs mean july temperature
#### Script by Erica Zaja, created 23/02/2023
### Last updated: 01/02/23

# 1. LIBRARIES -----
library(tidyverse)
library(brms)

# 2. DATA ------
all_cover_temps_long <- read_csv("data/all_cover_temps_long.csv")
all_cover_temps <- read_csv("data/all_cover_temps.csv")

# 3. WRANGLE -----
# sp - specific datasets

all_cover_temps_long_pulchra <- all_cover_temps_long %>%
  filter(Species == "Salix pulchra")

all_cover_temps_long_rich <- all_cover_temps_long %>%
  filter(Species == "Salix richardsonii") 

all_cover_temps_long_arctica <- all_cover_temps_long %>%
  filter(Species == "Salix arctica")

# explore 
hist(all_cover_temps_long_arctica$cover_change_percent) # left skew
hist(all_cover_temps_long_pulchra$cover_change_percent)# left skew
hist(all_cover_temps_long_rich$cover_change_percent)# left skew


# 4. MODELLING ------
# Salix richardsonii -----
# cover change per unit temp 
cover_temp_rich <- brms::brm(cover_change_percent ~ mean_temp + (1|Site),
                        data = all_cover_temps_long_rich,  family = skew_normal(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_rich) # not significant cover change per unit temp
plot(cover_temp_rich)
pp_check(cover_temp_rich, type = "dens_overlay", nsamples = 100) 


# Salix pulchra -----
# cover change per unit temp 

# Salix arctica -----
# cover change per unit temp 

