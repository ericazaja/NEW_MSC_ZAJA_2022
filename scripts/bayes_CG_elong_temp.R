# CG stem elongation vs annual temperature
#### Script by Erica Zaja, created 22/02/23
### Last updated: 22/02/23

# Workflow:
# model annual stem elongation as a function of annual temperature
# INFORMATIVE analysis, not used in the allom. equations. 
# Just to see if we see more growth in warmer years

# 1. Libraries -------
library(tidyverse)
library(readr)
library(brms)
library(tidybayes)

# 2. Load data -----
all_CG_growth_temps <- read_csv("data/all_CG_growth_temps.csv")

# 3. Wrangling -----
# separate into species specific datasets
all_CG_growth_temps_rich <- all_CG_growth_temps %>%
  filter(Species == "Salix richardsonii")

all_CG_growth_temps_pul <- all_CG_growth_temps %>%
  filter(Species == "Salix pulchra")

all_CG_growth_temps_arc <- all_CG_growth_temps %>%
  filter(Species == "Salix arctica")

# distributions
hist(all_CG_growth_temps_arc$mean_stem_elong, breaks=30) # quite normal
hist(all_CG_growth_temps_pul$mean_stem_elong, breaks=10) # right skew
hist(all_CG_growth_temps_rich$mean_stem_elong, breaks=10) # right skew

# 4. Modelling ------
# 4.1. Surface temp ------
# richardsonii -----
elong_temp_rich <- brms::brm(log(mean_stem_elong) ~ mean_ground_temp + (1|Year) + (1|Sample_age),
                            data = all_CG_growth_temps_rich, family = gaussian(), chains = 3,
                            iter = 3000, warmup = 1000)
summary(elong_temp_rich) # not significant 
plot(elong_temp_rich)
pp_check(elong_temp_rich) # beautiful

# pulchra ------
elong_temp_pul <- brms::brm(log(mean_stem_elong) ~ mean_ground_temp + (1|Year) + (1|Sample_age),
                             data = all_CG_growth_temps_pul, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000)
summary(elong_temp_pul) # not significant 
plot(elong_temp_pul)
pp_check(elong_temp_pul) # beautiful


# arctica ------
elong_temp_arc <- brms::brm(mean_stem_elong ~ mean_ground_temp + (1|Year) + (1|Sample_age),
                            data = all_CG_growth_temps_arc, family = gaussian(), chains = 3,
                            iter = 3000, warmup = 1000)
summary(elong_temp_arc) # not significant 
plot(elong_temp_arc)
pp_check(elong_temp_arc)

# 4.2. Soil temp ------
# 4.3. Soil moist ------
# 4.4. Soil temperature*soil moisture ------


# 5. Data visualisation ------
(elong_temp_pul_plot <- all_CG_growth_temps_pul %>%
   add_predicted_draws(elong_temp_pul) %>%  # adding the posterior distribution
   ggplot(aes(x = mean_ground_temp, y = mean_stem_elong)) +  
   stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                   alpha = 0.5, colour = "black") +
   geom_point(data = all_CG_growth_temps_pul, colour = "darkseagreen4", size = 3) +   # raw data
   scale_fill_brewer(palette = "Greys") +
   ylab("Mean stem elongation (mm)\n") +  # latin name for red knot
   xlab("\nMean surface temperature (degC)") +
   theme_shrub() +
   theme(legend.title = element_blank(),
         legend.position = c(0.15, 0.85)))

