# BAYESIAN allometry -----

# 1. LOADING LIBRARIES -----
library(tidyverse)

# 2. DATA -----
# Andy Cunliffe data from QHI: S rich. and arctica
QHI_salarc_shrub_biomass <- read_csv("data/allometry/Andy_paper/QHI_salarc_shrub_biomass.csv")
QHI_salric_shrub_biomass <- read_csv("data/allometry/Andy_paper/QHI_salric_shrub_biomass.csv")

# Isla Myers Smith Phd data from Pika Camp: S. pulchra + S rich.
Pika_all_shrub_biomass <- read_csv("data/allometry/Isla_phd/Pika_all_shrub_biomass.csv")

# 3. MODELS -------
hist(Pika_all_shrub_biomass$biomass_per_m2, breaks = 7) # weird, not much data
hist(QHI_salric_shrub_biomass$biomass_per_m2, breaks = 20) # not super normal
hist(QHI_salarc_shrub_biomass$biomass_per_m2, breaks = 10) # weird,not much data


# 3.1. Salix richardsonii ------

rich_allom <- brms::brm(biomass_per_m2 ~ 0 + max_height + percent_cover,
                                 data = QHI_salric_shrub_biomass, family = gaussian(), chains = 3,
                                 iter = 5000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(rich_allom) # not significant 
plot(rich_allom) # great
pp_check(rich_allom) # fine
# biomass increases with height
# Equation: Biomass =  (18.05*height +- 5.11) + (11.55 *cover +-  18.07)

# 3.2. Salix pulchra -------
pul_allom <- brms::brm(biomass_per_m2 ~ 0 + Shrub_Height_cm + max_cover,
                        data = Pika_all_shrub_biomass, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(pul_allom) # not significant 
plot(pul_allom) # great
pp_check(pul_allom) # meh
# Equation: Biomass =  ( -23.66 *height +-  5.48) + (61.66 *cover +-  6.58)

# 3.3. Salix arctica -------
arc_allom <- brms::brm(biomass_per_m2 ~ 0 + max_height + percent_cover,
                       data = QHI_salarc_shrub_biomass, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(arc_allom) # not significant 
plot(arc_allom) # great
pp_check(arc_allom) # meh
# Equation: Biomass =  ( 1.51 *height +-  22.32) + (14.87 *cover +-  19.22)


# 4.DATA VISUALISATION------

# 4.1. Salix richardsonii -------
rich <- (conditional_effects(rich_allom))
rich_data <- rich[[1]]

(stemp2<-ggplot(arc_data_3) +
    geom_point(data = all_CG_growth_temps_arc, aes(x = mean_soil_moist, y = mean_stem_elong),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Mean stem elongation (mm)\n") +
    xlab("\n Mean soil temperature (degC)" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())


