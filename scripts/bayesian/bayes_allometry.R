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

# removing an outlier from Pika data
Pika_all_shrub_biomass_edit <- Pika_all_shrub_biomass %>%
  distinct()
Pika_all_shrub_biomass_edit_2 <- Pika_all_shrub_biomass_edit[-1,] # plot 2a

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
                        data = Pika_all_shrub_biomass_edit_2, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(pul_allom) # not significant 
plot(pul_allom) # great
pp_check(pul_allom) # meh
# Equation: Biomass =  (1.08*height +-  5.17 ) + (18.16 *cover +-  8.42)

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
rich_data_2 <- rich[[2]]

(rich_height_allom <-ggplot(rich_data) +
    geom_point(data = QHI_salric_shrub_biomass, aes(x = max_height, y = biomass_per_m2),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii shrub biomass (g/m2)\n") +
    xlab("\n Shrub height (cm)" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

(rich_cov_allom <-ggplot(rich_data_2) +
    geom_point(data = QHI_salric_shrub_biomass, aes(x = percent_cover, y = biomass_per_m2),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii shrub biomass (g/m2) \n") +
    xlab("\n Cover (%)" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# 4.2. Salix pulchra -------
pul <- (conditional_effects(pul_allom))
pul_data <- pul[[1]]
pul_data_2 <- pul[[2]]

(pul_height_allom <-ggplot(pul_data) +
    geom_point(data = Pika_all_shrub_biomass_edit_2, aes(x = Shrub_Height_cm, y = biomass_per_m2),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Pulchra shrub biomass (g/m2)\n") +
    xlab("\n Shrub height (cm)" ) +
    #ylim(0, 4000 ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

(pul_cov_allom <-ggplot(pul_data_2) +
    geom_point(data = Pika_all_shrub_biomass_edit_2, aes(x = max_cover, y = biomass_per_m2),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Pulchra shrub biomass (g/m2) \n") +
    xlab("\n Cover (%)" ) +
   ylim(0, 1500) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# 4.2. Salix arctica -------
arc <- (conditional_effects(arc_allom))
arc_data <- arc[[1]]
arc_data_2 <- arc[[2]]

(arc_height_allom <-ggplot(arc_data) +
    geom_point(data = QHI_salarc_shrub_biomass, aes(x = max_height, y = biomass_per_m2),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Arctica shrub biomass (g/m2)\n") +
    xlab("\n Shrub height (cm)" ) +
    ylim(0, 300) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

(arc_cov_allom <-ggplot(arc_data_2) +
    geom_point(data = QHI_salarc_shrub_biomass, aes(x = percent_cover, y = biomass_per_m2),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Arctica shrub biomass (g/m2) \n") +
    xlab("\n Cover (%)" ) +
    ylim(0, 250) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# panels ------
height_all_allom <- grid.arrange(rich_height_allom,
                                 pul_height_allom,
                                 arc_height_allom, nrow = 1)

cov_all_allom <- grid.arrange(rich_cov_allom,
                                 pul_cov_allom,
                                 arc_cov_allom, nrow = 1)


