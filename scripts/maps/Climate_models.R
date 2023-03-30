# CLIMATE MODELS ------

# libraries ----
library(bayesplot)
library(brms)
library(tidybayes)
library(tidyverse)


# data ------
coord.chelsa.combo.c.2020 <- read.csv("data/coord.chelsa.combo.c.2020")
coord.chelsa.combo.c.all.biom <- read.csv("data/coord.chelsa.combo.c.all.biom.csv")


# temp over time (2020-2100)-------
hist(coord.chelsa.combo.c.all.biom$mean_temp_C) # normal
unique(coord.chelsa.combo.c.all.biom$year_index) # I want 2020 as year 0
coord.chelsa.combo.c.all.biom <- coord.chelsa.combo.c.all.biom %>%
  mutate(year_index= I(year-2020))

temp_time_future <- brms::brm(mean_temp_C ~ year_index, 
                        data = coord.chelsa.combo.c.all.biom, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(temp_time_future) #significant ! 
plot(temp_time_future) # great
pp_check(temp_time_future, type = "dens_overlay", ndraws  = 100) 


# model: 2020 only----
# biomass in 2020 ~ temp in 2020 
model_test <- brms::brm(biomass_per_m2 ~ mean_temp_C, 
                        data = coord.chelsa.combo.c.2020, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(model_test) #significant ! 
plot(model_test) # great
pp_check(model_test, type = "dens_overlay", ndraws  = 100) 

# Quick data viz
mod <- (conditional_effects(model_test))
mod_data <- mod[[1]]

(biomass_temp_2020 <-ggplot(mod_data) +
    geom_point(data = coord.chelsa.combo.c.2020, aes(x =mean_temp_C, y =biomass_per_m2 ),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Shrub biomass (g/m2) \n") +
    xlab("\n Mean temperature in 2020 (C)" ) +
   # ylim(0, 1500) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# Quick data viz
future_time <- (conditional_effects(temp_time_future))
future_time_dat <- future_time[[1]]

(future_temps_plot <-ggplot(future_time_dat) +
    geom_point(data = coord.chelsa.combo.c.all.biom, aes(x =year_index, y =mean_temp_C ),
               alpha = 0.02)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5,colour = "red") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = 0.4) +
    ylab("Temperature (degC) \n") +
    xlab("\n Year (scaled)" ) +
    # ylim(0, 1500) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())


