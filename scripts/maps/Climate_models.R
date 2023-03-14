# CLIMATE MODELS ------

# libraries ----
library(bayesplot)
library(brms)

# data ------
coord.chelsa.combo.c.2020 <- read.csv("data/coord.chelsa.combo.c.2020")

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
