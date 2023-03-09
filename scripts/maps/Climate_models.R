# CLIMATE MODELS ------

coord.chelsa.combo.b <- read.csv("data/coord.chelsa.combo.b")


# model: 2100 only----
# BASICALLY WHAT I HAVE HERE IS PREDICTED TEMPS in 2100 AND biomass in 2020, so
# I cant relate them, I need to multiply by my rate of change before

model_test <- brms::brm(biomass_per_m2 ~ mean_temp_2020_C, 
                        data = coord.chelsa.combo.c, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(model_test) #significant ! 
plot(model_test) # great
pp_check(model_test, type = "dens_overlay", ndraws  = 100) 
library(bayesplot)
