# CLIMATE MODELS ------

coord.chelsa.combo.b <- read.csv("data/coord.chelsa.combo.b")


# model: 2100 only----

model_test <- brms::brm(biomass_per_m2 ~ july_temp, 
                        data = coord.chelsa.combo.b, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(model_test) # not significant 
plot(model_test) # great
pp_check(model_test, type = "dens_overlay", ndraws  = 100) 
library(bayesplot)
