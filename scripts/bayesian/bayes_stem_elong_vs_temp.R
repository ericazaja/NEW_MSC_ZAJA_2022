# Bayesian stem elong vs temp ----
# DO stems grow more in the warmer sites vs the colder sites? 

# libraries -----
library(tidyverse)
library(brms)

# load data -----
all_cg_max_source <- read_csv("data/all_cg_max_source.csv") # with in situ temps
all_cg_max_source_CHELSA <- read_csv("data/all_cg_max_source_CHELSA.csv") # with chelsa temps

# wrangle ----
# so let's look at the  spp separately

all_cg_max_source_arctica <- all_cg_max_source %>%
  filter(Species == ("Salix arctica"))
all_cg_max_source_arctica_CHELSA <- all_cg_max_source_CHELSA %>%
  filter(Species == ("Salix arctica"))

all_cg_max_source_rich <- all_cg_max_source %>%
  filter(Species == ("Salix richardsonii"))
all_cg_max_source_rich_CHELSA <- all_cg_max_source_CHELSA %>%
  filter(Species == ("Salix richardsonii"))

all_cg_max_source_pulchra <- all_cg_max_source %>%
  filter(Species == ("Salix pulchra"))
all_cg_max_source_pulchra_CHELSA <- all_cg_max_source_CHELSA %>%
  filter(Species == ("Salix pulchra"))

all_cg_max_source_arctica$july_mean_temp <- as.factor(all_cg_max_source_arctica$july_mean_temp)
all_cg_max_source_arctica$Year <- as.factor(all_cg_max_source_arctica$Year)

all_cg_max_source_rich$july_mean_temp <- as.factor(all_cg_max_source_rich$july_mean_temp)
all_cg_max_source_rich$Year <- as.factor(all_cg_max_source_rich$Year)
all_cg_max_source_rich$population <- as.factor(all_cg_max_source_rich$population)

all_cg_max_source_pulchra$july_mean_temp <- as.factor(all_cg_max_source_pulchra$july_mean_temp)
all_cg_max_source_pulchra$Year <- as.factor(all_cg_max_source_pulchra$Year)

all_cg_max_source_arctica_CHELSA$july_mean_temp <- as.factor(all_cg_max_source_arctica_CHELSA$july_mean_temp)
all_cg_max_source_arctica_CHELSA$Year <- as.factor(all_cg_max_source_arctica_CHELSA$Year)

all_cg_max_source_rich_CHELSA$july_mean_temp <- as.factor(all_cg_max_source_rich_CHELSA$july_mean_temp)
all_cg_max_source_rich_CHELSA$Year <- as.factor(all_cg_max_source_rich_CHELSA$Year)

all_cg_max_source_pulchra_CHELSA$july_mean_temp <- as.factor(all_cg_max_source_pulchra_CHELSA$july_mean_temp)
all_cg_max_source_pulchra_CHELSA$Year <- as.factor(all_cg_max_source_pulchra_CHELSA$Year)

# Bayesian models ------
# 1. with IN SITU temperatures -----
# S. richardsonii -----
elong_temp_rich_box <- brms::brm(log(mean_stem_elong) ~ july_mean_temp + (1|Year),
                             data = all_cg_max_source_rich, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(elong_temp_rich_box) # significant 
plot(elong_temp_rich_box)
pp_check(elong_temp_rich_box) # beautiful

# S. pulchra -----
elong_temp_pul_box <- brms::brm(log(mean_stem_elong) ~ july_mean_temp + (1|Year),
                                 data = all_cg_max_source_pulchra, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(elong_temp_pul_box) # not significant 
plot(elong_temp_pul_box)
pp_check(elong_temp_pul_box) # beautiful

# S. arctica -----
# model below doesnt converge
elong_temp_arc_box <- brms::brm(log(mean_stem_elong) ~ july_mean_temp,
                                data = all_cg_max_source_arctica, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(elong_temp_arc_box) # not significant 
plot(elong_temp_arc_box)
pp_check(elong_temp_arc_box) # beautiful

# 2. with CHELSA temperatures -----
# S. richardsonii -----
elong_temp_rich_box_chelsa <- brms::brm(log(mean_stem_elong) ~ july_mean_temp + (1|Year),
                                 data = all_cg_max_source_rich_CHELSA, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(elong_temp_rich_box_chelsa) # significant 
plot(elong_temp_rich_box_chelsa)
pp_check(elong_temp_rich_box_chelsa) # beautiful

# S. pulchra -----
elong_temp_pul_box_chelsa <- brms::brm(log(mean_stem_elong) ~ july_mean_temp + (1|Year),
                                data = all_cg_max_source_pulchra_CHELSA, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(elong_temp_pul_box_chelsa) # significant 
plot(elong_temp_pul_box_chelsa)
pp_check(elong_temp_pul_box_chelsa) # beautiful

# S. arctica -----
# doesnt converge 
elong_temp_arc_box_chelsa <- brms::brm(log(mean_stem_elong) ~ july_mean_temp + (1|Year),
                                data = all_cg_max_source_arctica_CHELSA, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(elong_temp_arc_box_chelsa) # not significant 
plot(elong_temp_arc_box_chelsa)
pp_check(elong_temp_arc_box_chelsa) # beautiful

# Data visualisation -------

# Salix richardsonii -----
ric_elong_temp_box <- (conditional_effects(elong_temp_rich_box)) # extracting conditional effects from bayesian model
ric_elong_temp_box_data <- ric_elong_temp_box[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_height_box <-ggplot(ric_elong_temp_box_data) +
    geom_violin(data = all_cg_max_source_rich, aes(x = july_mean_temp, y = log(mean_stem_elong), fill = july_mean_temp, colour = july_mean_temp),
                alpha = 0.1)+ # raw data
    geom_jitter(data = all_cg_max_source_rich, aes(x = july_mean_temp, y = log(mean_stem_elong), colour = july_mean_temp),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__, colour = july_mean_temp), width=0.5, size = 4)+
   geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = july_mean_temp),
      alpha = 1,  width=.5) +
    ylab("Salix richardsonii stem elongation (log, mm)\n") +
    xlab("\n Mean July temperature in situ (Kluane, QHI, CG)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# Salix pulchra -----
pul_elong_temp_box <- (conditional_effects(elong_temp_pul_box)) # extracting conditional effects from bayesian model
pul_elong_temp_box_data <- pul_elong_temp_box[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_height_box <-ggplot(pul_elong_temp_box_data) +
    geom_violin(data = all_cg_max_source_pulchra, aes(x = july_mean_temp, y = log(mean_stem_elong), fill = july_mean_temp, colour = july_mean_temp),
                alpha = 0.1)+ # raw data
    geom_jitter(data = all_cg_max_source_pulchra, aes(x = july_mean_temp, y = log(mean_stem_elong), colour = july_mean_temp),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__, colour = july_mean_temp), width=0.5, size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = july_mean_temp),
                  alpha = 1,  width=.5) +
    ylab("Salix pulchra stem elongation (log, mm)\n") +
    xlab("\n Mean July temperature in situ (Kluane, QHI, CG)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())


# Salix richardsonii chelsa -----
ric_elong_temp_box_chelsa <- (conditional_effects(elong_temp_rich_box_chelsa)) # extracting conditional effects from bayesian model
ric_elong_temp_box_data_2 <- ric_elong_temp_box_chelsa[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_height_box <-ggplot(ric_elong_temp_box_data_2) +
    geom_violin(data = all_cg_max_source_rich_CHELSA, aes(x = july_mean_temp, y = log(mean_stem_elong), fill = july_mean_temp, colour = july_mean_temp),
                alpha = 0.1)+ # raw data
    geom_jitter(data = all_cg_max_source_rich_CHELSA, aes(x = july_mean_temp, y = log(mean_stem_elong), colour = july_mean_temp),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__, colour = july_mean_temp), width=0.5, size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = july_mean_temp),
                  alpha = 1,  width=.5) +
    ylab("Salix richardsonii stem elongation (log, mm)\n") +
    xlab("\n Mean July temperature CHELSA (QHI, Kluane, CG)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# Salix pulchra chelsa-----
pul_elong_temp_box_chelsa <- (conditional_effects(elong_temp_pul_box_chelsa)) # extracting conditional effects from bayesian model
pul_elong_temp_box_data_chelsa <- pul_elong_temp_box_chelsa[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_height_box <-ggplot(pul_elong_temp_box_data_chelsa) +
    geom_violin(data = all_cg_max_source_pulchra_CHELSA, aes(x = july_mean_temp, y = log(mean_stem_elong), fill = july_mean_temp, colour = july_mean_temp),
                alpha = 0.1)+ # raw data
    geom_jitter(data = all_cg_max_source_pulchra_CHELSA, aes(x = july_mean_temp, y = log(mean_stem_elong), colour = july_mean_temp),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__, colour = july_mean_temp), width=0.5, size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = july_mean_temp),
                  alpha = 1,  width=.5) +
    ylab("Salix pulchra stem elongation (log, mm)\n") +
    xlab("\n Mean July temperature CHELSA (QHI, Kluane, CG)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())
