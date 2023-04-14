# BAYESIAN: CG height and stem elongation over time 
#### Script by Erica Zaja, created 27/02/23
### Last updated: 27/02/23

# Libraries ------
library(tidybayes)
library(readr)
library(brms)
library(tidyverse)
library(gridExtra)

# DATA -------
all_CG_growth <- read_csv("data/common_garden_shrub_data/all_CG_growth.csv")

# Wrangle -------
# reclassing variables
all_CG_growth$Species <- as.factor(all_CG_growth$Species)
all_CG_growth$SampleID_standard <- as.factor(all_CG_growth$SampleID_standard)
all_CG_growth$population <- as.factor(all_CG_growth$population)
all_CG_growth$Site <- as.factor(all_CG_growth$Site)
all_CG_growth$Sample_Date <- as.POSIXct(all_CG_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_growth$Year <- as.factor(all_CG_growth$Year)

# Species specific ------
all_CG_growth_ric <- all_CG_growth %>%
  filter(Species == "Salix richardsonii")
  
all_CG_growth_pul<-  all_CG_growth%>%
  filter(Species == "Salix pulchra")

all_CG_growth_arc <-all_CG_growth%>%
  filter(Species == "Salix arctica")

# Only southern populations
all_CG_growth_ric_south <- all_CG_growth_ric %>%
  filter(population == "Southern")

all_CG_growth_pul_south <- all_CG_growth_pul %>%
  filter(population == "Southern")

all_CG_growth_arc_south <- all_CG_growth_arc %>%
  filter(population == "Southern") %>%
  filter(Canopy_Height_cm>=0)

# MODELLING 
# 1.STEM ELONG over time ------
# Salix richardsonii -------
elong_rich <- brms::brm(log(mean_stem_elong) ~ Sample_age*population +(1|Year),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_rich) # significant height growth over time
plot(elong_rich)
pp_check(elong_rich, type = "dens_overlay", nsamples = 100) 

# southern only 
elong_rich_south <- brms::brm(log(mean_stem_elong) ~ Sample_age + (1|Year),
                        data = all_CG_growth_ric_south,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_rich_south) # significant height growth over time
plot(elong_rich_south)
pp_check(elong_rich_south, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -----
elong_pul <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(1|Year),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_pul) # 
plot(elong_pul)
pp_check(elong_pul, type = "dens_overlay", nsamples = 100) 

# southern 
elong_pul_south <- brms::brm(log(mean_stem_elong) ~ Sample_age+(1|Year),
                       data = all_CG_growth_pul_south,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_pul_south) # 
plot(elong_pul_south)
pp_check(elong_pul_south, type = "dens_overlay", nsamples = 100) 


# Salix arctica -------
elong_arc <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(1|Year),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_arc) # significant growth over time
plot(elong_arc)
pp_check(elong_arc, type = "dens_overlay", nsamples = 100) 

# southern 
elong_arc_south <- brms::brm(log(mean_stem_elong) ~ Sample_age+(1|Year),
                       data = all_CG_growth_arc_south,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_arc_south) # significant growth over time
plot(elong_arc_south)
pp_check(elong_arc_south, type = "dens_overlay", nsamples = 100) 

# 2. HEIGHT over time ------
# Salix richardsonii -------

height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(1|Year),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

# only southern random slopes
# truncating to max richardsonii height from usda.gov (450cm, log(450)=6.109)
height_rich_south <- brms::brm(log(Canopy_Height_cm)|trunc(lb = 0, ub = 6.109248) ~ Sample_age+(Sample_age|SampleID_standard),
                         data = all_CG_growth_ric_south,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich_south) # significant height growth over time
plot(height_rich_south)
pp_check(height_rich_south, type = "dens_overlay", ndraws = 100) 



# Salix pulchra -----
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(1|Year),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

# only southern
# truncating to max height of pulchra 160 cm = log(160 )
height_pul_south <- brms::brm(log(Canopy_Height_cm) |trunc(lb = 0, ub =5.075174) ~ Sample_age+(Sample_age|SampleID_standard),
                               data = all_CG_growth_pul_south,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul_south) # 
plot(height_pul_south)
pp_check(height_pul_south, type = "dens_overlay", ndraws = 100) 

# Salix arctica -------
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(1|Year),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_arc) # significant growth over time
plot(height_arc)
pp_check(height_arc, type = "dens_overlay", nsamples = 100) 

# only southern
# truncarte to max height 23 cm, log(23)= 3.135494, -1 lower bound because some small values when logged give negative number eg log(0.5)
height_arc_south <- brms::brm(log(Canopy_Height_cm)|trunc(lb = -1, ub =3.135494) ~ Sample_age+(Sample_age|SampleID_standard),
                              data = all_CG_growth_arc_south,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_arc_south) # 
plot(height_arc_south)
pp_check(height_arc_south, type = "dens_overlay", ndraws = 100) 

# DATA VISUALISATION -------
# 1. HEIGHT -----
# Salix richardsonii ------
rich_height_1 <- (conditional_effects(height_rich_south))
rich_height_data <- rich_height_1[[1]]
rich_height_data_2 <- rich_height_1[[2]]

# this graph below only lets me plot one line
(rich_height_plot <-ggplot() +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = log(Canopy_Height_cm), colour = population),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, data = rich_height_data) +
  geom_line(aes(x = effect1__, y = estimate__),
                               linewidth = 1.5, data = rich_height_data) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii canopy height (log cm)\n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# this works well 
(rich_south_heights_plot_new <- all_CG_growth_ric_south %>%
    group_by(population) %>%
    add_predicted_draws(height_rich_south, allow_new_levels = TRUE) %>%
    ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), colour = ordered(population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_ric_south) +
    theme_shrub() +
    ylab("S. richardsonii (southern) canopy height (log cm)\n") +
    xlab("\nSample age") +
    theme(legend.position = "none"))

# Salix pulchra ------

(pul_south_heights_plot_new <- all_CG_growth_pul_south %>%
    group_by(population) %>%
    add_predicted_draws(height_pul_south, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_pul_south) +
    theme_shrub() +
    ylab("S. pulchra (southern) canopy height (log cm)\n") +
    xlab("\nSample age")+
   theme(legend.position = "none"))


# Salix arctica------
(arc_south_heights_plot_new <- all_CG_growth_arc_south %>%
  # group_by(population) %>%
   add_predicted_draws(height_arc_south, allow_new_levels = TRUE ) %>%
   ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc_south) +
   theme_shrub() +
   ylab("S. arctica (southern) canopy height (log cm)\n") +
   xlab("\nSample age")+
   theme(legend.position = "none"))

CG_height_panel_south <- grid.arrange(rich_south_heights_plot_new,
                                pul_south_heights_plot_new, 
                                arc_south_heights_plot_new, nrow = 1)
# 2.STEM ELONGATION -----
# Salix richardsonii -------
(rich_elong_plot_new <- all_CG_growth_ric_south %>%
  # group_by(population) %>%
   add_predicted_draws(elong_rich_south, allow_new_levels= TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(mean_stem_elong))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric_south) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Richardsonii stem elongation (log mm)\n") +
   xlab("\nSample age"))

# Salix pulchra ------
(pul_elong_plot_new <- all_CG_growth_pul_south %>%
  # group_by(population) %>%
   add_predicted_draws(elong_pul_south, allow_new_levels= TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(mean_stem_elong))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_pul_south) +
  scale_colour_viridis_d(begin = 0.1, end = 0.95) +
  scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Pulchra stem elongation (log mm)\n") +
   xlab("\nSample age"))


# Salix arctica------
(arc_elong_plot_new <- all_CG_growth_arc_south %>%
  # group_by(population) %>%
   add_predicted_draws(elong_arc_south, allow_new_levels= TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(mean_stem_elong))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc_south) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Arctica stem elongation (log mm)\n") +
   xlab("\nSample age"))

CG_height_panel <- grid.arrange(rich_heights_plot_new,
                                pul_heights_plot_new, 
                                arc_heights_plot_new, nrow = 1)
