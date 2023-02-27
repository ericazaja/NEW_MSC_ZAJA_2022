# BAYESIAN: CG height and stem elongation over time 
#### Script by Erica Zaja, created 27/02/23
### Last updated: 27/02/23

# Libraries ------
library(tidybayes)
library(brms)
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

# MODELLING 
# 1. HEIGHTS over time ------
# Salix richardsonii -------
height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population,
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich) # significant height growth over time
plot(height_rich)
pp_check(height_rich, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -----
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population,
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul) # 
plot(height_pul)
pp_check(height_pul, type = "dens_overlay", nsamples = 100) 

# Salix arctica -------
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population,
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_arc) # significant growth over time
plot(height_arc)
pp_check(height_arc, type = "dens_overlay", nsamples = 100) 

# 2. STEM ELONGATION over time ------

# DATA VISUALISATION -------
# 1. HEIGHT -----
# Salix richardsonii ------
rich_height_1 <- (conditional_effects(height_rich))
rich_height_data <- rich_height_1[[1]]
rich_height_data_2 <- rich_height_1[[2]]

# this graph below only lets me plot one line
(rich_height_plot <-ggplot(rich_height_data) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = log(Canopy_Height_cm), colour = population),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii canopy height (log cm)\n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# this works well
(rich_heights_plot_new <- all_CG_growth_ric %>%
    group_by(population) %>%
    add_predicted_draws(height_rich) %>%
    ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_ric) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub() +
    ylab("Richardsonii canopy height (log cm)\n") +
    xlab("\nSample age"))

# Salix pulchra ------

(pul_heights_plot_new <- all_CG_growth_pul %>%
    group_by(population) %>%
    add_predicted_draws(height_pul) %>%
    ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_pul) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub() +
    ylab("Pulchra canopy height (log cm)\n") +
    xlab("\nSample age"))


# Salix arctica------
(arc_heights_plot_new <- all_CG_growth_arc %>%
   group_by(population) %>%
   add_predicted_draws(height_arc) %>%
   ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Arctica canopy height (log cm)\n") +
   xlab("\nSample age"))

CG_height_panel <- grid.arrange(rich_heights_plot_new,
                                pul_heights_plot_new, 
                                arc_heights_plot_new, nrow = 1)
# 2.STEM ELONGATION -----

