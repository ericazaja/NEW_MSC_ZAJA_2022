# BAYESIAN: CG cover and biomass calculation and visualisation script
#### Script by Erica Zaja, created 23/02/23
### Last updated: 23/02/23

# LIBRARIES -----
library(tidyverse)
library(brms)
library(gridExtra)

# DATA -----
all_CG_growth_cover <- read_csv("data/all_CG_growth_cover.csv")

# 1. CG BIOMASS derivation from bayesian allom. equations ------
# calculate biomass for each species based on BAYESIAN allometric equations

# Salix richardsonii ------
# Equation:# FINAL EQUATION: Biomass =  (18.0*height +- 5.1) + (11.9 *cover +-  18.0)
CG_ric_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix richardsonii" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (18.0*Canopy_Height_cm) + (11.9*cover_percent), 
         biomass_error_high = biomass_per_m2 + 5.1 + 18.0,
         biomass_error_low = biomass_per_m2 - 5.1 - 18.0)

range(CG_ric_cover_biomass$biomass_per_m2) # 142.735 2709.216
write.csv(CG_ric_cover_biomass, "data/common_garden_shrub_data/CG_ric_cover_biomass.csv")

# Salix pulchra ------
# Equation: FINAL EQUATION:  Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)
CG_pul_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix pulchra" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (1.1*Canopy_Height_cm) + (18.1*cover_percent), 
         biomass_error_high = biomass_per_m2 + 5.0 + 8.2,
         biomass_error_low = biomass_per_m2 - 5.0 - 8.2)

range(CG_pul_cover_biomass$biomass_per_m2) # 4.05163 1842.96790
write.csv(CG_pul_cover_biomass, "data/common_garden_shrub_data/CG_pul_cover_biomass.csv")

# Salix arctica -----
# FINAL EQUATION:  # Equation: Biomass =  ( 2.2 *height +-  24.0) + (14.2 *cover +-  20.7)
CG_arc_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix arctica" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (2.2*Canopy_Height_cm) + (14.2*cover_percent), 
         biomass_error_high = biomass_per_m2 + 24.0 + 20.7,
         biomass_error_low = biomass_per_m2 - 24.0 - 20.7)

range(CG_arc_cover_biomass$biomass_per_m2) # 10.9884 347.1840
write.csv(CG_arc_cover_biomass, "data/common_garden_shrub_data/CG_arc_cover_biomass.csv")

# merge all into one dataset 
CG_ALL_cover_biomass <- rbind(CG_arc_cover_biomass, CG_pul_cover_biomass,
                              CG_ric_cover_biomass)

# 2. EXPLORE ------
hist(CG_arc_cover_biomass$cover_percent, breaks = 30) # not normal
hist(CG_pul_cover_biomass$cover_percent, breaks = 30)# not normal
hist(CG_ric_cover_biomass$cover_percent, breaks = 30)# not normal
hist(CG_ALL_cover_biomass$cover_percent, breaks = 30)# not normal

hist(CG_arc_cover_biomass$biomass_per_m2, breaks = 30) # not normal
hist(CG_pul_cover_biomass$biomass_per_m2, breaks = 30)# not normal
hist(CG_ric_cover_biomass$biomass_per_m2, breaks = 30) # almost normal
hist(CG_ALL_cover_biomass$biomass_per_m2, breaks = 30)# not normal

# 3. MODELLING ------

# 3.1. COVER over time ------
# Salix richardsonii -------
cover_rich <- brms::brm((cover_percent/100) ~ Sample_age + (1|Sample_age),
                                 data = CG_ric_cover_biomass,  family = "beta", chains = 3,
                                 iter = 5000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_rich) # significant cover growth over time
plot(cover_rich)
pp_check(cover_rich, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -------
cover_pul <- brms::brm((cover_percent/100) ~ Sample_age + (1|Sample_age),
                        data = CG_pul_cover_biomass,  family = "beta", chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_pul) # not significant cover growth over time
plot(cover_pul)
pp_check(cover_pul, type = "dens_overlay", nsamples = 100) 

# Salix arctica -------
cover_arc <- brms::brm((cover_percent/100) ~ Sample_age + (1|Sample_age),
                       data = CG_arc_cover_biomass,  family = "beta", chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_arc) #  significant cover growth over time
plot(cover_arc)
pp_check(cover_arc, type = "dens_overlay", nsamples = 100) 


# RANDOM SLOPE MODEL ------
CG_ALL_cover_biomass$Species <- as.factor(CG_ALL_cover_biomass$Species)
cover_random_slopes <- brms::brm((cover_percent/100) ~ Sample_age + (1+Sample_age|Species),
                                 data = CG_ALL_cover_biomass,  family = "beta", chains = 3,
                                 iter = 5000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_random_slopes) #  significant cover growth over time
plot(cover_random_slopes)
pp_check(cover_random_slopes, type = "dens_overlay", nsamples = 100) 

  
# 3.2. BIOMASS over time -----
# Salix richardsonii -----
biom_rich <- brms::brm(log(biomass_per_m2) ~ Sample_age + (1|Sample_age),
                        data = CG_ric_cover_biomass,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(biom_rich) # not significant cover growth over time
plot(biom_rich)
pp_check(biom_rich, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -----
biom_pul <- brms::brm(log(biomass_per_m2) ~ Sample_age + (1|Sample_age),
                       data = CG_pul_cover_biomass,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(biom_pul) # significant cover growth over time
plot(biom_pul)
pp_check(biom_pul, type = "dens_overlay", nsamples = 100) 

# Salix arctica ------
biom_arc <- brms::brm(log(biomass_per_m2) ~ Sample_age + (1|Sample_age),
                      data = CG_arc_cover_biomass,  family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(biom_arc) # significant cover growth over time
plot(biom_arc)
pp_check(biom_arc, type = "dens_overlay", nsamples = 100) 

# RANDOM SLOPE MODEL ------
biom_random_slopes <- brms::brm(log(biomass_per_m2) ~ Sample_age + (1+Sample_age|Species),
                                 data = CG_ALL_cover_biomass,  family = gaussian(), chains = 3,
                                 iter = 5000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(biom_random_slopes) #  significant cover growth over time
plot(biom_random_slopes)
pp_check(biom_random_slopes, type = "dens_overlay", nsamples = 100) 

# 4. DATA VISUALISATION ---------
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# 4.1 COVER -----
# Salix richardsonii ------
rich_cov_1 <- (conditional_effects(cover_rich))
rich_cov_data_1 <- rich_cov_1[[1]]

(rich_cover_plot <-ggplot(rich_cov_data_1) +
    geom_point(data = CG_ric_cover_biomass, aes(x = Sample_age, y = (cover_percent/100)),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii cover (prop.)\n") +
    xlab("\n Sample age" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# Salix pulchra ------
pul_cov_1 <- (conditional_effects(cover_pul))
pul_cov_data_1 <- pul_cov_1[[1]]

(pul_cover_plot <-ggplot(pul_cov_data_1) +
    geom_point(data = CG_pul_cover_biomass, aes(x = Sample_age, y = (cover_percent/100)),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Pulchra cover (prop.)\n") +
    xlab("\n Sample age" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# Salix arctica ------
arc_cov_1 <- (conditional_effects(cover_arc))
arc_cov_data_1 <- arc_cov_1[[1]]

(arc_cover_plot <-ggplot(arc_cov_data_1) +
    geom_point(data = CG_arc_cover_biomass, aes(x = Sample_age, y = (cover_percent/100)),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Arctica cover (prop.)\n") +
    xlab("\n Sample age" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# random slope model plot ----
(cover_random_slopes_plot <- CG_ALL_cover_biomass %>%
  bind_cols(as_tibble(fitted(cover_random_slopes))) %>%
  group_by(Species) %>%
  ggplot() +
  geom_point(aes(x = Sample_age, y = (cover_percent/100)), size = 4, alpha = .75, color = "dodgerblue2") +
  geom_point(aes(x = Sample_age, y = Estimate), shape = 1, size = 4, stroke = 1.5) +
   geom_smooth(aes(x = Sample_age, y = (cover_percent/100), colour = Species, fill = Species)) +
 labs(x = "Sample age", y = "Shrub cover (proportion)",
   subtitle = "Blue points are observed values. Black circles are fitted values.") +
 # scale_x_continuous(expand = c(.075, .075), breaks = 0:3) +
  #facet_wrap(~Species, nrow = 1) +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +   
  theme_shrub() +
  theme(plot.title = element_text(hjust = .5)))

# 4.2 BIOMASS -----
# Salix richardsonii ------
rich_biom_1 <- (conditional_effects(biom_rich))
rich_biom_data_1 <- rich_biom_1[[1]]

(rich_biom_plot <-ggplot(rich_biom_data_1) +
    geom_point(data = CG_ric_cover_biomass, aes(x = Sample_age, y = log(biomass_per_m2)),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii biomass (log g/m2)\n") +
    xlab("\n Sample age" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# Salix pulchra ------
pul_biom_1 <- (conditional_effects(biom_pul))
pul_biom_data_1 <- pul_biom_1[[1]]

(pul_biom_plot <-ggplot(pul_biom_data_1) +
    geom_point(data = CG_pul_cover_biomass, aes(x = Sample_age, y = log(biomass_per_m2)),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Pulchra  biomass (log g/m2)\n") +
    xlab("\n Sample age" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# Salix arctica ------
arc_biom_1 <- (conditional_effects(biom_arc))
arc_biom_data_1 <- arc_biom_1[[1]]

(arc_biom_plot <-ggplot(arc_biom_data_1) +
    geom_point(data = CG_arc_cover_biomass, aes(x = Sample_age, y = log(biomass_per_m2)),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Arctica cover  biomass (log g/m2)\n") +
    xlab("\n Sample age" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# random slope model plot ----
(biomass_random_slopes_plot <- CG_ALL_cover_biomass %>%
   bind_cols(as_tibble(fitted(biom_random_slopes))) %>%
   group_by(Species) %>%
   ggplot() +
   geom_point(aes(x = Sample_age, y = (log(biomass_per_m2)))) +
   geom_point(aes(x = Sample_age, y = Estimate), shape = 1, size = 4, stroke = 1.5) +
   geom_smooth(aes(x = Sample_age, y = (log(biomass_per_m2)), colour = Species, fill = Species)) +
   labs(x = "Sample age", y = "Shrub biomass (log g/m2)",
        subtitle = "Black points are observed values. Black circles are fitted values.") +
   # scale_x_continuous(expand = c(.075, .075), breaks = 0:3) +
   #facet_wrap(~Species, nrow = 1) +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +   
   theme_shrub() +
   theme(plot.title = element_text(hjust = .5)))


# panels -----
CG_cov_panel <- grid.arrange(rich_cover_plot,
                             pul_cover_plot, 
                             arc_cover_plot, nrow = 1)

CG_biom_panel <- grid.arrange(rich_biom_plot,
                             pul_biom_plot, 
                             arc_biom_plot, nrow = 1)



