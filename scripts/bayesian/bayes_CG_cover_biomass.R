# BAYESIAN: CG cover and biomass calculation and visualisation script
#### Script by Erica Zaja, created 23/02/23
### Last updated: 23/02/23


# LIBRARIES -----
library(tidyverse)
library(brms)

# DATA -----
all_CG_growth_cover <- read_csv("data/all_CG_growth_cover.csv")

# 1. CG BIOMASS derivation from bayesian allom. equations ------
# calculate biomass for each species based on BAYESIAN allometric equations

# Salix richardsonii ------
# Equation: Biomass =  (18.05*height +- 5.11) + (11.55 *cover +-  18.07)
CG_ric_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix richardsonii" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (18.05*Canopy_Height_cm) + (11.55*cover_percent), 
         biomass_error_high = biomass_per_m2 + 5.11 + 18.07,
         biomass_error_low = biomass_per_m2 - 5.11 - 18.07)

range(CG_ric_cover_biomass$biomass_per_m2) # 142.8825 2699.6420
write.csv(CG_ric_cover_biomass, "data/common_garden_shrub_data/CG_ric_cover_biomass.csv")

# Salix pulchra ------
# Equation: Biomass =  (1.08*height +-  5.17 ) + (18.16 *cover +-  8.42)
CG_pul_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix pulchra" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (1.08*Canopy_Height_cm) + (18.16*cover_percent), 
         biomass_error_high = biomass_per_m2 + 5.17 + 8.42,
         biomass_error_low = biomass_per_m2 - 5.17 - 8.42)

range(CG_pul_cover_biomass$biomass_per_m2) # 4.017768 1847.963440
write.csv(CG_pul_cover_biomass, "data/common_garden_shrub_data/CG_pul_cover_biomass.csv")

# Salix arctica -----
# Biomass =  ( 1.51 *height +-  22.32) + (14.87 *cover +-  19.22)
CG_arc_cover_biomass <- all_CG_growth_cover %>%
  filter(Species == "Salix arctica" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (1.51*Canopy_Height_cm) + (14.87*cover_percent), 
         biomass_error_high = biomass_per_m2 + 22.32 + 19.22,
         biomass_error_low = biomass_per_m2 - 22.32 - 19.22)

range(CG_arc_cover_biomass$biomass_per_m2) # 9.60174 358.80240
write.csv(CG_arc_cover_biomass, "data/common_garden_shrub_data/CG_arc_cover_biomass.csv")

# 2. EXPLORE ------
hist(CG_arc_cover_biomass$cover_percent, breaks = 30) # not normal
hist(CG_pul_cover_biomass$cover_percent, breaks = 30)# not normal
hist(CG_ric_cover_biomass$cover_percent, breaks = 30)# not normal

hist(CG_arc_cover_biomass$biomass_per_m2, breaks = 30) # not normal
hist(CG_pul_cover_biomass$biomass_per_m2, breaks = 30)# not normal
hist(CG_ric_cover_biomass$biomass_per_m2, breaks = 30) # almost normal

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

# 4. DATA VISUALISATION ---------
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

# panels -----
CG_cov_panel <- grid.arrange(rich_cover_plot,
                             pul_cover_plot, 
                             arc_cover_plot, nrow = 1)

CG_biom_panel <- grid.arrange(rich_biom_plot,
                             pul_biom_plot, 
                             arc_biom_plot, nrow = 1)


