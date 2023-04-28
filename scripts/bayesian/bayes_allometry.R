# BAYESIAN allometry -----

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(brms)
library(gridExtra)

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

hist(Pika_all_shrub_biomass$Shrub_Height_cm, breaks = 7) # weird, not much data
hist(QHI_salric_shrub_biomass$max_height, breaks = 7) # weird, not much data
hist(QHI_salarc_shrub_biomass$max_height, breaks = 7) # weird, not much data

hist(Pika_all_shrub_biomass$max_cover, breaks = 7) # weird, not much data
hist(QHI_salric_shrub_biomass$percent_cover, breaks = 7) # weird, not much data
hist(QHI_salarc_shrub_biomass$percent_cover, breaks = 7) # weird, not much data

# removing an outlier from Pika data
Pika_all_shrub_biomass_edit <- Pika_all_shrub_biomass %>%
  group_by(Plot) %>%
  dplyr::distinct(Plot, Shrub_Height_cm, max_biomass, max_cover, Species, 
                  biomass_per_m2)

Pika_all_shrub_biomass_edit_2 <- Pika_all_shrub_biomass_edit[-1,] # plot 2a

# 3.1. Salix richardsonii ------

rich_allom <- brms::brm(biomass_per_m2 ~ 0 + max_height + percent_cover,
                                 data = QHI_salric_shrub_biomass, family = gaussian(), chains = 3,
                                 iter = 5000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(rich_allom) # not significant 
plot(rich_allom) # great
pp_check(rich_allom, type = "dens_overlay", nsamples = 100) # fine
# biomass increases with height

# running the model 6 times, recording the estimates and errors each time, 
# making a mean and rounding to one decimal
heights_rich <- c(18.00, 17.96,  17.94, 17.99, 18.02, 17.89) 
heights_errors_rich <- c( 5.09 ,5.25, 5.05, 5.09, 5.08, 5.25 )
mean(heights_rich)# 17.96667
round(17.96667, digits = 1) # 18.0
mean(heights_errors_rich)# 5.135
round(5.135, digits = 1) # 5.1
covers_rich <- c(11.76, 11.96, 11.93, 11.80, 11.73, 12.18)
covers_errors_rich <- c(17.91, 18.43, 17.75, 17.95, 17.72, 18.29)
mean(covers_rich) 
round(11.89333, digits = 1) # 11.9
mean(covers_errors_rich) 
round(18.00833, digits = 1) # 18.0
# FINAL EQUATION: Biomass =  (18.0*height +- 5.1) + (11.9 *cover +-  18.0)

# 3.2. Salix pulchra -------
pul_allom <- brms::brm(biomass_per_m2 ~ 0 + Shrub_Height_cm + max_cover,
                        data = Pika_all_shrub_biomass_edit_2, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(pul_allom) # not significant 
plot(pul_allom) # great
pp_check(pul_allom, type = "dens_overlay", nsamples = 100) # meh

# running the model 6 times, recording the estimates and errors each time, 
# making a mean and rounding to one decimal
heights_pul <- c(0.91, 1.14, 1.04, 1.05, 1.46, 1.21) 
heights_errors_pul <- c( 5.07, 5.01, 5.17,  4.91, 5.14, 4.95  )
mean(heights_pul)# 1.135
round(1.135, digits = 1) # 1.1
mean(heights_errors_pul)#  5.041667
round( 5.041667, digits = 1) # 5.0
covers_pul <- c(18.46, 18.07, 18.26,  18.26,  17.58, 17.98  )
covers_errors_pul <- c(8.19, 8.14,8.38, 7.96, 8.31, 8.03)
mean(covers_pul) 
round(18.10167, digits = 1) # 18.1
mean(covers_errors_pul) 
round(8.168333, digits = 1) # 8.2
# FINAL EQUATION:  Biomass =  (1.1*height +-  5.0 ) + (18.1 *cover +-  8.2)


# 3.3. Salix arctica -------
arc_allom <- brms::brm(biomass_per_m2 ~ 0 + max_height + percent_cover,
                       data = QHI_salarc_shrub_biomass, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
# Adding the 0 term tells the model to fit the line through the origin

summary(arc_allom) # not significant 
plot(arc_allom) # great
pp_check(arc_allom, type = "dens_overlay", nsamples = 100) # meh

# running the model 6 times, recording the estimates and errors each time, 
# making a mean and rounding to one decimal
heights_arc <- c(2.67,  2.33, 2.13,2.00, 1.72, 2.64) 
heights_errors_arc  <- c(  22.74, 23.38, 23.19, 26.92,23.28, 24.61 )
mean(heights_arc)# 2.248333
round(2.248333, digits = 1) # 2.2
mean(heights_errors_arc)
round( 24.02, digits = 1) # 24.0
covers_arc  <- c(13.87, 14.11,14.29, 14.45, 14.65, 13.88)
covers_errors_arc  <- c(19.63, 20.16, 19.95, 23.30, 20.05, 21.19)
mean(covers_arc) 
round(14.20833, digits = 1) # 14.2
mean(covers_errors_arc) 
round(20.71333, digits = 1) # 20.7
# FINAL EQUATION:  # Equation: Biomass =  ( 2.2 *height +-  24.0) + (14.2 *cover +-  20.7)


# 4.DATA VISUALISATION------
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(size=16),
                                 axis.text.x  = element_text(vjust=0.5, size=14, colour = "black"), 
                                 axis.title.y = element_text(size=16),
                                 axis.text.y  = element_text(vjust=0.5, size=14, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}


# 4.1. Salix richardsonii -------
rich <- (conditional_effects(rich_allom))
rich_data <- rich[[1]]
rich_data_2 <- rich[[2]]

(rich_height_allom <-ggplot(rich_data) +
    geom_point(data = QHI_salric_shrub_biomass, aes(x = max_height, y = biomass_per_m2),
               alpha = 0.5, colour = "#2e8b57")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#2e8b57") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#2e8b57") +
    labs(y = expression("Shrub biomass" ~ g/m^2)) +
    ggtitle(expression(italic("Salix richardsonii"))) +
    xlab("Shrub height (cm)" ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

(rich_cov_allom <-ggplot(rich_data_2) +
    geom_point(data = QHI_salric_shrub_biomass, aes(x = percent_cover, y = biomass_per_m2),
               alpha = 0.5, colour = "#2e8b57")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#2e8b57") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#2e8b57") +
   # ggtitle(expression(italic("Salix richardsonii"))) +
    labs(y = expression("Shrub biomass" ~ g/m^2), 
         x = expression("Cover %/"~m^2)) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# 4.2. Salix pulchra -------
pul <- (conditional_effects(pul_allom))
pul_data <- pul[[1]]
pul_data_2 <- pul[[2]]

(pul_height_allom <-ggplot(pul_data) +
    geom_point(data = Pika_all_shrub_biomass_edit_2, aes(x = Shrub_Height_cm, y = biomass_per_m2),
               alpha = 0.5, colour = "#8a2e61")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#8a2e61") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#8a2e61") +
    ggtitle(expression(italic("Salix pulchra"))) +
    # labs(y = expression("Shrub biomass" ~ g/m^2)) +
    xlab("Shrub height (cm)" ) +
    labs(y = "")+
    #ylim(0, 4000 ) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

(pul_cov_allom <-ggplot(pul_data_2) +
    geom_point(data = Pika_all_shrub_biomass_edit_2, aes(x = max_cover, y = biomass_per_m2),
               alpha = 0.5, colour = "#8a2e61")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#8a2e61") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#8a2e61") +
    #ggtitle(expression(italic("Salix pulchra"))) +
    #labs(y = expression("Shrub biomass" ~ g/m^2)) +
    labs(x = expression("Cover %/"~m^2), 
         y = "") +
   #ylim(0, 1500) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# 4.2. Salix arctica -------
arc <- (conditional_effects(arc_allom))
arc_data <- arc[[1]]
arc_data_2 <- arc[[2]]

(arc_height_allom <-ggplot(arc_data) +
    geom_point(data = QHI_salarc_shrub_biomass, aes(x = max_height, y = biomass_per_m2),
               alpha = 0.5, colour = "#8a852e")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#8a852e") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#8a852e") +
    ggtitle(expression(italic("Salix arctica"))) +
    #labs(y = expression("Shrub biomass" ~ g/m^2)) +
    xlab("Shrub height (cm)" ) +
    labs(y = "")+
    #ylim(0, 300) +
    #scale_color_brewer(palette = "Greys")+
    #scale_fill_brewer(palette = "Greys")+
    theme_shrub())

(arc_cov_allom <-ggplot(arc_data_2) +
    geom_point(data = QHI_salarc_shrub_biomass, aes(x = percent_cover, y = biomass_per_m2),
               alpha = 0.5, colour = "#8a852e")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour =  "#8a852e") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1,fill = "#8a852e") +
   # labs(y = expression("Shrub biomass" ~ g/m^2)) +
      labs( x = expression("Cover %/"~m^2), 
            y = "")+
   # ylim(0, 250) +
    #ggtitle(expression(italic("Salix arctica"))) +
    #scale_color_brewer(palette = "Greys")+
    #scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# panels ------
height_all_allom <- grid.arrange(rich_height_allom,
                                 pul_height_allom,
                                 arc_height_allom, nrow = 1)

cov_all_allom <- grid.arrange(rich_cov_allom,
                                 pul_cov_allom,
                                 arc_cov_allom, nrow = 1)

all <- grid.arrange(height_all_allom, cov_all_allom)

rich_allom <- grid.arrange(rich_height_allom,rich_cov_allom, rich_height_cov_plot)
pulchra_allom <- grid.arrange(pul_height_allom,pul_cov_allom, pul_height_cov_plot)
arctic_allom <- grid.arrange(arc_height_allom, arc_cov_allom, arc_height_cov_plot)

all_allom <- grid.arrange(rich_allom,pulchra_allom,arctic_allom, ncol=3)

# HEIGHT vs COVER ------
# Sal. rich. -----
rich_height_cov <- brms::brm(max_height |trunc(lb = 0, ub = 450) ~ percent_cover,
                        data = QHI_salric_shrub_biomass, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_height_cov)
pp_check(rich_height_cov, type = "dens_overlay", nsamples = 100) # meh


# Sal.pul ----
pul_height_cov <- brms::brm(Shrub_Height_cm|trunc(lb = 0, ub = 160) ~ max_cover,
                       data = Pika_all_shrub_biomass_edit_2, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(pul_height_cov)
pp_check(pul_height_cov, type = "dens_overlay", nsamples = 100) # meh

# Sal. arc -----
arc_height_cov <- brms::brm(max_height|trunc(lb = 0, ub = 23) ~ percent_cover ,
                       data = QHI_salarc_shrub_biomass, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(arc_height_cov)
pp_check(arc_height_cov, type = "dens_overlay", nsamples = 100) # meh

# Data vis
rich_height_cov1 <- (conditional_effects(rich_height_cov))
rich_height_cov2 <- rich_height_cov1[[1]]

(rich_height_cov_plot <-ggplot(rich_height_cov2) +
    geom_point(data = QHI_salric_shrub_biomass, aes(x = percent_cover, y = max_height),
               alpha = 0.5, colour = "#2e8b57")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#2e8b57") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#2e8b57") +
    #ggtitle(expression(italic("Salix richardsonii"))) +
    ylab("Shrub height (cm)\n") +
    labs(x = expression("Cover %/"~m^2)) + 
    theme_shrub())

pul_height_cov1 <- (conditional_effects(pul_height_cov))
pul_height_cov2 <- pul_height_cov1[[1]]

(pul_height_cov_plot <-ggplot(pul_height_cov2) +
    geom_point(data = Pika_all_shrub_biomass_edit_2, aes(x = max_cover, y = Shrub_Height_cm),
               alpha = 0.5, colour = "#8a2e61")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#8a2e61") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#8a2e61") +
   # ggtitle(expression(italic("Salix pulchra"))) +
    ylab("Shrub height (cm)\n") +
    labs(x = expression("Cover %/"~m^2)) + 
    #ylim(0, 4000 ) +
    theme_shrub())

arc_height_cov1 <- (conditional_effects(arc_height_cov))
arc_height_cov2 <- arc_height_cov1[[1]]

(arc_height_cov_plot <-ggplot(arc_height_cov2) +
    geom_point(data = QHI_salarc_shrub_biomass, aes(x = percent_cover, y = max_height),
               alpha = 0.5, colour = "#8a852e")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#8a852e") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#8a852e") +
    # ggtitle(expression(italic("Salix arctica"))) +
    ylab("Shrub height (cm)\n") +
    labs(x = expression("Cover %/"~m^2)) + 
    #ylim(0, 4000 ) +
    theme_shrub())

heightcov_all <- grid.arrange(rich_height_cov_plot,
                              pul_height_cov_plot,
                              arc_height_cov_plot, nrow = 1)

