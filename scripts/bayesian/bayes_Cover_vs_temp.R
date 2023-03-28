# Cover vs mean july temperature
#### Script by Erica Zaja, created 23/02/2023
### Last updated: 01/02/23

# 1. LIBRARIES -----
library(tidyverse)
library(brms)

# REGRESSION yearly temp VS yearly cover change

# 2. DATA ------
all_cover_temps_long <- read_csv("data/all_cover_temps_long.csv")
all_cover_temps <- read_csv("data/all_cover_temps.csv")
all_cover_temps_new <- read_csv("data/all_cover_temps_new.csv")


# 3. WRANGLE -----
# sp - specific datasets

all_cover_temps_long_pulchra <- all_cover_temps_new %>%
  filter(Species == "Salix pulchra")

all_cover_temps_long_rich <- all_cover_temps_new %>%
  filter(Species == "Salix richardsonii") 

all_cover_temps_long_arctica <- all_cover_temps_new %>%
  filter(Species == "Salix arctica")

# big means 
all_cover_temps_means_pulchra <- all_cover_temps %>%
  filter(Species == "Salix pulchra")

all_cover_temps_means_rich <- all_cover_temps %>%
  filter(Species == "Salix richardsonii")

all_cover_temps_means_arc <- all_cover_temps %>%
  filter(Species == "Salix arctica")

# explore 
hist(all_cover_temps_long_arctica$cover_change_percent,  breaks = 30) # left skew
hist(all_cover_temps_long_pulchra$cover_change_percent, breaks = 30)# left skew
hist(all_cover_temps_long_rich$cover_change_percent, breaks = 30)# left skew

# trying to center cover change
all_cover_temps_long_rich$cover_change_percent <- scale(all_cover_temps_long_rich$cover_change_percent, center = TRUE, scale = TRUE)
all_cover_temps_long_pulchra$cover_change_percent <- scale(all_cover_temps_long_pulchra$cover_change_percent, center = TRUE, scale = TRUE)
all_cover_temps_long_arctica$cover_change_percent <- scale(all_cover_temps_long_arctica$cover_change_percent, center = TRUE, scale = TRUE)

# 4. MODELLING ------
# Salix richardsonii -----
# cover change per unit temp 
cover_temp_rich <- brms::brm(cover_change_percent ~ mean_temp_C + (1|Site) + (1|Year),
                        data = all_cover_temps_long_rich,  family = skew_normal(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_rich) # not significant cover change per unit temp
plot(cover_temp_rich)
pp_check(cover_temp_rich, type = "dens_overlay", nsamples = 100) 

# cover change per unit temp 
cover_temp_rich_means <- brms::brm(mean_cover_change ~ mean_temp,
                             data = all_cover_temps_means_rich,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_rich_means) # 14.20 
plot(cover_temp_rich_means)
pp_check(cover_temp_rich_means, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -----
# cover change per unit temp 
cover_temp_pul <- brms::brm(cover_change_percent ~ mean_temp_C + (1|Site) + (1|Year),
                             data = all_cover_temps_long_pulchra,  family = skew_normal(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_pul) # not significant cover change per unit temp
plot(cover_temp_pul)
pp_check(cover_temp_pul, type = "dens_overlay", nsamples = 100) 

# cover change per unit temp 
cover_temp_pul_means <- brms::brm(mean_cover_change ~ mean_temp_C,
                                   data = all_cover_temps_means_pulchra,  family = gaussian(), chains = 3,
                                   iter = 5000, warmup = 1000, 
                                   control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_pul_means) # 2.25 %
plot(cover_temp_pul_means)
pp_check(cover_temp_pul_means, type = "dens_overlay", nsamples = 100) 

# Salix arctica -----
# cover change per unit temp 
cover_temp_arc <- brms::brm(cover_change_percent ~ mean_temp_C + (1|Site) + (1|Year)
                            data = all_cover_temps_long_arctica,  family = skew_normal(), chains = 3,
                            iter = 5000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_arc) # not significant cover change per unit temp
plot(cover_temp_arc)
pp_check(cover_temp_arc, type = "dens_overlay", nsamples = 100) 

# cover change per unit temp 
cover_temp_arc_means <- brms::brm(mean_cover_change ~ mean_temp,
                                  data = all_cover_temps_means_arc,  family = gaussian(), chains = 3,
                                  iter = 5000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_temp_arc_means) # 10.71  %
plot(cover_temp_arc_means)
pp_check(cover_temp_arc_means, type = "dens_overlay", nsamples = 100) 

# DATA VISUALISATION ------
# Salix richardsonii ------
rich_covchange <- (conditional_effects(cover_temp_rich))
rich_covchange_data <- rich_covchange[[1]]

(rich_cover_plot <-ggplot(rich_covchange_data) +
    geom_point(data = all_cover_temps_long_rich, aes(x = mean_temp_C, y = cover_change_percent, colour = Site),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
  #  geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
    #            alpha = .1) +
    ylab("Richardsonii cover change (centred)\n") +
    xlab("\n Mean july temperature (degC)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub())

# Salix pulchra ------
pul_covchange <- (conditional_effects(cover_temp_pul))
pul_covchange_data <- pul_covchange[[1]]

(pul_cover_plot <-ggplot(pul_covchange_data) +
    geom_point(data = all_cover_temps_long_pulchra, aes(x = mean_temp, y = cover_change_percent, colour = Site),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    #  geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
    #            alpha = .1) +
    ylab("Pulchra cover change (centred)\n") +
    xlab("\n Mean july temperature (degC)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub())

# Salix arctica ------
arc_covchange <- (conditional_effects(cover_temp_arc))
arc_covchange_data <- arc_covchange[[1]]

(arc_cover_plot <-ggplot(pul_covchange_data) +
    geom_point(data = all_cover_temps_long_arctica, aes(x = mean_temp, y = cover_change_percent, colour = Site),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    #  geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
    #            alpha = .1) +
    ylab("Arctica cover change (centred)\n") +
    xlab("\n Mean july temperature (degC)" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub())

# POST MEETING ------
# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# data 
july_enviro_chelsa <- read_csv("data/july_enviro_chelsa.csv") # chelsa temperature and precipitation data 
ITEX_shrubs_msc <- read_csv("data/ITEX/itex_EZ_shrubs_2023.csv") # newest 
all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")

# 1. temp over time
#rename column
july_enviro_chelsa <- july_enviro_chelsa %>%
  rename ("mean_temp_C" ="(mean_temp_C = (mean_temp/10 - 273.15))")

july_enviro_chelsa <- july_enviro_chelsa %>%
  mutate(Site = case_when(site == "ATIGUN" ~ "ANWR",
                          site %in% c("IMNAVAIT", "TUSSOKGRID") ~ "TOOLIK", 
                          site == "QHI" ~ "QHI",
                          site == "Common_garden" ~ "CG",
                          site == "Kluane_plateau" ~ "KP"))


july_enviro_chelsa$Site <- as.factor(july_enviro_chelsa$Site)

july_enviro_chelsa$mean_temp_C_scaled <- center_scale(july_enviro_chelsa$mean_temp_C)
hist(july_enviro_chelsa$mean_temp_C_scaled)

july_enviro_chelsa <-july_enviro_chelsa %>%
  mutate(index_year = I(year-1998))

# temp over time (random intercept random slope)
temp_time <- brms::brm(mean_temp_C_scaled ~ index_year + (index_year|Site),
                            data = july_enviro_chelsa,  family = gaussian(), chains = 3,
                            iter = 5000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(temp_time)

(temp_time_plot <- july_enviro_chelsa %>%
    group_by(Site) %>%
    add_predicted_draws(temp_time) %>%
    ggplot(aes(x =index_year, y =mean_temp_C_scaled , color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = july_enviro_chelsa) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("July temperature (degC, scaled) \n") +
    xlab("\nYear (scaled)")+ theme_shrub())
  
# I think I only need the index_year estimates
temp_time_random <- as.data.frame(ranef(temp_time)) # extract random eff. slopes 
temp_time_random$Site <- row.names(temp_time_random) # Apply row.names function
rownames(temp_time_random) <- NULL
colnames(temp_time_random)[5] <- "index_year_estimate" 
colnames(temp_time_random)[6] <- "index_year_error" 
colnames(temp_time_random)[7] <- "index_year_Q_25" 
colnames(temp_time_random)[8] <- "index_year_Q_97"
view(temp_time_random)

temp_time_random_year <- temp_time_random %>%
  dplyr::select("Site","index_year_estimate" ,"index_year_error", "index_year_Q_25",
                "index_year_Q_97")
view(temp_time_random_year)

# SLOPE vs slopes -----
# S. Pulchra slope vs slope -----
temp_time_pul <- temp_time_random_year %>%
  filter(Site %in% c("QHI", "TOOLIK"))

temp_cover_pul <- full_join(temp_time_pul, cov_time_pul_random_new, by = c("Site"="Site"))

view(temp_cover_pul)

temp_time <- brms::brm(sitesubsiteplot_index_year_estimate ~ index_year_estimate + (1|Site),
                       data = temp_cover_pul,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(temp_time)
pp_check(temp_time, type = "dens_overlay", nsamples = 100) 

(temp_time_plot <- temp_cover_pul %>%
    group_by(Site) %>%
    add_predicted_draws(temp_time) %>%
    ggplot(aes(x =index_year_estimate, y =sitesubsiteplot_index_year_estimate , color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = temp_cover_pul) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Cover change (scaled) \n") +
    xlab("\nTemperature change (scaled)")+ theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica slope vs slope -----
temp_time_arc <- temp_time_random_year %>%
  filter(Site %in% c("QHI", "ANWR"))

temp_cover_arc <- full_join(temp_time_arc, cov_time_arc_random_new, by = c("Site"="Site"))

view(temp_cover_arc)

temp_time_arc_mod <- brms::brm(sitesubsiteplot_index_year_estimate ~ index_year_estimate + (1|Site),
                       data = temp_cover_arc,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(temp_time_arc_mod)
pp_check(temp_time_arc_mod, type = "dens_overlay", nsamples = 100) 

(temp_time_arc_plot <- temp_cover_arc %>%
    group_by(Site) %>%
    add_predicted_draws(temp_time_arc_mod) %>%
    ggplot(aes(x =index_year_estimate, y =sitesubsiteplot_index_year_estimate , color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = temp_cover_arc) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Cover change (scaled) \n") +
    xlab("\nTemperature change (scaled)")+ theme_shrub() +
    labs(title = "Salix arctica"))


# IGNORE BELOW FOR NOW -----
# 2. cover over time only CG 
# calculate cover based on widths for all CG and source pop shurbs
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)

all_CG_growth_cover_southern <- all_CG_source_growth %>%
  filter(population != "Northern") # removing northern common garden pop

all_CG_growth_cover_southern$population <- as.character(all_CG_growth_cover_southern$population)
all_CG_growth_cover_southern$population <- as.factor(all_CG_growth_cover_southern$population)

unique(all_CG_growth_cover_southern$population) #Southern     source_south source_north
unique(all_CG_growth_cover_southern$Site) # [1] "Common_garden" "Kluane"        "Qikiqtaruk"   

# calculate percentage cover of all species in CG, Kluane and QHI
all_CG_growth_cover <- all_CG_growth_cover_southern %>%
  mutate(cover = (Width_cm*Width_2_cm)/10000)%>%
  mutate(cover_percent = cover *100) %>%
  filter(cover_percent <=100) # setting max to 100% cover 

all_CG_source_growth_cover <- all_CG_growth_cover %>%
  dplyr::select(Species, SampleID_standard, population, Sample_age, cover_percent) %>%
  mutate(Site = case_when(population =="Southern" ~ "CG",
                          population == "source_south" ~ "KP", 
                          population == "source_north" ~ "QHI"))%>%
  rename("Plot" = "SampleID_standard", "Year"="Sample_age") %>%
  dplyr::select(-population)%>%
  na.omit()


ITEX_shrubs_cover <- ITEX_shrubs_msc %>%
  filter(SurveyedArea %in% c(1.0000, 1))%>% # keeping only 1m2 quadrat
  # filter(SITE!= "QHI") %>%
  dplyr::select(YEAR, PLOT, SPECIES_NAME, SITE, RelCover) %>%
  rename("Year"="YEAR", "Plot"= "PLOT", "Species" = "SPECIES_NAME", "Site"= "SITE", "cover_percent"= "RelCover") %>%
  na.omit()


all_cover_long <- rbind(ITEX_shrubs_cover, all_CG_source_growth_cover)
all_cover_long <- all_cover_long %>%
  mutate(cover_prop = cover_percent/100)%>%
  filter(Site == "CG")

all_cover_long$Site <- as.factor(all_cover_long$Site)                    
all_cover_long$Species <- as.factor(all_cover_long$Species)
all_cover_long$Plot <- as.factor(all_cover_long$Plot)

hist(all_cover_long$cover_prop)

all_cover_long_rich <- all_cover_long %>%
  filter(Species == "Salix richardsonii") # no richardsonii in 3 sites 
all_cover_long_pul <- all_cover_long %>%
  filter(Species == "Salix pulchra")
all_cover_long_arc <- all_cover_long %>%
  filter(Species == "Salix arctica")

# richardsonii only in CG
cov_time_rich <- brms::brm(cover_prop ~ Year + (1|Plot),
                       data = all_cover_long_rich,  family = "beta", chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cov_time_rich)

(cov_time_plot <- all_cover_long_rich %>%
    add_predicted_draws(cov_time_rich) %>%
    ggplot(aes(x = Year, y =cover_prop, colour= Site, fill =Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = all_cover_long_rich) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Cover (proportion) \n") +
    xlab("\nYear (scaled)")+ theme_shrub()+
    labs(title = "Salix richardsonii in CG"))

cov_time_CG_rich <- as.data.frame(ranef(cov_time_rich)) # extract random eff. slopes 







