# Script to plot QHI monitoring heights over time -----

# libraries
library(tidyverse)
library(brms)
library(readxl)

# scale function =====
# centering with 'scale()'
#center_scale <- function(x) {
 # scale(x, scale = FALSE)
#}


# LOAD DATA -----
# data 1999-2019
QHI_1999_2022 <- read_csv("data/ITEX/pointfr_1999_2022_clean.csv")

# Something is wrong here - check with Mariana
test <- QHI_1999_2022 %>% 
  mutate(Year_index = I(YEAR - 1998)) %>%
  group_by(SUBSITE, PLOT, YEAR) %>%
  summarise(HeightMax = max(Height))

# DATA WRANGLE ------
# make a subsite plot year x y column 
QHI_2022 <- QHI_1999_2022 %>% select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height) %>% filter(SPP == "Salix pulchra" & STATUS == "LIVE")

QHI_2022_sum_max <- QHI_2022 %>% 
  group_by(SUBSITE, PLOT, YEAR, SPP) %>%
  summarise(HeightMax = max(Height)) %>%
  mutate(Height_mm = case_when(HeightMax >= 100 ~ "TRUE", HeightMax < 100 ~ "FALSE")) %>%
  select(-HeightMax)

QHI_2022_cm <- QHI_2022 %>% 
  left_join(QHI_2022_sum_max) %>% 
  mutate(Height_cm = case_when(Height_mm == "TRUE" ~ Height/10, Height_mm < 100 ~ Height)) %>% 
  # filter(Height_cm > 10) %>% # This will remove things like roots and basal stems
  select(-Height) %>%
  na.omit(Height_cm)

# keeping only max values
QHI_2022_max <- QHI_2022_cm %>%
  group_by(SUBSITE, PLOT, YEAR, SPP, X, Y) %>%
  summarise(max_heights_cm = max(Height_cm)) %>%
  distinct() %>% # keeping one unique value
  na.omit(max_heights_cm)

range(QHI_2022_max$max_heights_cm) # 0.2 39.3 cm (this is what you had before: 10.9 41.0 cm)

# quick plot
# raw data 
(pulchra_height_plot <- QHI_2022_max %>%
    ggplot(aes(x = YEAR, y = max_heights_cm)) +
    geom_point(data = QHI_2022_max) +
    geom_smooth(method = "lm") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm) \n") +
    xlab("\nYear (scaled)"))

# + theme_shrub())

# Isla has stopped at this point, below this point needs editing to run properly

# MODELLING------
# Model QHI pulchra heights over time-----

range(QHI_2022_max$max_heights_cm) #  0.0 39.3
hist(QHI_2022_max$max_heights_cm)

QHI_2022_max$height_scale <- center_scale(QHI_2022_max$max_heights_cm)
QHI_2022_max$SubsitePlot <- with(QHI_2022_max, paste0(SUBSITE, PLOT))
QHI_2022_max <- QHI_2022_max %>% 
  mutate(Year_index = I(YEAR - 1998)) 

QHI_2022_max$SubsitePlot <- as.factor(QHI_2022_max$SubsitePlot)
unique(QHI_2022_max$SubsitePlot)

QHI_height_time <- brms::brm(max_heights_cm ~ Year_index + (Year_index|SubsitePlot),
                               data = QHI_2022_max,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(QHI_height_time)
pp_check(QHI_height_time, type = "dens_overlay", ndraws = 100) 

(pulchra_height_plot <- QHI_2022_max %>%
    group_by(SubsitePlot)%>%
    add_predicted_draws(QHI_height_time) %>%
    ggplot(aes(x = Year_index, y = max_heights_cm)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = QHI_2022_max) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm) \n") +
    xlab("\nYear (scaled)") +
    theme_shrub())

QHI_height_random <- as.data.frame(ranef(QHI_height_time)) # extract random eff. slopes 
QHI_height_random$Plot <- row.names(QHI_height_random) # Apply row.names function
rownames(QHI_height_random) <- NULL
colnames(QHI_height_random)[5] <- "plot_height_index_year_estimate" 
colnames(QHI_height_random)[6] <- "plot_height_index_year_error" 
colnames(QHI_height_random)[7] <- "plot_height_index_year_Q_25" 
colnames(QHI_height_random)[8] <- "plot_height_index_year_Q_97"
view(QHI_height_random)

QHI_height_random_year <- QHI_height_random %>%
  dplyr::select("Plot","plot_height_index_year_estimate" ,"plot_height_index_year_error", "plot_height_index_year_Q_25",
                "plot_height_index_year_Q_97")%>%
  mutate(Site = rep("QHI"))
view(QHI_height_random_year)

# Slope vs slope (height change vs temp change)-----
#temp_time_QHI <- temp_time_random_year %>%
 # filter(Site =="QHI")

#QHI_temp_height_pul <- full_join(temp_time_QHI, QHI_height_random_year, by = c("Site"="Site"))

#view(QHI_temp_height_pul)

# this doesnt work! makes no sense? 
temp_time <- brms::brm(plot_height_index_year_estimate ~ index_year_estimate,
                       data = QHI_temp_height_pul,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(temp_time)
pp_check(temp_time, type = "dens_overlay", nsamples = 100) 

(temp_time_plot <- QHI_temp_height_pul %>%
    add_predicted_draws(temp_time) %>%
    ggplot(aes(x =index_year_estimate, y =plot_height_index_year_estimate)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = QHI_temp_height_pul) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Height change (cm, scaled) \n") +
    xlab("\nTemperature change (scaled)")+ theme_shrub() +
    labs(title = "Salix pulchra"))



