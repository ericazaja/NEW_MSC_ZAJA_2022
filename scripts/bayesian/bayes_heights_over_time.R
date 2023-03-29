# Script to plot QHI monitoring heights over time -----

# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# libraries
library(tidyverse)
library(brms)

# Data-----
# data 1999-2019
QHI_1999_2019 <- read_csv("data/ITEX/pointfr_1999-2019_sas.csv")
# 2022 data
QHI_2022 <- read_excel("data/ITEX/qhi_Point_Framing_Longform_2022_inprogress.xlsx")

# Wrangle ------
QHI_2022_salix <- QHI_2022 %>%
  filter(SPP == "Salix pulchra") %>%
        # TISSUE == "Live")%>%
  rename("Height_cm"="Height")%>%
  dplyr::select(SITE, SUBSITE, PLOT, YEAR, PlotN, SPP, TISSUE, STATUS, Height_cm)

QHI_1999_2019_salix <- QHI_1999_2019 %>%
  filter(SPP == "Salix pulchra")%>%
  # TISSUE == "Live")%>% 
  mutate(Height_cm = Height..cm./100) %>%
  dplyr::select(SITE, SUBSITE, PLOT, YEAR, SPP, TISSUE, STATUS, Height_cm)%>%
  mutate(PlotN = "HE5")

# merge 
QHI_1999_2022 <- rbind(QHI_1999_2019_salix, QHI_2022_salix)

# Model QHI pulchra heights over time-----
unique(QHI_1999_2022$YEAR)
QHI_1999_2022 <-QHI_1999_2022 %>%
  mutate(Year_index = I(YEAR - 2014))

QHI_1999_2022$height_scale <- center_scale(QHI_1999_2022$Height_cm)
QHI_1999_2022$PlotN <- as.factor(QHI_1999_2022$PlotN)
unique(QHI_1999_2022$PlotN)

QHI_height_time <- brms::brm( height_scale ~ Year_index + (Year_index|PlotN),
                               data = QHI_1999_2022,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(QHI_height_time)
pp_check(QHI_height_time, type = "dens_overlay", ndraws = 100) 

(pulchra_height_plot <- QHI_1999_2022 %>%
    add_predicted_draws(QHI_height_time) %>%
    ggplot(aes(x = Year_index, y = height_scale)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = QHI_1999_2022) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm, scaled) \n") +
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
temp_time_QHI <- temp_time_random_year %>%
  filter(Site =="QHI")

QHI_temp_height_pul <- full_join(temp_time_QHI, QHI_height_random_year, by = c("Site"="Site"))

view(QHI_temp_height_pul)

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

