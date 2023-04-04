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
# QHI_1999_2019 <- read_csv("data/ITEX/pointfr_1999-2019_sas.csv")
# 2022 data
# QHI_2022 <- read_excel("data/ITEX/qhi_Point_Framing_Longform_2022_inprogress.xlsx")

# NEW QHI data
QHI_new <- read_csv("data/ITEX/pointfr_1999_2022_clean.csv")
view(QHI_new)

# DATA WRANGLE ------
# make a subsite plot year x y column 
#QHI_2022$SubsitePlotYearXY<- with(QHI_2022, paste0(SUBSITE, PLOT, YEAR, X, Y))
#QHI_1999_2019$SubsitePlotYearXY<- with(QHI_1999_2019, paste0(SUBSITE, PLOT, YEAR, X, Y))
QHI_new$SubsitePlotYearXY<- with(QHI_new, paste0(SUBSITE, PLOT, YEAR, X, Y))

# keeping only max values
#QHI_2022_max <- QHI_2022 %>%
 # group_by(SubsitePlotYearXY) %>%
 # mutate(max_heights_cm = max(Height))%>%
 # filter(SPP == "Salix pulchra",
 # TISSUE == "Live")%>%
 # dplyr::select(X, Y, SITE, SUBSITE, PLOT, YEAR, PlotN, SPP, 
       #         max_heights_cm, SubsitePlotYearXY)
#
#QHI_2022_max <- QHI_2022_max %>%
 # group_by(SubsitePlotYearXY)%>%
 # distinct() # keeping one unique value

#range(QHI_2022_max$max_heights_cm) # 10.9 41.0 cm! Makes sense.

# Making a plot column for the 1999-2019 data
#QHI_1999_2019$PlotN<- with(QHI_1999_2019, paste0(SUBSITE, PLOT))

#keep max heights only 
#QHI_2019_max <- QHI_1999_2019 %>%
 # group_by(SubsitePlotYearXY) %>%
  #mutate(max_heights = max(Height..cm.))
# keep pulchra only
#QHI_2019_max <- QHI_2019_max %>%
 # filter(SPP %in% c("Salix pulchra", "SALPUL"),
        # STATUS != "Standing dead", 
        # STATUS != "STANDINGDEAD", 
      #   TISSUE != "Standing dead")%>%
  #dplyr::select(X, Y, SITE, SUBSITE, PLOT, YEAR, PlotN, SPP, 
    #            max_heights, SubsitePlotYearXY)

#view(QHI_2019_max)

#QHI_2019_max <- QHI_2019_max %>%
#  group_by(SubsitePlotYearXY)%>%
#  distinct() # keep one unique value 

QHI_new_max <- QHI_new %>%
  group_by(SubsitePlotYearXY) %>%
 mutate(max_heights_cm = max(Height))%>%
  filter(SPP == "Salix pulchra",
         STATUS == "LIVE")%>%
  dplyr::select(X, Y, SITE, SUBSITE, PLOT, YEAR, PlotN, SPP, 
                max_heights_cm, SubsitePlotYearXY)

QHI_new_max <- QHI_new_max %>%
  group_by(SubsitePlotYearXY)%>%
  distinct() # keeping one unique

QHI_new_max_2022 <- QHI_new_max %>%
  filter(YEAR == 2022)

QHI_new_max_minus_2022 <- QHI_new_max %>%
  filter(YEAR != 2022 )

QHI_new_max_minus_2022$PlotN<- with(QHI_new_max_minus_2022, paste0(SUBSITE, PLOT))

QHI_new_bind_all <- rbind(QHI_new_max_minus_2022,QHI_new_max_2022)

# ISLA HELP figure out what is going on here ------
QHI_minus_2019_22_max <- QHI_new_bind_all %>%
  filter(YEAR != 2019) %>%# I think 2019 and 2022 might be in cm, while all other years in mm?
  filter(YEAR != 2022)
# separate 2019 and 2022 data
QHI_2019_22_max <- QHI_new_bind_all %>%
  filter(YEAR %in% c(2019, 2022))
view(QHI_2019_22_max)
# making values into mm (my judgement..?)
QHI_minus_2019_22_max_1 <- QHI_minus_2019_22_max %>%
  mutate(max_heights_cm = max_heights_cm/10)

# remerging data
QHI_new_bind <- rbind(QHI_minus_2019_22_max_1,QHI_2019_22_max)
view(QHI_new_bind)
write.csv(QHI_new_bind, "data/ITEX/QHI_new_bind.csv")
#QHI_2019_max <- QHI_2019_max %>%
 # mutate(max_heights_cm = case_when(max_heights> 41 ~ (max_heights/10), # im assuming anything above 30 is in mm
 ##                                   max_heights<= 41 ~ max_heights)) %>%
 # mutate(max_heights_cm= case_when(max_heights_cm <= 3 ~ (max_heights_cm*10), # im assuming anything above 30 is in mm
  #                                 max_heights_cm > 3 ~ max_heights_cm))%>%
 # dplyr::select(-max_heights)

# unique(QHI_2019_max$PlotN)

# merge with 2022 data
#QHI_1999_2022 <- rbind(QHI_2013_2019_bind, QHI_2022_max)
#unique(QHI_1999_2022$YEAR)
# quick plot
# raw data 

(pulchra_height_plot <- QHI_new_bind %>%
    ggplot(aes(x = YEAR, y = max_heights_cm)) +
    geom_point(data = QHI_new_bind) +
    geom_smooth(method = "lm") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm) \n") +
    xlab("\nYear") +
    theme_shrub())

# MODELLING------
# Model QHI pulchra heights over time-----
unique(QHI_new_bind$YEAR)
QHI_new_bind <-QHI_new_bind %>%
  mutate(Year_index = I(YEAR - 1998))

range(QHI_new_bind$max_heights_cm) #  0.0 39.3
hist(QHI_new_bind$max_heights_cm)

#QHI_new_bind$height_scale <- center_scale(QHI_new_bind$max_heights_cm)
QHI_new_bind$PlotN <- as.factor(QHI_new_bind$PlotN)
unique(QHI_new_bind$PlotN)

QHI_height_time <- brms::brm(max_heights_cm ~ Year_index + (Year_index|PlotN),
                               data = QHI_new_bind,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(QHI_height_time)
pp_check(QHI_height_time, type = "dens_overlay", ndraws = 100) 

(pulchra_height_plot <- QHI_1999_2022 %>%
    group_by(PlotN)%>%
    add_predicted_draws(QHI_height_time) %>%
    ggplot(aes(x = Year_index, y = max_heights_cm)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = QHI_1999_2022) +
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



