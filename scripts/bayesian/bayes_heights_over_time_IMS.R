# Script to plot QHI monitoring heights over time -----

# libraries
library(tidyverse)
library(plyr)
library(brms) 
library(readxl)
library(tidybayes)

# LOAD DATA -----
# data 1999-2019
QHI_1999_2022 <- read_csv("data/ITEX/pointfr_1999_2022_clean.csv")
# upload new data from QHI repo
salpuls <- read_csv("data/ITEX/salpuls.csv")

# Wrangle salpuls data
salpuls <- subset(salpuls,!is.na(Height..cm.)) %>%
  select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height..cm.)%>%
  dplyr::rename(Height = Height..cm.)

# filter out 2016 where STATUS and TISSU messed up
salpuls_no_2016 <- salpuls %>%
  filter(YEAR != 2016)

salpuls_2016 <- salpuls %>%
  filter(YEAR == 2016) %>% 
  mutate(STATUS_2 = case_when(STATUS %in% c("Leaf", "Stem") ~ "Live", 
            STATUS == "Live"~ "Live", 
            STATUS == "Standing dead" ~"Standing dead"))%>%
  select(-STATUS) %>%
  dplyr::rename(STATUS = STATUS_2)

salpuls_bind <- rbind(salpuls_no_2016,salpuls_2016)

salpuls_new <- salpuls_bind %>%
  filter(STATUS %in% c("LIVE", "Live"))

salpuls_new$SubsitePlotYear <- with(salpuls_new, paste0(SUBSITE, PLOT, YEAR))

meansp <- ddply(salpuls_new,.(YEAR), summarise,
                mean.height = mean(Height),
                sd = sd(Height))


# keep only 2019 and 2022 
QHI_2019_2022 <- QHI_1999_2022 %>%
  filter(YEAR %in% c(2019, 2022)) %>% select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height) %>% 
  filter(SPP == "Salix pulchra" & STATUS == "LIVE")

# make a SubsitePlotYear col
QHI_2019_2022$SubsitePlotYear <- with(QHI_2019_2022, paste0(SUBSITE, PLOT, YEAR))
QHI_2019_2022$SubsitePlotYear <- as.factor(QHI_2019_2022$SubsitePlotYear )

# bind old and new data
all_bind <- rbind(salpuls_new, QHI_2019_2022)

all_bind$SubsitePlot <- with(all_bind, paste0(SUBSITE, PLOT))
all_bind <- all_bind %>% 
  mutate(Year_index = I(YEAR - 1998)) 

hist(all_bind$Height) # normal
range(all_bind$Height) #0 32

# Keep max value per coordinate of the point framing
all_bind_new <- all_bind %>%
  group_by(SubsitePlot, YEAR, X, Y) %>%
  dplyr::summarise(max_pointfr_height = max(Height))
all_bind_new <- all_bind_new %>% 
  mutate(Year_index = I(YEAR - 1998)) 


# model with ALL data
QHI_height_new <- brms::brm(max_pointfr_height ~ Year_index + (Year_index|SubsitePlot),
                             data = all_bind_new,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(QHI_height_new)
pp_check(QHI_height_new, type = "dens_overlay", ndraws = 100) 

all_bind_d <- (conditional_effects(QHI_height_new))
all_bind_dat <- all_bind_d[[1]]

(pulchra_height_plot <-ggplot(all_bind_dat) +
    geom_point(data = all_bind, aes(x = Year_index, y = Height),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Salix pulchra height (cm)\n") +
    xlab("\n Year (scaled) (cm)" ) +
    # ylim(0, 300) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_classic())

# merge all data
# do the max and means on the all_bind instead of thebelow !

QHI_2019_2022_mean <- ddply(QHI_2019_2022,.(YEAR, SUBSITE, PLOT), summarise,
      mean_height = mean(Height))
QHI_2019_2022_max <- ddply(QHI_2019_2022,.(YEAR, SUBSITE, PLOT), summarise,
                            max_height = max(Height))

# PLOT MEAN
avg_salpuls <- ddply(salpuls_new,.(YEAR, SUBSITE, PLOT), summarise,
                     mean_height = mean(Height))

# PLOT MAX
max_salpuls <- ddply(salpuls_new,.(YEAR, SUBSITE, PLOT), summarise,
                     max_height = max(Height))

new_bind_mean <- rbind(avg_salpuls,QHI_2019_2022_mean)
new_bind_max <- rbind(max_salpuls,QHI_2019_2022_max)

new_bind_mean$SubsitePlot <- with(new_bind_mean, paste0(SUBSITE, PLOT))
new_bind_max$SubsitePlot <- with(new_bind_max, paste0(SUBSITE, PLOT))

new_bind_mean <- new_bind_mean %>% 
  mutate(Year_index = I(YEAR - 1998)) 

new_bind_max <- new_bind_max %>% 
  mutate(Year_index = I(YEAR - 1998)) 

# quick plot
# raw data 
(pulchra_height_plot <- all_bind_test %>%
    ggplot(aes(x = YEAR, y = max_all)) +
    geom_point(data = all_bind_test) +
    geom_smooth(method = "lm") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm) \n") +
    xlab("\nYear (scaled)"))


# model with plot mean data
MEAN <- brms::brm(mean_height ~ Year_index + (Year_index),
                            data = new_bind_mean,  family = gaussian(), chains = 3,
                            iter = 5000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(MEAN)
pp_check(MEAN, type = "dens_overlay", ndraws = 100) 

all_bind_mean <- (conditional_effects(MEAN))
all_bind_mean_dat <- all_bind_mean[[1]]

(pulchra_height_plot <-ggplot(all_bind_mean_dat) +
    geom_point(data = new_bind_mean, aes(x = Year_index, y = mean_height),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Salix pulchra height (cm)\n") +
    xlab("\n Year (scaled) (cm)" ) +
    # ylim(0, 300) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_classic())

# model with plot max data
MAX <- brms::brm(max_height ~ Year_index + (Year_index),
                  data = new_bind_max,  family = gaussian(), chains = 3,
                  iter = 5000, warmup = 1000, 
                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(MAX)
pp_check(MAX, type = "dens_overlay", ndraws = 100) 

all_bind_max <- (conditional_effects(MAX))
all_bind_max_dat <- all_bind_max[[1]]

(pulchra_height_plot <-ggplot(all_bind_max_dat) +
    geom_point(data = new_bind_max, aes(x = Year_index, y = max_height),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Salix pulchra height (cm)\n") +
    xlab("\n Year (scaled) (cm)" ) +
    # ylim(0, 300) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_classic())








# STOP ----

# Something is wrong here - check with Mariana
test <- QHI_1999_2022 %>% 
  mutate(Year_index = I(YEAR - 1998)) %>%
  group_by(SUBSITE, PLOT, YEAR) %>%
  summarise(HeightMax = max(Height))

# DATA WRANGLE ------
# make a subsite plot year x y column 
QHI_2022 <- QHI_1999_2022 %>% select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height) %>% 
  filter(SPP == "Salix pulchra" & STATUS == "LIVE")

QHI_2022_sum_max <- QHI_2022 %>% 
  group_by(SUBSITE, PLOT, YEAR, SPP) %>%
  summarise(HeightMax = max(Height)) %>%
  mutate(Height_mm = case_when(HeightMax >= 100 ~ "TRUE", HeightMax < 100 ~ "FALSE")) %>%
  select(-HeightMax)


QHI_2022_cm <- QHI_2022 %>% 
  left_join(QHI_2022_sum_max) %>% 
  mutate(Height_cm = case_when(Height_mm == "TRUE" ~ Height/10, 
                               Height_mm == "FALSE"~ Height)) %>% 
  # filter(Height_cm > 10) %>% # This will remove things like roots and basal stems
  select(-Height) %>%
  na.omit(Height_cm) 

# keeping only max values
QHI_2022_max <- QHI_2022_cm %>%
  group_by(SUBSITE, PLOT, YEAR, SPP, X, Y) %>%
  summarise(max_heights_cm = max(Height_cm)) %>%
  distinct() %>% # keeping one unique value
  na.omit(max_heights_cm) %>%
  mutate(max_heights_integer = round(max_heights_cm))

range(QHI_2022_max$max_heights_cm) # 0.2 39.3 cm 
hist(QHI_2022_max$max_heights_cm, breaks = 30) # not normal at all

QHI_2022_max$SubsitePlot <- with(QHI_2022_max, paste0(SUBSITE, PLOT))

# calculating a plot mean per year
QHI_2022_max_2 <- QHI_2022_max %>%
  group_by(SubsitePlot, YEAR) %>%
  summarise(plot_mean = mean(max_heights_cm))
hist(QHI_2022_max_2$plot_mean, breaks=30)

# calculating a plot max
QHI_2022_max_3 <- QHI_2022_max %>%
  group_by(SubsitePlot, YEAR) %>%
  summarise(plot_max = max(max_heights_cm))
hist(QHI_2022_max_3$plot_max, breaks = 30)

#QHI_2022_max <- QHI_2022_max %>%
 # mutate(log_max_heights = log(max_heights_cm))

# MODELLING------
# Model QHI pulchra heights over time-----

range(QHI_2022_max$max_heights_cm) #  0.0 39.3
hist(QHI_2022_max$max_heights_cm, breaks = 30)
hist(QHI_2022_max$log_max_heights, breaks = 30)
hist(QHI_2022_max$max_heights_integer, breaks = 30)

QHI_2022_max$SubsitePlot <- with(QHI_2022_max, paste0(SUBSITE, PLOT))
QHI_2022_max <- QHI_2022_max %>% 
  mutate(Year_index = I(YEAR - 1998)) 

QHI_2022_max$SubsitePlot <- as.factor(QHI_2022_max$SubsitePlot)
unique(QHI_2022_max$SubsitePlot)

QHI_height_time <- brms::brm(max_heights_cm ~ Year_index + (Year_index|SubsitePlot),
                               data = QHI_2022_max,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


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




