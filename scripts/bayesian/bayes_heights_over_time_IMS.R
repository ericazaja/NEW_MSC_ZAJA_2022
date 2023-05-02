# Script to plot QHI monitoring heights over time -----

# libraries
library(tidyverse)
library(plyr)
library(brms) 
library(readxl)
library(tidybayes)
library(gridExtra)

# funciton to extract model summary
model_summ_heights <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Year_index # change name of random effect here 
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "random" # could change rowname here of random effect if you'd like 
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}


# LOAD DATA -----
# data 1999-2019
QHI_1999_2022 <- read_csv("data/ITEX/pointfr_1999_2022_clean.csv")
# upload new data from QHI repo
salpuls <- read_csv("data/ITEX/salpuls.csv")

# Wrangle salpuls data
salpuls <- subset(salpuls,!is.na(Height..cm.)) %>%
  dplyr::select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height..cm.)%>%
  dplyr::rename(Height = Height..cm.)

# filter out 2016 where STATUS and TISSU messed up
salpuls_no_2016 <- salpuls %>%
  dplyr::filter(YEAR != 2016)

salpuls_2016 <- salpuls %>%
  dplyr::filter(YEAR == 2016) %>% 
  mutate(STATUS_2 = case_when(STATUS %in% c("Leaf", "Stem") ~ "Live", 
            STATUS == "Live"~ "Live", 
            STATUS == "Standing dead" ~"Standing dead"))%>%
  dplyr::select(-STATUS) %>%
  dplyr::rename(STATUS = STATUS_2)

salpuls_bind <- rbind(salpuls_no_2016,salpuls_2016)

salpuls_new <- salpuls_bind %>%
  dplyr::filter(STATUS %in% c("LIVE", "Live"))

salpuls_new$SubsitePlotYear <- with(salpuls_new, paste0(SUBSITE, PLOT, YEAR))

meansp <- ddply(salpuls_new,.(YEAR), summarise,
                mean.height = mean(Height),
                sd = sd(Height))


# keep only 2019 and 2022 
QHI_2019_2022 <- QHI_1999_2022 %>%
  dplyr::filter(YEAR %in% c(2019, 2022)) %>% dplyr::select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height) %>% 
  dplyr::filter(SPP == "Salix pulchra" & STATUS == "LIVE")

# make a SubsitePlotYear col
QHI_2019_2022$SubsitePlotYear <- with(QHI_2019_2022, paste0(SUBSITE, PLOT, YEAR))
QHI_2019_2022$SubsitePlotYear <- as.factor(QHI_2019_2022$SubsitePlotYear )

# bind old and new data
all_bind <- rbind(salpuls_new, QHI_2019_2022)

all_bind$SubsitePlot <- with(all_bind, paste0(SUBSITE, PLOT))
all_bind <- all_bind %>% 
  dplyr::mutate(Year_index = I(YEAR - 1998)) 

hist(all_bind$Height) # normal
range(all_bind$Height)#0 32

# Keep max value per coordinate of the point framing
all_bind_new <- all_bind %>%
  group_by(SubsitePlot, YEAR, X, Y) %>%
  dplyr::summarise(max_pointfr_height = max(Height))

all_bind_new <- all_bind_new %>% 
  dplyr::mutate(Year_index = I(YEAR - 1998)) 


# model with ALL data
QHI_height_new <- brms::brm(max_pointfr_height ~ Year_index + (Year_index|SubsitePlot),
                             data = all_bind_new,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(QHI_height_new)
pp_check(QHI_height_new, type = "dens_overlay", ndraws = 100) 

all_bind_d <- (conditional_effects(QHI_height_new))
all_bind_dat <- all_bind_d[[1]]

(pulchra_height_plot_all <-ggplot(all_bind_dat) +
    geom_point(data = all_bind, aes(x = Year_index, y = Height),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("ALL Salix pulchra height (cm)\n") +
    xlab("\n Year (scaled) " ) +
    # ylim(0, 300) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_classic())

# calculate plot means and plot max

all_bind_new_mean <- ddply(all_bind_new,.(YEAR, SubsitePlot), summarise,
        mean_height = mean(max_pointfr_height))
  
all_bind_new_max <- ddply(all_bind_new,.(YEAR, SubsitePlot), summarise,
                           max_height = max(max_pointfr_height))

all_bind_new_mean <- all_bind_new_mean %>% 
  mutate(Year_index = I(YEAR - 1998)) 

all_bind_new_max <- all_bind_new_max %>% 
  mutate(Year_index = I(YEAR - 1998)) 

range(all_bind_new$max_pointfr_height)

# quick plot
# raw data 
(pulchra_height_plot <- all_bind_new_mean %>%
    ggplot(aes(x = YEAR, y = mean_height)) +
    geom_point(data = all_bind_new_mean) +
    geom_smooth(method = "lm") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm) \n") +
    xlab("\nYear (scaled)"))


# model with plot mean data
# using trunc to  so that the predictions will never give you a value over 160cm (the max pulchra height)
MEAN <- brms::brm(mean_height|trunc(lb = 0, ub = 160) ~ Year_index + (1|Year_index), 
                            data = all_bind_new_mean,  family = gaussian(), chains = 3,
                            iter = 5000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(MEAN)
pp_check(MEAN, type = "dens_overlay", ndraws = 100) 

MEAN_summ <- model_summ_heights(MEAN)

rownames(MEAN_summ) <- c("Intercept    ", "Year (indexed)    ", "Random intercept    ", "sigma     ")
MEAN_summ$Rhat <- as.character(formatC(MEAN_summ$Rhat, digits = 2, format = 'f'))

MEAN_summ <- MEAN_summ %>%
  mutate("Site" = "QHI","Scenario" = "Natural",
         "Response variable" = "Mean canopy height") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")

# plot
all_bind_mean <- (conditional_effects(MEAN))
all_bind_mean_dat <- all_bind_mean[[1]]

(pulchra_height_plot_mean <-ggplot(all_bind_mean_dat) +
    geom_point(data = all_bind_new_mean, aes(x = Year_index, y = mean_height),
               alpha = 0.5, colour =  "#D55E00")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour =  "#D55E00") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1,  fill =  "#D55E00") +
    ylab("Mean canopy height (cm)\n") +
    xlab("\n Year (scaled)" ) +
    ylim(0, 100) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) )


# model with plot max data
MAX <- brms::brm(max_height|trunc(lb = 0, ub = 160) ~ Year_index + (1|Year_index),
                  data = all_bind_new_max,  family = gaussian(), chains = 3,
                  iter = 5000, warmup = 1000, 
                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(MAX)
pp_check(MAX, type = "dens_overlay", ndraws = 100) 

MAX_summ <- model_summ_heights(MAX)

rownames(MAX_summ) <- c("    Intercept ", "     Year (indexed) ", "    Random intercept ", "       sigma")
MAX_summ$Rhat <- as.character(formatC(MAX_summ$Rhat, digits = 2, format = 'f'))

MAX_summ <- MAX_summ %>%
  mutate("Site" = "QHI", "Scenario"= "Natural",
         "Response variable" = "Max canopy height") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")

Mean_max_bind <- rbind(MEAN_summ, MAX_summ, all_natural_cov)
bind_with_cg <- rbind(cg_models_bind,Mean_max_bind)


kable_bind_with_cg <- bind_with_cg %>% 
  kbl(caption="Table. Heights and cover over time of natural vs novel scenarios. ", 
      col.names = c("Response variable", "Scenario", "Site", "Estimate", "Error", "Lower 95% CI", "Upper 95% CI",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=3, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=TRUE, html_font="Helvetica")# can change fonts

kable_bind_with_cg <- kable_bind_with_cg %>%
  row_spec(0,  bold = TRUE)%>%
  row_spec(c(2,8,14,18,22, 26, 30, 34), bold = TRUE)%>%
  landscape()

kable_bind_with_cg

# optional: making specific column text in italics
save_kable(kable_bind_with_cg,file = "outputs/tables/kable_bind_with_cg_2.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)





# plot
all_bind_max <- (conditional_effects(MAX))
all_bind_max_dat <- all_bind_max[[1]]

(pulchra_height_plot_max <-ggplot(all_bind_max_dat) +
    geom_point(data = all_bind_new_max, aes(x = Year_index, y = max_height),
               alpha = 0.5, colour = "#D55E00")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#D55E00") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#D55E00") +
    ylab("Max canopy height (cm)\n") +
    xlab("\n Year (scaled)" ) +
    ylim(0, 100) +
    #scale_color_brewer(palette = "Greys")+
    #scale_fill_brewer(palette = "Greys")+
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) )


panel <- grid.arrange(pulchra_height_plot_all, pulchra_height_plot_mean,
                      pulchra_height_plot_max, nrow =1)

# STOP ----

# Something is wrong here - check with Mariana
test <- QHI_1999_2022 %>% 
  mutate(Year_index = I(YEAR - 1998)) %>%
  group_by(SUBSITE, PLOT, YEAR) %>%
  summarise(HeightMax = max(Height))

# DATA WRANGLE 
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

# MODELLING
# Model QHI pulchra heights over time

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




