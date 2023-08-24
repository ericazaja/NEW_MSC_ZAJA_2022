# Script to plot QHI monitoring heights over time -----
# created by Erica and helped by Prof. Isla Myers-Smith

# Loading libraries -----
library(tidyverse)
library(plyr)
library(brms) 
library(readxl)
library(tidybayes)
library(gridExtra)
library(kableExtra)
library(ggeffects)

# functions ----
# function to extract model summary
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


# Loading data -----
# data 1999-2019
QHI_1999_2022 <- read_csv("data/ITEX/pointfr_1999_2022_clean.csv")
# upload new data from QHI repo
salpuls <- read_csv("data/ITEX/salpuls.csv")

# Data wrangling ------
# Wrangle salpuls data
salpuls <- subset(salpuls,!is.na(Height..cm.)) %>%
  dplyr::select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height..cm.)%>%
  dplyr::rename(Height = Height..cm.)

# filter out 2016 where STATUS and TISSUE messed up
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

# Modelling ------
# model with ALL data
QHI_height_new <- brms::brm(max_pointfr_height ~ Year_index + (Year_index|SubsitePlot),
                             data = all_bind_new,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(QHI_height_new)
pp_check(QHI_height_new, type = "dens_overlay", ndraws = 100) 

all_bind_d <- (conditional_effects(QHI_height_new))
all_bind_dat <- all_bind_d[[1]]

# plot
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

# raw data plot
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
saveRDS(MEAN, file = "outputs/models/MEAN.rds")
MEAN <- readRDS("outputs/models/MEAN.rds")

MEAN_summ <- model_summ_heights(MEAN)

rownames(MEAN_summ) <- c("Intercept    ", "Year (indexed)    ", "Random intercept    ", "sigma     ")
MEAN_summ$Rhat <- as.character(formatC(MEAN_summ$Rhat, digits = 2, format = 'f'))

MEAN_summ <- MEAN_summ %>%
  mutate("Site" = "QHI","Scenario" = "Natural",
         "Response variable" = "Mean height") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")

# plot mean data model
all_bind_mean <- (conditional_effects(MEAN))
all_bind_mean_dat <- all_bind_mean[[1]]

(pulchra_height_plot_mean <-ggplot(all_bind_mean_dat) +
    geom_point(data = all_bind_new_mean, aes(x = Year_index, y = mean_height),
               alpha = 0.5, colour =  "#2b788c")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour =  "#2b788c") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1,  fill =  "#2b788c") +
    ylab("Mean canopy height (cm)\n") +
    xlab("\n Year (scaled)" ) +
   # ylim(0, 100) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))

(pulchra_height_plot_mean_new <- all_bind_new_mean %>%
    #group_by(population) %>%
    add_predicted_draws(MEAN, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Year_index, y = mean_height), color = "#2b788c") +
    stat_lineribbon(aes(y = .prediction), colour = "#2b788c", fill = "#2b788c",.width = c(.50), alpha = 0.2) +
    geom_point(data = all_bind_new_mean, alpha = 0.5, colour = "#2b788c") +
    # scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    #scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    #scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    theme_shrub() +
    ggtitle(expression(italic("Salix pulchra"))) +
    ylab("Mean canopy height (cm)\n") +
    xlab("\nYear (scaled)") + theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))
# theme(legend.position = "none"))


# model with plot max data
MAX <- brms::brm(max_height|trunc(lb = 0, ub = 160) ~ Year_index + (1|Year_index),
                  data = all_bind_new_max,  family = gaussian(), chains = 3,
                  iter = 5000, warmup = 1000, 
                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(MAX)
pp_check(MAX, type = "dens_overlay", ndraws = 100) 
saveRDS(MAX, file = "outputs/models/MAX.rds")
MAX <- readRDS("outputs/models/MAX.rds")

MAX_summ <- model_summ_heights(MAX)

rownames(MAX_summ) <- c("    Intercept ", "     Year (indexed) ", "    Random intercept ", "       sigma")
MAX_summ$Rhat <- as.character(formatC(MAX_summ$Rhat, digits = 2, format = 'f'))

MAX_summ <- MAX_summ %>%
  mutate("Site" = "QHI", "Scenario"= "Natural",
         "Response variable" = "Max height") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")

# binding datasets from various scripts to make final table
Mean_max_bind <- rbind(MEAN_summ, MAX_summ, all_natural_cov)
bind_with_cg <- rbind(cg_models_bind, Mean_max_bind)

write.csv(bind_with_cg, file= "data/bind_with_cg.csv")


subsetted <- bind_with_cg[c(1, 2, 6,7, 8,12, 13, 14,16, 17, 18,20, 21, 22,24, 25, 26, 28,29,30,32,33, 34, 36),] # only keeping sample age term

rownames(subsetted) <- c("Intercept", "Age","sigma","Intercept " , "Age ",
                        "sigma ", "Intercept      ", "Year ", "sigma  ", 
                        "     Intercept  ", "Year     ", "sigma   ",
                        " Intercept   ", " Year   ", "sigma     ",
                        "  Intercept ", "   Year ", "       sigma  ",
                        "    Intercept ", "     Year ", "     sigma  ",
                        "Intercept    ", " Year    ", "  sigma     ")

write.csv(subsetted, file= "data/subsetted_table_1_dat.csv")
  
kable_bind_with_cg <- bind_with_cg %>% 
  kbl(caption="Table. Heights and cover over time of natural vs novel scenarios. ", 
      col.names = c("Response variable", "Scenario", "Site", "Estimate", "Error", "Lower 95% CI", "Upper 95% CI",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=1, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=TRUE, html_font="Helvetica")# can change fonts

kable_bind_with_cg <- kable_bind_with_cg %>%
  row_spec(0,  bold = TRUE)%>%
  #column_spec(5,  bold = TRUE)%>%
 #column_spec(4,  bold = TRUE)%>%
  row_spec(c(2,8,14,18,22, 26, 30, 34), bold = TRUE)%>%
  landscape()

kable_bind_with_cg

# optional: making specific column text in italics
save_kable(kable_bind_with_cg,file = "outputs/tables/kable_bind_with_cg.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)


# plot with max data model
(pulchra_height_plot_max_new <- all_bind_new_max %>%
    #group_by(population) %>%
    add_predicted_draws(MAX, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Year_index, y = max_height), color = "#2b788c") +
    stat_lineribbon(aes(y = .prediction), colour = "#2b788c", fill = "#2b788c",.width = c(.50), alpha = 0.2) +
    geom_point(data = all_bind_new_max, alpha = 0.5, colour = "#2b788c") +
    # scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    #scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    #scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    theme_shrub() +
    ggtitle(expression(italic("Salix pulchra"))) +
    ylab("Max canopy height (cm)\n") +
    xlab("\nYear (scaled)") + theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))
# theme(legend.position = "none"))


all_bind_max <- (conditional_effects(MAX))
all_bind_max_dat <- all_bind_max[[1]]

(pulchra_height_plot_max <-ggplot(all_bind_max_dat) +
    geom_point(data = all_bind_new_max, aes(x = Year_index, y = max_height),
               alpha = 0.5, colour = "#2b788c")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour = "#2b788c") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1, fill = "#2b788c") +
    ylab("Max canopy height (cm)\n") +
    xlab("\n Year (scaled)" ) +
   # ylim(0, 100) +
    #scale_color_brewer(palette = "Greys")+
    #scale_fill_brewer(palette = "Greys")+
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))


panel <- grid.arrange(pulchra_height_plot_mean_new,
                      pulchra_height_plot_max_new, nrow =1)

ggsave(panel, filename ="output/figures/QHI_heights_panel.png", width = 14.67, height = 6.53, units = "in")

# trying plotting with ggpredict
ggpred_MAX_qhi <- ggpredict(MAX, terms = "Year_index")
colnames(ggpred_MAX_qhi) = c('Year_index', 'fit', 'lwr', 'upr', 'dunno')
# estimate at year 1 = 8.105129
# estimate at year 24 = 24.729984

(ggrped_MAX_qhi_plot <-ggplot(ggpred_MAX_qhi) +
    geom_point(data = all_bind_new_max, aes(x = Year_index, y = max_height),
               alpha = 0.5, colour = "#2b788c")+ # raw data
    geom_line(aes(x = Year_index , y = fit), linewidth = 1, colour = "#2b788c")+
    geom_ribbon(aes(x = Year_index, ymin = lwr, ymax = upr),
                  alpha = 0.1, fill = "#2b788c") +
    ylab("Max. canopy height (cm)\n") +
    xlab("\n Year (indexed)" ) +
    #scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    #scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

# trying model with ggpredict
ggpred_MEAN_qhi <- ggpredict(MEAN, terms = "Year_index")
colnames(ggpred_MEAN_qhi) = c('Year_index', 'fit', 'lwr', 'upr', 'dunno')
# estimate at year 1 = 4.941228
# estimate at year 24 = 12.754196


(ggrped_MEAN_qhi_plot <-ggplot(ggpred_MEAN_qhi) +
    geom_point(data = all_bind_new_mean, aes(x = Year_index, y = mean_height),
               alpha = 0.5, colour = "#2b788c")+ # raw data
    geom_line(aes(x = Year_index , y = fit), linewidth = 1, colour = "#2b788c")+
    geom_ribbon(aes(x = Year_index, ymin = lwr, ymax = upr),
                alpha = 0.1, fill = "#2b788c") +
    ylab("Mean canopy height (cm)\n") +
    xlab("\n Year (indexed)" ) +
    #scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    #scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 







