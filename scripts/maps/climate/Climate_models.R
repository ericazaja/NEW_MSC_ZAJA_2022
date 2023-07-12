# CLIMATE MODELS ------

# libraries ----
library(bayesplot)
library(brms)
library(tidybayes)
library(tidyverse)


# data ------
# coord.chelsa.combo.c.2020 <- read.csv("data/coord.chelsa.combo.c.2020")
coord.chelsa.combo.c.all.biom <- read.csv("data/coord.chelsa.combo.c.all.biom.csv")
coord.chelsa.combo.c.biom.2020 <- read.csv("data/coord.chelsa.combo.c.biom.2020.csv")
# coord.chelsa.combo.c.all <- read_csv("data/coord.chelsa.combo.c.all.csv") # too heavy, model wont run


# temp over time (2020-2100)-------
hist(coord.chelsa.combo.c.all.biom$mean_temp_C) # normal
unique(coord.chelsa.combo.c.all.biom$year_index) # I want 2020 as year 0
coord.chelsa.combo.c.all.biom <- coord.chelsa.combo.c.all.biom %>%
  mutate(year_index= I(year-2020))

temp_time_future <- brms::brm(mean_temp_C ~ year_index, 
                        data = coord.chelsa.combo.c.all.biom, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(temp_time_future) #significant ! 
plot(temp_time_future) # great
pp_check(temp_time_future, type = "dens_overlay", ndraws  = 100) 
# Save an object to a file
saveRDS(temp_time_future, file = "outputs/models/temp_time_future.rds")
# Restore the object
temp_time_future <- readRDS(file ="outputs/models/temp_time_future.rds")
#without random effects 
model_summ_no_rf <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  sigma$nobs <- obs
  
  modelTerms <- as.data.frame(bind_rows(fixed, sigma))  # merge it all together
}

temp_time_future_mod <- model_summ_no_rf(temp_time_future)

rownames(temp_time_future_mod) <- c(" Intercept ", "Year (index)", " sigma ")
temp_time_future_mod$Rhat <- as.character(formatC(temp_time_future_mod$Rhat, digits = 2, format = 'f'))

temp_time_future_table <- temp_time_future_mod %>% 
  kbl(caption="Table. Future temperature in study area (2020-2100). ", 
      col.names = c("Estimate", "Error", "Lower 95% CI", "Upper 95% CI",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

# optional: making specific column text in italics
column_spec(temp_time_future_table, 2, width = NULL, bold = FALSE) # 2 is my example column number 

save_kable(temp_time_future_table,file = "outputs/tables/temp_time_future_table.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)

# model: 2020 only----
# biomass in 2020 ~ temp in 2020 
model_test <- brms::brm(biomass_per_m2 ~ mean_temp_C, 
                        data = coord.chelsa.combo.c.biom.2020, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(model_test) #significant ! 
plot(model_test) # great
pp_check(model_test, type = "dens_overlay", ndraws  = 100) 
# Save an object to a file
saveRDS(model_test, file = "outputs/models/biomass_temp_2020.rds")
# Restore the object
model_test <- readRDS(file ="outputs/models/biomass_temp_2020.rds")

model_test_mod <- model_summ_no_rf(model_test)
rownames(model_test_mod) <- c(" Intercept ", "Mean July Temp (°C)", " sigma ")
model_test_mod$Rhat <- as.character(formatC(model_test_mod$Rhat, digits = 2, format = 'f'))

model_test_mod_table <- model_test_mod %>% 
  kbl(caption="Table. Temperature 2020 in study area vs biomass (g/m2) ", 
      col.names = c("Estimate", "Error", "Lower 95% CI", "Upper 95% CI",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts


save_kable(model_test_mod_table,file = "outputs/tables/model_test_mod_table.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)

# Quick data viz
mod <- (conditional_effects(model_test))
mod_data <- mod[[1]]

xlab <- "\nJuly temperature 2020 (°C) "

(biomass_temp_2020 <-ggplot(mod_data) +
    geom_point(data = coord.chelsa.combo.c.biom.2020, aes(x =mean_temp_C, y =biomass_per_m2 ),
             alpha = 0.01, colour = "red4")+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, colour ="black") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = 0.5) +
   # ylab("Shrub biomass (g/m2) \n") +
    labs(x = xlab,
         y = expression ("Shrub biomass"~g/m^2))+
   # ylim(0, 1500) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())

# Quick data viz
future_time <- (conditional_effects(temp_time_future))
future_time_dat <- future_time[[1]]

ylab <- "Projected july temperature (°C) \n"

(future_temps_plot <-ggplot(future_time_dat) +
    geom_point(data = coord.chelsa.combo.c.all.biom, aes(x =year_index, y = mean_temp_C ), colour = "red4",
               alpha = 0.05)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5,colour = "black") +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = 0.4) +
    labs( y = ylab)+
    xlab("\n Year (scaled)" ) +
    # ylim(0, 1500) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_shrub())


