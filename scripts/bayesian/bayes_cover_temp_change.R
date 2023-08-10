# Cover change VS temp change at ITEX sites 

library(brms)
library(tidybayes)
library(tidyverse)
library(ggeffects)

# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# data  --------
july_enviro_chelsa <- read_csv("data/climate_data/july_enviro_chelsa.csv") # chelsa temperature and precipitation data 
ITEX_shrubs_msc <- read_csv("data/ITEX/itex_EZ_shrubs_2023.csv") # newest 
all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")

# 1. temp over time
#rename column
july_enviro_chelsa <- july_enviro_chelsa %>%
  dplyr::rename ("mean_temp_C" ="(mean_temp_C = (mean_temp/10 - 273.15))")

july_enviro_chelsa <- july_enviro_chelsa %>%
  dplyr::mutate(Site = case_when(site == "ATIGUN" ~ "ANWR",
                          site %in% c("IMNAVAIT", "TUSSOKGRID") ~ "TOOLIK", 
                          site == "QHI" ~ "QHI",
                          site == "Common_garden" ~ "CG",
                          site == "Kluane_plateau" ~ "KP"))


july_enviro_chelsa$Site <- as.factor(july_enviro_chelsa$Site)
hist(july_enviro_chelsa$mean_temp_C)
#july_enviro_chelsa$mean_temp_C_scaled <- center_scale(july_enviro_chelsa$mean_temp_C)
hist(july_enviro_chelsa$mean_temp_C_scaled)

july_enviro_chelsa <-july_enviro_chelsa %>%
  filter(year >=1999) %>%
  select(-PrecipMeanJuly)%>%
  mutate(index_year = I(year-1998))%>%
  filter(Site != "ANWR")

# temp over time (random intercept random slope)
temp_time <- brms::brm(mean_temp_C ~ index_year + (index_year|Site),
                       data = july_enviro_chelsa,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

temp_time_interact <- brms::brm(mean_temp_C ~ index_year*Site + (1|index_year),
                       data = july_enviro_chelsa,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(temp_time_interact,file = "outputs/models/temp_time_interact.rds")
summary(temp_time_interact)
ylab <- "Mean July temperature (°C)"

temp_preds <- ggpredict(temp_time_interact, terms = c("index_year", "Site"))
# temp at year 22 for QHI=8.761188
#  temp at year 0 for QHI=3.539306
#delta =8.761188-3.539306=5.221882
# temp change per year 5.221882/21 = 0.248661 deg C/year

#  temp at year 22 for toolik=11.826728
#  temp at year 0 for toolik=9.003889
# delta 11.826728-9.003889 =  2.822839 deg C/year
#  temp change per year = 2.822839/22 = 0.1283109 decC/year

# mean (0.248661+0.1283109)/2 = 0.188486 degC/year in natural populations


(temp_time_plot <- july_enviro_chelsa %>%
    group_by(Site) %>%
    add_predicted_draws(temp_time, allow_new_levels = TRUE) %>%
    ggplot(aes(x =index_year, y =mean_temp_C , color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = july_enviro_chelsa) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    labs(y= ylab) +
    xlab("\nYear, scaled") +
    theme_shrub())

ggpred_chelsa_temps <- ggpredict(temp_time_interact, terms =c("index_year", "Site"))
colnames(ggpred_chelsa_temps) = c('index_year', 'fit', 'lwr', 'upr', 'Site')

(ggpred_chelsa_temps_plot <-ggplot(ggpred_chelsa_temps) +
    geom_point(data = july_enviro_chelsa, aes(x = index_year, y = mean_temp_C, colour = Site),
               alpha = 0.5)+ # raw data
    geom_line( aes(x = index_year , y = fit, colour = Site), linewidth = 1) +
    geom_ribbon(aes(x = index_year, ymin = lwr, ymax = upr, fill = Site),
                alpha = 0.1)+
    labs(y= ylab) +
    xlab("\n Year (indexed)" ) +
    #scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    #scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    scale_color_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480",
                                  "CG"= "#98d83b", "KP" = "yellow2"))+
    scale_fill_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480", "CG"= "#98d83b","KP" = "yellow2"))+
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

ggsave(ggpred_chelsa_temps_plot, file="outputs/figures/ggpred_chelsa_temps_plot.png", width = 8, height = 6.53, units = "in")

# I think I only need the index_year estimates
temp_time_random <- as.data.frame(ranef(temp_time)) # extract random eff. slopes 
temp_time_random$Site <- row.names(temp_time_random) # Apply row.names function
rownames(temp_time_random) <- NULL
colnames(temp_time_random)[5] <- "index_year_estimate" 
colnames(temp_time_random)[6] <- "index_year_error" 
colnames(temp_time_random)[7] <- "index_year_Q_25" 
colnames(temp_time_random)[8] <- "index_year_Q_97"
view(temp_time_random)

temp_time_coef <- as.data.frame(coef(temp_time)) # all coefficients. 
## ERICA CHECK THIS: I think the slope of each line is the coefficients for year minus the random eff. slopes (year coeff)

temp_time_random_year <- temp_time_random %>%
  dplyr::select("Site","index_year_estimate" ,"index_year_error", "index_year_Q_25",
                "index_year_Q_97")
view(temp_time_random_year)
write.csv(temp_time_random_year,"data/temp_time_random_year.csv")

# funciton to extract model summary
model_summ_temp <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$index_year # change name of random effect here 
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  # fixed$effect <- "population"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  # random$effect <- "SampleID_standard"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  row.names(random)[row.names(index_year) == "index_year"] <- "index_year" # could change rowname here of random effect if you'd like 
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

temps_summ <- model_summ(temp_time_interact)
temps_summ$Rhat <- as.character(formatC(temps_summ$Rhat, digits = 2, format = 'f'))
rownames(temps_summ) <- c("Intercept (ANWR)", "Year (indexed)", "CG"
                               , "KP", "QHI", "TOOLIK","Year (indexed):CG",
                               "Year (indexed):KP", "Year (indexed):QHI", "Year (indexed):TOOLIK",
                               "Year (indexed) "," sigma ")

temp_time_interact_table <- temps_summ %>% 
  kbl(caption="Table. CHELSA temperatures over time of different sites. ", 
      col.names = c("Estimate", "Error", "Lower 95% CI", "Upper 95% CI",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

# optional: making specific column text in italics
column_spec(temp_time_interact_table, 2, width = NULL, bold = FALSE) # 2 is my example column number 

save_kable(temp_time_interact_table,file = "outputs/tables/temp_time_interact_table.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)




# IGNORE Slope vs slope models -----
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


# IGNORE BELOW -----
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




