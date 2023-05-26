# BAYESIAN models for itex cover analysis
# script by Erica
# last update: 21/02/2023

# libraries ------
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(plyr)
library(sjPlot)

# funciton to extract model summary
model_summ_cov <- function(x) {
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

# load data ------
itex_EZ_shrubs_2023 <- read_csv("data/ITEX/itex_EZ_shrubs_2023.csv")
#pulchra_yearly_max <- read_csv("data/ITEX/itex.meta.max.csv")
#pulchra_yearly_mean <- read_csv("data/ITEX/itex.meta.mean.csv")


# wrangle data -----
#pulchra_yearly_max <- pulchra_yearly_max%>%
 # mutate(cover_prop_max = max_cov/100)%>% 
 # mutate(Year_index = I(YEAR - 1988)) %>%
 # mutate(SITE = case_when(SiteSubsite %in% c("QHI:HE", "QHI:KO")~ "QHI", 
                 #         SiteSubsite %in% c("TOOLIK:IMNAVAIT", "TOOLIK:MOIST", "TOOLIK:TUSSOCKGRID")~ "TOOLIK"))

#pulchra_yearly_mean <- pulchra_yearly_mean%>%
 # mutate(cover_prop_mean = mean_cov/100)%>% 
 # mutate(Year_index = I(YEAR - 1988)) %>%
  #mutate(SITE = case_when(SiteSubsite %in% c("QHI:HE", "QHI:KO")~ "QHI", 
       #                   SiteSubsite %in% c("TOOLIK:IMNAVAIT", "TOOLIK:MOIST", "TOOLIK:TUSSOCKGRID")~ "TOOLIK"))

# dividing percent cover by 100 to use beta family in models
itex_EZ_shrubs_2023 <- itex_EZ_shrubs_2023 %>%
  mutate(cover_prop = RelCover/100)

# divide species 
itex_EZ_arctica <- itex_EZ_shrubs_2023 %>%
  filter(SPECIES_NAME == "Salix arctica") %>%
  filter(SITE %in% c("ANWR", "QHI" ))

itex_EZ_pulchra <- itex_EZ_shrubs_2023 %>%
  filter(SPECIES_NAME == "Salix pulchra")

# calculate subsite mean per year, cannot do it plot level
mean <- ddply(itex_EZ_pulchra,.(YEAR, SiteSubsite), summarise,
              mean_cov = mean(cover_prop))# take all plots and calculate mean. so i have one value per yer

# calculate subsite max per year, cannot do it plot level
max <- ddply(itex_EZ_pulchra,.(YEAR, SiteSubsite), summarise,
             max_cov = max(cover_prop)) # # take all plots and calculate max. so i have one value per yer (plot with highest value)
hist(max$max_cov, breaks = 10)

# mean and max in one dataset
meanmax <- ddply(itex_EZ_pulchra,.(YEAR, SiteSubsite), summarise,
                 max_cov = max(cover_prop), mean_cov = mean(cover_prop))

mean <- mean %>%
mutate(Year_index = I(YEAR - 1988)) %>%
  filter(SiteSubsite != "QHI:KO")%>%
  mutate(SITE = case_when(SiteSubsite %in% c("QHI:HE", "QHI:KO")~ "QHI", 
                          SiteSubsite %in% c("TOOLIK:IMNAVAIT", "TOOLIK:MOIST", "TOOLIK:TUSSOCKGRID")~ "TOOLIK"))

range(mean$mean_cov) #  0.06909564 0.22607917
hist(mean$mean_cov, breaks = 10)

  
max <- max %>%
  mutate(Year_index = I(YEAR - 1988))%>%
  filter(SiteSubsite != "QHI:KO")%>%
  mutate(SITE = case_when(SiteSubsite %in% c("QHI:HE", "QHI:KO")~ "QHI", 
                          SiteSubsite %in% c("TOOLIK:IMNAVAIT", "TOOLIK:MOIST", "TOOLIK:TUSSOCKGRID")~ "TOOLIK"))

range(max$max_cov) # 0.0800000 0.7692308

meanmax <- meanmax %>%
  mutate(Year_index = I(YEAR - 1988)) %>%
  filter(SiteSubsite != "QHI:KO")%>%
  mutate(SITE = case_when(SiteSubsite %in% c("QHI:HE", "QHI:KO")~ "QHI", 
                          SiteSubsite %in% c("TOOLIK:IMNAVAIT", "TOOLIK:MOIST", "TOOLIK:TUSSOCKGRID")~ "TOOLIK"))

hist(itex_EZ_shrubs_2023$cover_prop, breaks = 30)
hist(itex_EZ_arctica$cover_prop, breaks = 20) # NOT zero inflated
hist(itex_EZ_pulchra$cover_prop, breaks = 30) # YES zero inflated
range(itex_EZ_arctica$cover_prop) # 0.0078125 0.3861386
range(itex_EZ_pulchra$cover_prop) #  0.006944444 0.769230769
range(itex_EZ_shrubs_2023$cover_prop)
# 0.006944444 0.769230769
# values between 0.0001 and 0.9999 ! So beta distribution

range(itex_EZ_pulchra$YEAR) # 1989 2022
range(itex_EZ_arctica$YEAR)

# filter out plots that dont have at least 3 years of repeat measure
#itex_EZ_arctica <- itex_EZ_arctica %>%
 # filter(PLOT <=6)

#itex_EZ_pulchra <- itex_EZ_pulchra %>%
 # filter(PLOT != 15)  %>%
 # filter(PLOT != 28)   %>%
 # filter(PLOT != 35)   %>%
 # filter(PLOT != 40) %>%
#filter(PLOT != 58)  %>%
  #filter(PLOT != 65)   %>%
  #filter(PLOT != 68)  %>%
  #filter(PLOT != 69)  %>%
  #filter(PLOT != 82)%>%
  #filter(SUBSITE != "KO")

itex_EZ_arctica$SITE <- as.factor(itex_EZ_arctica$SITE)
itex_EZ_pulchra$SITE <- as.factor(itex_EZ_pulchra$SITE)
itex_EZ_pulchra$SiteSubsitePlot <- as.factor(itex_EZ_pulchra$SiteSubsitePlot)
itex_EZ_arctica$SiteSubsitePlot <- as.factor(itex_EZ_arctica$SiteSubsitePlot)
itex_EZ_pulchra$SitePlotYear<- with(itex_EZ_pulchra, paste0(SITE, PLOT, YEAR))
itex_EZ_pulchra$SitePlot<- with(itex_EZ_pulchra, paste0(SITE, PLOT))

# quick plots
(plot <- ggplot(meanmax) +
  geom_point(aes(x =YEAR , y = mean_cov, color= SITE, fill =SITE)))

(plot <- ggplot(meanmax) +
    geom_point(aes(x =YEAR , y = max_cov, color= SITE, fill =SITE)))

# MODELLING -----
# cover over time at different sites
# Salix pulchra -----
# overall model 
pulchra_cover <- brms::brm(cover_prop ~ I(YEAR-1988)+ (I(YEAR-1988)|SiteSubsitePlot),
                      data = itex_EZ_pulchra, family = "beta", chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_cover) 
plot(pulchra_cover)
pp_check(pulchra_cover, type = "dens_overlay", nsamples = 100) 

# site max and means
pulchra_cover_max <- brms::brm(max_cov ~ Year_index * SITE + (1|Year_index),
                           data = maxtest, family = "beta", chains = 3,
                           iter = 5000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))
pp_check(pulchra_cover_max, ndraws=100)
plot_model(pulchra_cover_max, type = "pred", terms = c("Year_index ", "SITE")) # to visualise interaction
summary(pulchra_cover_max) # 0.001
saveRDS(pulchra_cover_max, file = "outputs/models/pulchra_cover_max.rds")
pulchra_cover_max <- readRDS("outputs/models/pulchra_cover_max.rds")
# QHI estimate =  0.002518971 
# toolik = 0.002518971  + 0.016781174  
# mean of slope both sites= 0.01090955

# max and means
pulchra_cover_mean <- brms::brm(mean_cov ~ Year_index * SITE + (1|Year_index),
                               data = mean, family = "beta", chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_cover_mean) # mean year estimate for both sites 0.00677882
pp_check(pulchra_cover_mean, ndraws=100)
plot_model(pulchra_cover_mean, type = "pred", terms = c("Year_index ", "SITE")) # to visualise interaction
saveRDS(pulchra_cover_mean, file = "outputs/models/pulchra_cover_mean.rds")
pulchra_cover_mean <- readRDS("outputs/models/pulchra_cover_mean.rds")

pulchra_cover_mean_summ <- model_summ_cov(pulchra_cover_mean)
rownames(pulchra_cover_mean_summ) <- c("Intercept ", "Year (indexed) ", "Toolik site ", "Year (indexed):Toolik site",
                                       "Random intercept", "phi")
pulchra_cover_mean_summ$Rhat <- as.character(formatC(pulchra_cover_mean_summ$Rhat, digits = 2, format = 'f'))

pulchra_cover_mean_summ <- pulchra_cover_mean_summ %>%
  mutate("Scenario" = "natural",
         "Response variable" = "Mean cover") %>% 
  relocate("Scenario", .before = "Estimate") %>%
  relocate("Response variable", .before = "Scenario")

# Extracting outputs
cov_time_pul_fix <- as.data.frame(fixef(pulchra_cover_max)) # extract fixed eff. slopes 
cov_time_pul_random <- as.data.frame(ranef(pulchra_cover_mean)) # extract random eff. slopes 
cov_time_pul_coef <- as.data.frame(coef(pulchra_cover_max, summary = TRUE, robust = FALSE, 
                                        probs = c(0.025, 0.975))) # extract combined coeff (random and fixed)

cov_time_pul_random$SiteSubsitePlot <- row.names(cov_time_pul_random) # Apply row.names function
rownames(cov_time_pul_random) <- NULL
colnames(cov_time_pul_random)[5] <- "sitesubsiteplot_index_year_estimate" 
colnames(cov_time_pul_random)[6] <- "sitesubsiteplot_index_year_error" 
colnames(cov_time_pul_random)[7] <- "sitesubsiteplot_index_year_Q_25" 
colnames(cov_time_pul_random)[8] <- "sitesubsiteplot_index_year_Q_97"

cov_time_pul_random_new <- cov_time_pul_random %>%
  dplyr::select("SiteSubsitePlot","sitesubsiteplot_index_year_estimate" ,"sitesubsiteplot_index_year_error" ,
                "sitesubsiteplot_index_year_Q_25" ,"sitesubsiteplot_index_year_Q_97")
cov_time_pul_random_new$Site <- ifelse(grepl("QHI", cov_time_pul_random_new$SiteSubsitePlot), "QHI",
                                                      ifelse(grepl("TOOLIK", cov_time_pul_random_new$SiteSubsitePlot), "TOOLIK" , NA))
view(cov_time_pul_random_new)

# single site models
# QHI only
mean_QHI <- mean %>%
  dplyr::filter(SITE == "QHI")

max_QHI <- max %>%
  dplyr::filter(SITE == "QHI")

pulchra_cover_mean_QHI <- brms::brm(mean_cov ~ Year_index + (1|Year_index),
                                data = mean_QHI, family = "beta", chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(pulchra_cover_mean_QHI, file = "outputs/models/pulchra_cover_mean_QHI.rds")
pulchra_cover_mean_QHI_summ <- model_summ_cov(pulchra_cover_mean_QHI)
rownames(pulchra_cover_mean_QHI_summ) <- c("Intercept   ", "Year (indexed)            ",
                                       "Random intercept  ", "phi   ")
pulchra_cover_mean_QHI_summ$Rhat <- as.character(formatC(pulchra_cover_mean_QHI_summ$Rhat, digits = 2, format = 'f'))

pulchra_cover_mean_QHI_summ <- pulchra_cover_mean_QHI_summ %>%
  mutate("Site" = "QHI", "Scenario" = "Natural",
         "Response variable" = "Mean cover") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")

pulchra_cover_max_QHI <- brms::brm(max_cov ~ Year_index + (1|Year_index),
                                    data = max_QHI, family = "beta", chains = 3,
                                    iter = 5000, warmup = 1000, 
                                    control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(pulchra_cover_max_QHI, file = "outputs/models/pulchra_cover_max_QHI.rds")

pulchra_cover_max_QHI_summ <- model_summ_cov(pulchra_cover_max_QHI)
rownames(pulchra_cover_max_QHI_summ) <- c(" Intercept  ", "Year (indexed)  ",
                                           "Random intercept ", "phi ")
pulchra_cover_max_QHI_summ$Rhat <- as.character(formatC(pulchra_cover_max_QHI_summ$Rhat, digits = 2, format = 'f'))

pulchra_cover_max_QHI_summ <- pulchra_cover_max_QHI_summ %>%
  mutate("Site" = "QHI", "Scenario" = "Natural",
         "Response variable" = "Max cover") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")

summary(pulchra_cover_mean_QHI) # 0.01686414
summary(pulchra_cover_max_QHI)
cov_time_pul_fix_QHI <- as.data.frame(fixef(pulchra_cover_mean_QHI)) # extract fixed eff. slopes 
cov_time_pul_fix_QHI_max <- as.data.frame(fixef(pulchra_cover_max_QHI)) # extract fixed eff. slopes 


# TOOLIK ONLY
mean_toolik <- mean %>%
  dplyr::filter(SITE == "TOOLIK")

max_toolik <- max %>%
  dplyr::filter(SITE == "TOOLIK")

pulchra_cover_mean_toolik <- brms::brm(mean_cov ~ Year_index + (1|Year_index),
                                    data = mean_toolik, family = "beta", chains = 3,
                                    iter = 5000, warmup = 1000, 
                                    control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(pulchra_cover_mean_toolik, file = "outputs/models/pulchra_cover_mean_toolik.rds")

pulchra_cover_mean_toolik_summ <- model_summ_cov(pulchra_cover_mean_toolik)
rownames(pulchra_cover_mean_toolik_summ) <- c("Intercept     ", "  Year (indexed) ",
                                           "  Random intercept ", "phi  ")
pulchra_cover_mean_toolik_summ$Rhat <- as.character(formatC(pulchra_cover_mean_toolik_summ$Rhat, digits = 2, format = 'f'))

pulchra_cover_mean_toolik_summ <- pulchra_cover_mean_toolik_summ %>%
  mutate("Site" = "Toolik", "Scenario" = "Natural",
         "Response variable" = "Mean cover") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
  relocate("Scenario", .before = "Site")
  

pulchra_cover_max_toolik <- brms::brm(max_cov ~ Year_index + (1|Year_index),
                                       data = max_toolik, family = "beta", chains = 3,
                                       iter = 5000, warmup = 1000, 
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))
saveRDS(pulchra_cover_max_toolik, file = "outputs/models/pulchra_cover_max_toolik.rds")
pulchra_cover_max_toolik_summ <- model_summ_cov(pulchra_cover_max_toolik)
rownames(pulchra_cover_max_toolik_summ) <- c(" Intercept  ", " Year (indexed) ",
                                              " Random intercept ", " phi ")
pulchra_cover_max_toolik_summ$Rhat <- as.character(formatC(pulchra_cover_max_toolik_summ$Rhat, digits = 2, format = 'f'))

pulchra_cover_max_toolik_summ <- pulchra_cover_max_toolik_summ %>%
  mutate("Site" = "Toolik", "Scenario"="Natural",
         "Response variable" = "Max cover", 
         "Scenario" = "Natural") %>% 
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site")%>%
relocate("Scenario", .before = "Site")

all_natural_cov <- rbind(pulchra_cover_max_QHI_summ, pulchra_cover_mean_QHI_summ, 
                         pulchra_cover_max_toolik_summ, pulchra_cover_mean_toolik_summ)

ALL_MAIN_MODELS <- rbind(bind_with_cg, all_natural_cov)
summary(pulchra_cover_mean_toolik) # 0.003300622
summary(pulchra_cover_max_toolik) 
cov_time_pul_fix_toolik <- as.data.frame(fixef(pulchra_cover_mean_toolik)) # extract fixed eff. slopes 
cov_time_pul_fix_toolik_max <- as.data.frame(fixef(pulchra_cover_max_toolik)) # extract fixed eff. slopes 

# salix arctica -----
arctica_cover <- brms::brm(cover_prop ~ I(YEAR-1996)+(I(YEAR-1996)|SiteSubsitePlot),
                           data = itex_EZ_arctica, family = gaussian(),
                           chains = 3,iter = 5000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(arctica_cover)
plot(arctica_cover)
pp_check(arctica_cover, type = "dens_overlay", nsamples = 100)

# Extracting outputs
cov_time_arc_fix <- as.data.frame(fixef(arctica_cover)) # extract fixed eff. slopes 
cov_time_arc_random <- as.data.frame(ranef(arctica_cover)) # extract random eff. slopes 
cov_time_arc_coef <- as.data.frame(coef(arctica_cover, summary = TRUE, robust = FALSE, 
                                        probs = c(0.025, 0.975))) # extract combined coeff (random and fixed)

cov_time_arc_random$SiteSubsitePlot <- row.names(cov_time_arc_random) # Apply row.names function
rownames(cov_time_arc_random) <- NULL
colnames(cov_time_arc_random)[5] <- "sitesubsiteplot_index_year_estimate" 
colnames(cov_time_arc_random)[6] <- "sitesubsiteplot_index_year_error" 
colnames(cov_time_arc_random)[7] <- "sitesubsiteplot_index_year_Q_25" 
colnames(cov_time_arc_random)[8] <- "sitesubsiteplot_index_year_Q_97"

cov_time_arc_random_new <- cov_time_arc_random %>%
  dplyr::select("SiteSubsitePlot","sitesubsiteplot_index_year_estimate" ,"sitesubsiteplot_index_year_error" ,
                "sitesubsiteplot_index_year_Q_25" ,"sitesubsiteplot_index_year_Q_97")
cov_time_arc_random_new$Site <- ifelse(grepl("QHI", cov_time_arc_random_new$SiteSubsitePlot), "QHI",
                                       ifelse(grepl("ANWR", cov_time_arc_random_new$SiteSubsitePlot), "ANWR" , NA))
view(cov_time_arc_random_new)


# data visualisation ------
# one line per site
(pulchra_cover_plot <- itex_EZ_pulchra_max %>%
  group_by(SITE) %>%
   add_predicted_draws(pulchra_cover_max) %>%
   ggplot(aes(x = YEAR, y = cover_prop, color = ordered(SITE), fill = ordered(SITE))) +
   stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
   geom_point(data = itex_EZ_pulchra_max) +
   scale_fill_brewer(palette = "Set2") +
   scale_color_brewer(palette = "Dark2") +
   ylab("Salix pulchra cover \n") +
   xlab("\nYear") +
   theme_shrub() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black"))) 

(arctica_cover_plot <- itex_EZ_arctica %>%
    group_by(SITE) %>%
    add_predicted_draws(arctica_cover) %>%
    ggplot(aes(x = YEAR, y = cover_prop, color = ordered(SITE), fill = ordered(SITE))) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = itex_EZ_arctica) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix arctica cover \n") +
    xlab("\nYear") +
    theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

# mean and max plot
library(tidybayes)
library(brms)
cov_mean <- (conditional_effects(pulchra_cover_mean))
cov_mean_dat <- cov_mean[[1]]

(pulchra_cov_plot_mean <-ggplot(cov_mean_dat) +
    geom_point(data = pulchra_yearly_mean, aes(x = Year_index, y = cover_prop_mean, colour = Site),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("PLOT MEAN Salix pulchra cover (prop)\n") +
    xlab("\n Year (scaled)" ) +
    #ylim(0, 30) +
    scale_color_brewer(palette = "Greys")+
    scale_fill_brewer(palette = "Greys")+
    theme_classic())

(pulchra_cover_plot <- pulchra_yearly_max %>%
    group_by(SITE) %>%
    add_predicted_draws(pulchra_cover_max) %>%
    ggplot(aes(x = Year_index, y = cover_prop_max, color = ordered(SITE), fill = ordered(SITE))) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = pulchra_yearly_max) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra cover \n") +
    xlab("\nYear") +
    # theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 


(pulchra_cover_plot_mean <- meanmax %>%
    group_by(SITE) %>%
    add_predicted_draws(pulchra_cover_mean) %>%
    add_predicted_draws(pulchra_cover_max) %>%
    ggplot(aes(x = Year_index, y = mean_cov, color = ordered(SITE), fill = ordered(SITE))) +
    ggplot(aes(x = Year_index, y = max_cov, color = ordered(SITE), fill = ordered(SITE))) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = mean) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra cover mean \n") +
    xlab("\nYear") +
    ylim(0, 0.8)+
    theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

(pulchra_cover_plot_max <- max %>%
    group_by(SITE) %>%
    add_predicted_draws(pulchra_cover_max) %>%
    ggplot(aes(x = Year_index, y = max_cov, color = ordered(SITE), fill = ordered(SITE))) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = max) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra cover max\n") +
    xlab("\nYear") +
    theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

library(gridExtra)
grid.arrange(pulchra_cover_plot_max, pulchra_cover_plot_mean, nrow=1)

# trying to plot both models in one graph
cov_mean_2 <- (conditional_effects(pulchra_cover_mean))
cov_max_3 <- (conditional_effects(pulchra_cover_max))
cov_mean_dat_2 <- cov_mean_2[[3]] # this cuts off toolik for some reason?
cov_max_dat_3 <- cov_max_3[[3]] # this cuts off toolik for some reason?
cov_all_dat <- full_join(cov_mean_dat_2,cov_max_dat_3, by = c("Year_index"="Year_index",
                                                              "SITE"="SITE", "cond__"= "cond__",
                                                              "effect1__"= "effect1__"))
(pulchra_cov_plot_meanmax <-ggplot(cov_all_dat) +
    geom_point(data = meanmax, aes(x = Year_index, y = mean_cov, colour = SITE),
               alpha = 0.5, show.legend = FALSE)+
    geom_point(data = meanmax, aes(x = Year_index, y = max_cov, colour = SITE, fill = SITE),
               alpha = 0.5, shape=23, show.legend = FALSE)+
    geom_line(aes(x = effect1__, y = estimate__.x, colour = SITE),
              linewidth = 1.5) +
    geom_line(aes(x = effect1__, y = estimate__.y,  colour = SITE),
              linewidth = 1.5, linetype = "dashed") +
   geom_ribbon(aes(x = effect1__, ymin = lower__.x, ymax = upper__.x,  fill = SITE),
              alpha = 0.1) +
    geom_ribbon(aes(x = effect1__, ymin = lower__.y, ymax = upper__.y,  fill = SITE),
                alpha = 0.1) +
    ylab("Plot mean and plot max Salix pulchra cover (prop)\n") +
    xlab("\n Year (scaled)" ) +
    scale_color_manual(values = c("QHI" = "#c00100", "TOOLIK" = "#332288"))+
    theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

# final graphs  ----
cov_mean_2 <- (conditional_effects(pulchra_cover_mean))
cov_max_3 <- (conditional_effects(pulchra_cover_max))
cov_mean_dat_2 <- cov_mean_2[[3]] 
cov_max_dat_3 <- cov_max_3[[3]]

(pul_mean_cover_plot <-ggplot(cov_mean_dat_2) +
    geom_point(data = mean, aes(x = Year_index, y = mean_cov, colour = SITE),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__, colour = SITE),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__, fill = SITE),
                alpha = .1) +
    #ylim(0, 1.00)+
    ylab("Mean cover (proportion)\n") +
    xlab("\n Year (scaled)" ) +
    scale_color_manual(values = c("QHI" = "#e75480", "TOOLIK" = "#2b788c"))+
    scale_fill_manual(values = c("QHI" = "#e75480", "TOOLIK" = "#2b788c"))+
   # scale_fill_brewer(palette = "Set2") +
   # scale_color_brewer(palette = "Dark2") +
    theme_shrub() + theme(legend.position = "none")+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))
  #+ theme(legend.position = c(0.95, 0.95)))

(pul_max_cover_plot <-ggplot(cov_max_dat_3) +
    geom_point(data = max, aes(x = Year_index, y = max_cov, colour = SITE),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__, colour = SITE),
              linewidth = 1.5) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__, fill = SITE),
                alpha = .1) +
    ylab("Max cover (proportion)\n") +
    xlab("\n Year (scaled)" ) +
   # ylim(0, 1.00) +
    scale_color_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    scale_fill_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    theme_shrub() + theme(legend.position = "none") +
    theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))


panel_coverheight_max <- grid.arrange(pulchra_height_plot_max,pul_max_cover_plot,nrow=1)
panel_coverheight_mean <- grid.arrange(pulchra_height_plot_mean,pul_mean_cover_plot, nrow=1)
ggsave(panel_coverheight_max, filename ="outputs/figures/panel_coverheight_max.png", width = 14.67, height = 6.53, units = "in")
ggsave(panel_coverheight_mean, filename ="outputs/figures/panel_coverheight_mean.png", width = 14.67, height = 6.53, units = "in")


panel_coverheight_meanmax <- grid.arrange(pulchra_height_plot_mean,pul_mean_cover_plot, pulchra_height_plot_max,
                                          pul_max_cover_plot, nrow=2)

panel_coverheight_meanmax_cg <- grid.arrange(pulchra_height_plot_mean,pul_mean_cover_plot, pulchra_height_plot_max,
                                             pul_max_cover_plot, pul_height_plot, pul_cover_plot, nrow=3)
ggsave(panel_coverheight_meanmax_cg, filename = "outputs/figures/panel_coverheight_meanmax_cg.png", width = 8.27, height = 11.69)

#common legend
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('QHI', 'TOOLIK', 'CG'), pch=16, pt.cex=2, cex=1.5, bty='n',
       col = c('#e75480', 'black', '#5ccc64'), text(family="Helvetica Light"))
mtext("Site", at=0.2, cex=2, family = "Helvetica Light")

# new graphs
max$SITE <- as.factor(max$SITE)
(pul_max_cover_plot_new <- max %>%
    group_by(SITE) %>%
    add_predicted_draws(pulchra_cover_max, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Year_index, y = max_cov, colour = SITE)) +
    stat_lineribbon(aes(y = .prediction, fill = SITE), .width = c(.50), alpha = 0.2) +
    geom_point(data = max, alpha = 0.5) +
    ylab("Max cover (proportion)\n") +
    xlab("\n Year (scaled)" ) +
    # ylim(0, 1.00) +
    scale_color_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    scale_fill_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    theme_shrub() + theme(legend.position = "none") +
    theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))

mean$SITE <- as.factor(mean$SITE)
(pul_mean_cover_plot_new <- mean %>%
    group_by(SITE) %>%
    add_predicted_draws(pulchra_cover_mean, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Year_index, y = mean_cov, colour = SITE)) +
    stat_lineribbon(aes(y = .prediction, fill = SITE), .width = c(.50), alpha = 0.2) +
    geom_point(data = mean, alpha = 0.5) +
    ylab("Mean cover (proportion)\n") +
    xlab("\n Year (scaled)" ) +
    # ylim(0, 1.00) +
    scale_color_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    scale_fill_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    theme_shrub() + theme(legend.position = "none") +
    theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))

panel_coverheight_max_new <- grid.arrange(pulchra_height_plot_max_new,pul_max_cover_plot_new,nrow=1)
panel_coverheight_mean_new <- grid.arrange(pulchra_height_plot_mean_new,pul_mean_cover_plot_new, nrow=1)
ggsave(panel_coverheight_max_new, filename ="outputs/figures/panel_coverheight_max.png", width = 14.67, height = 6.53, units = "in")
ggsave(panel_coverheight_mean_new, filename ="outputs/figures/panel_coverheight_mean.png", width = 14.67, height = 6.53, units = "in")

# FINAL GRAPHS ------
ggpred_cov_mean <- ggpredict(pulchra_cover_mean, terms = c("Year_index", "SITE"))
colnames(ggpred_cov_mean) = c('Year_index', 'fit', 'lwr', 'upr', 'SITE')
# actual mean cover value at year 10 toolik: 0.09632101
# actual mean cover value at year 34 toolik:0.08991054

# actual mean cover value at year 10 qhi:0.14057432
# actual mean cover value at year 34 qhi:0.19638900


(QHI_TOOLIK_mean_cover <-ggplot(ggpred_cov_mean) +
    geom_point(data = mean, aes(x = Year_index, y = mean_cov, colour = SITE),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Year_index , y = fit,colour = SITE), linewidth = 1)+
    geom_ribbon(aes(x = Year_index, ymin = lwr, ymax = upr,fill = SITE),
                  alpha = 0.1) +
    ylab("Mean cover (proportion)\n") +
    xlab("\n Year (scaled)" ) +
    # ylim(0, 1.00) +
    scale_color_manual(values = c("TOOLIK" = "#2b788c", "QHI" = "#e75480"))+
    scale_fill_manual(values = c("TOOLIK" = "#2b788c", "QHI" = "#e75480"))+
    theme_shrub() + theme(legend.position = "none") +
    theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))

ggpred_cov_max <- ggpredict(pulchra_cover_max, terms = c("Year_index", "SITE"))
colnames(ggpred_cov_max) = c('Year_index', 'fit', 'lwr', 'upr', 'SITE')

# actual max cover value at year 10 toolik: 0.3605562
# actual max cover value at year 34 toolik:0.4741059

# actual max cover value at year 10 qhi:0.2699834
# actual max cover value at year 34 qhi:0.2822647

(QHI_TOOLIK_max_cover <-ggplot(ggpred_cov_max) +
    geom_point(data = max, aes(x = Year_index, y = max_cov, colour = SITE),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Year_index , y = fit,colour = SITE), linewidth = 1)+
    geom_ribbon(aes(x = Year_index, ymin = lwr, ymax = upr,fill = SITE),
                alpha = 0.1) +
    ylab("Max. cover (proportion)\n") +
    xlab("\n Year (scaled)" ) +
    # ylim(0, 1.00) +
    scale_color_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    scale_fill_manual(values = c("QHI" = "#2b788c", "TOOLIK" = "#e75480"))+
    theme_shrub() + theme(legend.position = "none") +
    theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))

ggpred_panel_coverheight_max_new <- grid.arrange(ggrped_MAX_qhi_plot,QHI_TOOLIK_max_cover,nrow=1)
ggpred_panel_coverheight_mean_new <- grid.arrange(ggrped_MEAN_qhi_plot, QHI_TOOLIK_mean_cover, nrow=1)
ggsave(ggpred_panel_coverheight_max_new, filename ="outputs/figures/ggpred_panel_coverheight_max_new.png", width = 14.67, height = 6.53, units = "in")
ggsave(ggpred_panel_coverheight_mean_new, filename ="outputs/figures/ggpred_panel_coverheight_mean_new.png", width = 14.67, height = 6.53, units = "in")
