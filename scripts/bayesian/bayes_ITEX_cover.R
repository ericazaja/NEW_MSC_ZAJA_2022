# BAYESIAN models for itex cover analysis
# script by Erica
# last update: 21/02/2023

# libraries ------
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)

# load data ------
itex_EZ_shrubs_2023 <- read_csv("data/ITEX/itex_EZ_shrubs_2023.csv")

# wrangle data -----
itex_EZ_shrubs_2023 <- itex_EZ_shrubs_2023 %>%
  mutate(cover_prop = RelCover/100)

# divide species 
itex_EZ_arctica <- itex_EZ_shrubs_2023 %>%
  filter(SPECIES_NAME == "Salix arctica") %>%
  filter(SITE %in% c("ANWR", "QHI" ))

itex_EZ_pulchra <- itex_EZ_shrubs_2023 %>%
  filter(SPECIES_NAME == "Salix pulchra")

hist(itex_EZ_shrubs_2023$cover_prop, breaks = 30)
hist(itex_EZ_arctica$cover_prop, breaks = 20) # NOT zero inflated
hist(itex_EZ_pulchra$cover_prop, breaks = 30) # YES zero inflated
range(itex_EZ_arctica$cover_prop) # 0.0078125 0.3861386
range(itex_EZ_pulchra$cover_prop) #  0.006944444 0.769230769
range(itex_EZ_shrubs_2023$cover_prop)
# 0.006944444 0.769230769
# values between 0.0001 and 0.9999 ! So beta distribution

range(itex_EZ_pulchra$YEAR)
range(itex_EZ_arctica$YEAR)

# filter out plots that dont have at least 3 years of repeat measure
itex_EZ_arctica <- itex_EZ_arctica %>%
  filter(PLOT <=6)

itex_EZ_pulchra <- itex_EZ_pulchra %>%
  filter(PLOT != 15)  %>%
  filter(PLOT != 28)   %>%
  filter(PLOT != 35)   %>%
  filter(PLOT != 40) %>%
filter(PLOT != 58)  %>%
  filter(PLOT != 65)   %>%
  filter(PLOT != 68)  %>%
  filter(PLOT != 69)  %>%
  filter(PLOT != 82)%>%
  filter(SUBSITE != "KO")

itex_EZ_arctica$SITE <- as.factor(itex_EZ_arctica$SITE)
itex_EZ_pulchra$SITE <- as.factor(itex_EZ_pulchra$SITE)
itex_EZ_pulchra$SiteSubsitePlot <- as.factor(itex_EZ_pulchra$SiteSubsitePlot)
itex_EZ_arctica$SiteSubsitePlot <- as.factor(itex_EZ_arctica$SiteSubsitePlot)
itex_EZ_pulchra$SitePlotYear<- with(itex_EZ_pulchra, paste0(SITE, PLOT, YEAR))
itex_EZ_pulchra$SitePlot<- with(itex_EZ_pulchra, paste0(SITE, PLOT))

# keeping max values 

itex_EZ_pulchra_max<-  itex_EZ_pulchra %>%
  group_by(SiteSubsitePlotYear) %>%
  slice(which.max(cover_prop)) %>%
  distinct()

view(itex_EZ_pulchra_max)
(plot <- ggplot(itex_EZ_pulchra_max) +
  geom_point(aes(x =YEAR , y = cover_prop, color= SITE, fill =SITE)))


# modelling cover over time -----

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

# salix pulchra -----
pulchra_cover <- brms::brm(cover_prop ~ I(YEAR-1988)+ (I(YEAR-1988)|SiteSubsitePlot),
                      data = itex_EZ_pulchra, family = "beta", chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_cover) 
plot(pulchra_cover)
pp_check(pulchra_cover, type = "dens_overlay", nsamples = 100) 

pulchra_cover_max <- brms::brm(cover_prop ~ I(YEAR-1988)+ (I(YEAR-1988)|SiteSubsitePlot),
                           data = itex_EZ_pulchra_max, family = "beta", chains = 3,
                           iter = 5000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(pulchra_cover_max)

# Extracting outputs
cov_time_pul_fix <- as.data.frame(fixef(pulchra_cover)) # extract fixed eff. slopes 
cov_time_pul_random <- as.data.frame(ranef(pulchra_cover)) # extract random eff. slopes 
cov_time_pul_coef <- as.data.frame(coef(pulchra_cover, summary = TRUE, robust = FALSE, 
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


