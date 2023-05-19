# BAYESIAN: CG height and stem elongation over time 
#### Script by Erica Zaja, created 27/02/23
### Last updated: 27/02/23

# Libraries ------
library(tidybayes)
library(readr)
library(brms)
library(tidyverse)
library(gridExtra)
library(kableExtra)
library(knitr)
library(ggpubr)


# funciton to extract model summary
model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Sample_age # change name of random effect here 
  random = sum$random$SampleID_standard # change name of random effect here 
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
 # fixed$effect <- "population"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
 # random$effect <- "SampleID_standard"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "random" # could change rowname here of random effect if you'd like 
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

# DATA -------
all_CG_growth <- read_csv("data/common_garden_shrub_data/all_CG_growth.csv")

# Wrangle -------
# reclassing variables
all_CG_growth$Species <- as.factor(all_CG_growth$Species)
all_CG_growth$SampleID_standard <- as.factor(all_CG_growth$SampleID_standard)
all_CG_growth$population <- as.factor(all_CG_growth$population)
all_CG_growth$Site <- as.factor(all_CG_growth$Site)
all_CG_growth$Sample_Date <- as.POSIXct(all_CG_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_growth$Year <- as.factor(all_CG_growth$Year)

# Species specific ------
all_CG_growth_ric <- all_CG_growth %>%
  filter(Species == "Salix richardsonii")
  
all_CG_growth_pul<-  all_CG_growth%>%
  filter(Species == "Salix pulchra")

all_CG_growth_arc <-all_CG_growth%>%
  filter(Species == "Salix arctica")

# Only southern populations
all_CG_growth_ric_south <- all_CG_growth_ric %>%
  filter(population == "Southern")

all_CG_growth_pul_south <- all_CG_growth_pul %>%
  filter(population == "Southern")

all_CG_growth_arc_south <- all_CG_growth_arc %>%
  filter(population == "Southern") %>%
  filter(Canopy_Height_cm>=0)

# MODELLING 

# HEIGHT over time ------
# Salix richardsonii -------

height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(height_rich, file = "outputs/models/height_rich.rds")
height_rich <- readRDS("outputs/models/height_rich.rds")

library(ggeffects)
ggpred_height_ric <- ggpredict(height_rich, terms = c("Sample_age", "population"))
colnames(ggpred_height_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_height_rich_plot <-ggplot(ggpred_height_ric) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    ggtitle(expression(italic("Salix richardsonii"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 


# extracting model summary
height_rich_summ <- model_summ(height_rich)
height_rich_summ$Species <- "Salix richardsonii"
height_rich_summ <- height_rich_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")

# change estimates by adding estimate to other rows 
#height_rich_summ[3,1] <- height_rich_summ[3,1] + height_rich_summ[1,1]
#height_rich_summ[4,1] <- height_rich_summ[4,1] + height_rich_summ[2,1]

# change lower CI by adding 
#height_rich_summ[3,3] <- height_rich_summ[3,3] + height_rich_summ[1,3]
#height_rich_summ[4,3] <- height_rich_summ[4,3] + height_rich_summ[2,3]

# change upper CI
#height_rich_summ[3,4] <- height_rich_summ[3,4] + height_rich_summ[1,4]
#height_rich_summ[4,4] <- height_rich_summ[4,4] + height_rich_summ[2,4]

#height_rich_summ <- height_rich_summ[c(1:4),] # this removes the random effects

# estimate for northern sample age: 1.50+0.09*1 = exp(1.59) = 4.903749 cm, in year 1
# estimate for southern sample age: (1.50+0.91)+(0.09*1+0.16*1) = exp(2.66) = 14.29629 in year 1
# estimate for southern sample age in year 9: (1.50+0.91)+(0.09*9 +0.16*9) = exp(4.66) = 105.6361 in year 9 -->105.6361/9 = 11.73734 cm/year
# estimate for n sample age in year 9: 1.50+0.09*9 = exp(2.31) = 10.07442 in year 9 --> 10.07442/9= 1.11938 cm/year


# estimate for year N.:  exp(0.09) = 1.09
# estimate for year S.:  0.09*year +0.16*year = exp(0.25) = 1.28

# %diff
(14.29629-4.903749)/4.903749
# 2%

# times larger
14.29629/4.903749
# 2.91538


rownames(height_rich_summ) <- c("Intercept", "Sample age", "Southern population"
                           , "Sample age:Southern population", "Random intercept", 
                           "sd(Sample age)", "cor(Intercept, Sample age)", "sigma")

height_rich_summ$Rhat <- as.character(formatC(height_rich_summ$Rhat, digits = 2, format = 'f'))

height_rich_summ <- height_rich_summ %>%
  #mutate(Species = "Salix richardsonii")%>%
  relocate("Species", .before = "Estimate")

# only southern random slopes
max(all_CG_growth_ric_south$Canopy_Height_cm, na.rm=TRUE) # 127
mean(all_CG_growth_ric_south$Canopy_Height_cm, na.rm=TRUE) # 127

# truncating to max richardsonii height from usda.gov (450cm, log(450)=6.109)
height_rich_south <- brms::brm(log(Canopy_Height_cm)|trunc(lb = 0, ub = 6.109248) ~ Sample_age+(Sample_age|SampleID_standard),
                         data = all_CG_growth_ric_south,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich_south) # significant height growth over time
plot(height_rich_south)
pp_check(height_rich_south, type = "dens_overlay", ndraws = 100) 



# Salix pulchra -----
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(height_pul, file = "outputs/models/height_pul.rds")
height_pul <- readRDS("outputs/models/height_pul.rds")

summary(height_pul)

# estimate for northern sample age: 1.99-0.02=1.97 --> exp(1.97)= 7.170676 cm in year 1
# estimate for s sample age: (1.99+0.95) + (-0.02+0.03)= 2.95, exp(2.95)= 19.10595 cm per sample age
# estimate for s. sample age at year 9: (1.99+0.95) + (-0.02*9+0.03*9)= 3.03, exp(3.03)= 20.69 in year 9 --> 2.299692 per year, 2.30969
# estimate for northern sample age at year 9: 1.99-0.02*9=1.97= 3.03, exp(3.03)= 6.110447 in year 9 --> 0.6789386 per year


ggpred_height_pul <- ggpredict(height_pul, terms = c("Sample_age", "population"))
colnames(ggpred_height_pul) = c('Sample_age','fit', 'lwr', 'upr',"population")


(ggpred_height_pul_plot <-ggplot(ggpred_height_pul) +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    ggtitle(expression(italic("Salix pulchra"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

# only south--
ggpred_height_pul_south <- ggpred_height_pul %>%
  filter(population=="Southern")

(ggpred_height_pul_plot <-ggplot(ggpred_height_pul_south) +
    geom_point(data = all_CG_growth_pul_south, aes(x = Sample_age, y = Canopy_Height_cm), colour = "#98d83b",
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit), colour ="#98d83b", linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr), fill = "#98d83b",
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    ggtitle(expression(italic("Salix pulchra"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme(axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

# times larger
19.10595/7.170676
# 2.664456

height_pul_summ <- model_summ(height_pul)

rownames(height_pul_summ) <- c("Intercept      ", "Sample age      ", "Southern population "
                                , "Sample age:Southern population ", "Random intercept ", 
                              "sd(Sample age) ", "cor(Intercept, Sample age) ", "sigma ")
height_pul_summ$Rhat <- as.character(formatC(height_pul_summ$Rhat, digits = 2, format = 'f'))

height_pul_summ <- height_pul_summ %>%
  mutate(Species = "Salix pulchra")%>%
 relocate("Species", .before = "Estimate")

#height_pul_ric <- rbind(height_rich_summ, height_pul_summ)
#height_pul_summ$Species <- "Salix pulchra"
#height_pul_summ <- height_pul_summ %>%
  # mutate(Species = "Salix pulchra")%>%
 # relocate("Species", .before = "Estimate")

height_pul_summ <- height_pul_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")

# change estimates by adding estimate to other rows 
#height_pul_summ[3,1] <- height_pul_summ[3,1] + height_pul_summ[1,1]
#height_pul_summ[4,1] <- height_pul_summ[4,1] + height_pul_summ[2,1]

# change lower CI by adding 
#height_pul_summ[3,3] <- height_pul_summ[3,3] + height_pul_summ[1,3]
#height_pul_summ[4,3] <- height_pul_summ[4,3] + height_pul_summ[2,3]

# change upper CI
#height_pul_summ[3,4] <- height_pul_summ[3,4] + height_pul_summ[1,4]
#height_pul_summ[4,4] <- height_pul_summ[4,4] + height_pul_summ[2,4]

#height_pul_summ <- height_pul_summ[c(1:4),]

# only southern
# truncating to max height of pulchra 160 cm = log(160 )
height_pul_south <- brms::brm(log(Canopy_Height_cm) |trunc(lb = 0, ub =5.075174) ~ Sample_age+(Sample_age|SampleID_standard),
                               data = all_CG_growth_pul_south,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul_south) # 
plot(height_pul_south)
pp_check(height_pul_south, type = "dens_overlay", ndraws = 100) 
saveRDS(height_pul_south, file = "outputs/models/height_pul_south.rds")
height_pul_south <- readRDS("outputs/models/height_pul_south.rds")
# estimate for s. sample age 9: 2.96+(0.003288811*9)  = 2.989599= exp(2.989599)=  19.87771 --> 19.87771/9 =2.208634 cm /year

height_pul_south_summ <- model_summ(height_pul_south)
rownames(height_pul_south_summ) <- c("Intercept          ", "Sample age            ", "Random intercept         ", 
                            "sd(Sample age) ", "cor(Intercept, Sample age)    ", "phi       ")
height_pul_south_summ$Rhat <- as.character(formatC(height_pul_south_summ$Rhat, digits = 2, format = 'f'))

height_pul_south_summ <- height_pul_south_summ %>%
  mutate("Site" = "CG", "Scenario"="Novel",
         "Response variable" = "Canopy height") %>% 
  mutate("Estimate_back" = exp(Estimate),
         "Error_back"= exp(Est.Error))%>%
  select(-"Estimate", -"Est.Error")%>%
  dplyr::rename("Estimate" ="Estimate_back","Est.Error"= "Error_back") %>%
  relocate("Est.Error", .before = "l-95% CI")%>%
  relocate("Estimate", .before = "Est.Error")%>%
  relocate("Site", .before = "Estimate") %>%
  relocate("Response variable", .before = "Site") %>%
  relocate("Scenario", .before = "Site") 

cg_models_bind <- rbind(cov_pul_summ, height_pul_south_summ)


# Salix arctica -------
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
saveRDS(height_arc, file = "outputs/models/height_arc.rds")
height_arc <- readRDS("outputs/models/height_arc.rds")

summary(height_arc) # significant growth over time
# estimate for northern sample age 9: 0.86+0.08*9= exp(1.58) = 4.854956 -->0.5394396
# estimate for s. sample age: 0.86-0.28 + 0.08*9 + 0.11*9 = 2.29= exp(2.29)=  9.874938 --> 1.097215

ggpred_height_arc <- ggpredict(height_arc, terms = c("Sample_age", "population"))
colnames(ggpred_height_arc) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_height_arc_plot <-ggplot(ggpred_height_arc) +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    ggtitle(expression(italic("Salix arctica"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

ggpred_CG_height_panel <- ggarrange(ggpred_height_rich_plot,
                                    ggpred_height_pul_plot, 
                                    ggpred_height_arc_plot, nrow = 1,
                             common.legend = TRUE, legend="none")

ggsave(ggpred_CG_height_panel, filename ="outputs/figures/ggpred_CG_height_panel.png", width = 14.67, height = 6.53, units = "in")

plot(height_arc)
pp_check(height_arc, type = "dens_overlay", nsamples = 100) 

height_arc_summ <- model_summ(height_arc)

height_arc_summ$Species <- "Salix arctica"
height_arc_summ <- height_arc_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")

# change estimates by adding estimate to other rows 
#height_arc_summ[3,1] <- height_arc_summ[3,1] + height_arc_summ[1,1]
#height_arc_summ[4,1] <- height_arc_summ[4,1] + height_arc_summ[2,1]

# change lower CI by adding 
#height_arc_summ[3,3] <- height_arc_summ[3,3] + height_arc_summ[1,3]
#height_arc_summ[4,3] <- height_arc_summ[4,3] + height_arc_summ[2,3]

# change upper CI
#height_arc_summ[3,4] <- height_arc_summ[3,4] + height_arc_summ[1,4]
#height_arc_summ[4,4] <- height_arc_summ[4,4] + height_arc_summ[2,4]

rownames(height_arc_summ) <- c(" Intercept ", " Sample age ", " Southern population "
                               , " Sample age:Southern population ", " Random intercept ", 
                               " sd(Sample age) ", " cor(Intercept, Sample age) ", " sigma ")
height_arc_summ$Rhat <- as.character(formatC(height_arc_summ$Rhat, digits = 2, format = 'f'))

#height_arc_summ <- height_arc_summ[c(1:4),]

# binding all summaries
all_height_summ <- rbind(height_rich_summ,height_pul_summ,height_arc_summ)

#all_height_summ_back <- all_height_summ %>% 
 # dplyr::mutate("Estimate (back)" = exp(Estimate)) %>% 
 # dplyr::mutate("Lower 95% CI (back)" = exp(l_95_CI_log))%>%
  #dplyr::mutate("Upper 95% CI (back)" = exp(u_95_CI_log)) %>%
  #dplyr::rename("Est.Error (log)" = "Est.Error") %>%
  #dplyr::rename("Lower 95% CI (log)" = "l_95_CI_log") %>%
  #dplyr::rename("Upper 95% CI (log)" = "u_95_CI_log")
  

#all_height_summ_table <- all_height_summ_back %>% 
 # kbl(caption="Table. Heights over time of northern and southern shrubs in the common garden. ", 
  #    col.names = c("Species", "Estimate (log)", "Error (log)", "Lower 95% CI (log)", "Upper 95% CI (log)",
   #                 "Rhat", "Bulk effective sample size", "Tail effective sample size",
    #                "Effect", "Sample size", "Estimate (back)", "Lower 95% CI (back)", "Upper 95% CI (back)"), # give the column names you want making sure you have one name per column!
    #  digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
 # kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

all_height_summ_table <- all_height_summ %>% 
kbl(caption="Table. Heights over time of northern and southern shrubs in the common garden. ", 
col.names = c("Species", "Estimate (log)", "Error (log)", "Lower 95% CI (log)", "Upper 95% CI (log)",
               "Rhat", "Bulk effective sample size", "Tail effective sample size",
              "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
 kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

# optional: making specific column text in italics
column_spec(all_height_summ_table, 2, width = NULL, bold = FALSE, italic = TRUE) # 2 is my example column number 

save_kable(all_height_summ_table,file = "outputs/tables/kable_rich_pul_arc.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)

# only southern
# truncarte to max height 23 cm, log(23)= 3.135494, -1 lower bound because some small values when logged give negative number eg log(0.5)
height_arc_south <- brms::brm(log(Canopy_Height_cm)|trunc(lb = -1, ub =3.135494) ~ Sample_age+(Sample_age|SampleID_standard),
                              data = all_CG_growth_arc_south,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_arc_south) # 
plot(height_arc_south)
pp_check(height_arc_south, type = "dens_overlay", ndraws = 100) 

# DATA VISUALISATION -------
# HEIGHT -----
# Salix richardsonii ------
rich_height_1 <- (conditional_effects(height_rich))
rich_height_data <- rich_height_1[[1]]
rich_height_data_2 <- rich_height_1[[2]]

# this graph below only lets me plot one line
(rich_height_plot <-ggplot() +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = exp(estimate__)),
              linewidth = 1.5, data = rich_height_data) +
  geom_line(aes(x = effect1__, y = exp(estimate__)),
                               linewidth = 1.5, data = rich_height_data) +
    geom_ribbon(aes(x = effect1__, ymin = exp(lower__), ymax = exp(upper__)),
                alpha = .1, data = rich_height_data) +
    ylab("Richardsonii canopy height (log cm)\n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub())

# this works well 
(rich_heights_plot_new <- all_CG_growth_ric %>%
    group_by(population) %>%
    add_predicted_draws(height_rich, allow_new_levels = TRUE) %>%
    ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), colour = population)) +
    stat_lineribbon(aes(y = .prediction, fill = population), .width = c(.5), alpha = 1/4) +
    geom_point(data = all_CG_growth_ric, alpha = 0.5) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    theme_shrub() +
    ggtitle(expression(italic("Salix richardsonii"))) +
    ylab("Canopy height (cm)\n") +
    xlab("\nSample age") + theme(text=element_text(family="Helvetica Light")) )
    #theme(legend.position = "none"))

# Salix pulchra ------

(pul_heights_plot_new <- all_CG_growth_pul %>%
    group_by(population) %>%
    add_predicted_draws(height_pul, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Sample_age, y = Canopy_Height_cm, color = population)) +
    stat_lineribbon(aes(y = exp(.prediction), fill = population), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_pul, alpha = 0.5) +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
   theme_shrub() +
   ggtitle(expression(italic("Salix pulchra"))) +
   ylab("Canopy height (cm)\n") +
   xlab("\nSample age") + theme(text=element_text(family="Helvetica Light")) )
  # theme(legend.position = "none"))

(pul_heights_plot_new_south <- all_CG_growth_pul_south %>%
    #group_by(population) %>%
    add_predicted_draws(height_pul_south, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Sample_age, y = Canopy_Height_cm), color = "#5ccc64") +
    stat_lineribbon(aes(y = exp(.prediction)), colour = "#122814", fill = "#5ccc64",.width = c(.50), alpha = 0.2) +
    geom_point(data = all_CG_growth_pul_south, alpha = 0.5, colour = "#5ccc64") +
   # scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    #scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    theme_shrub() +
    ggtitle(expression(italic("Salix pulchra"))) +
    ylab("Canopy height (cm)\n") +
    xlab("\nSample age") + theme(text=element_text(family="Helvetica Light")) )
# theme(legend.position = "none"))

# Salix pulchra south ------
pul_height_1 <- (conditional_effects(height_pul_south))
pul_height_data_1 <- pul_height_1[[1]]
pul_height_data_back <- pul_height_data_1 %>%
  dplyr::mutate(canopy_height_back = exp(log(Canopy_Height_cm)))

(pul_height_plot <-ggplot(pul_height_data_back) +
    geom_point(data = all_CG_growth_pul_south, aes(x = Sample_age,y = Canopy_Height_cm),
               alpha = 0.5, colour = "#5ccc64")+
    geom_line(aes(x = effect1__, y = exp(estimate__)),
              linewidth = 1.5, colour = "#5ccc64") +
    geom_ribbon(aes(x = effect1__, ymin = exp(lower__), ymax = exp(upper__)),
                alpha = .1, fill = "#5ccc64") +
    labs(x = "\n Sample age",
         y = "Canopy height (cm)\n")+
    scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
    # ylab("Cover (/m2)\n") +
    # xlab("\n Sample age" ) +
    # scale_fill_brewer(palette = "orange") +
    scale_color_brewer(palette = "Dark2") +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0)))

pulchra_CG_panel <- grid.arrange(ggpred_height_pul_plot,ggpred_cover_pul_plot, nrow=1)
ggsave(pulchra_CG_panel, filename ="outputs/figures/pulchra_CG_panel.png", width = 14.67, height = 6.53, units = "in")


# Salix arctica------
(arc_heights_plot_new <- all_CG_growth_arc %>%
  group_by(population) %>%
   add_predicted_draws(height_arc, allow_new_levels = TRUE ) %>%
   ggplot(aes(x = Sample_age, y = Canopy_Height_cm, color = population)) +
   stat_lineribbon(aes(y = exp(.prediction), fill = population), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc, alpha = 0.5) +
   scale_x_continuous(breaks = seq(0, 9, by = 1)) + 
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_shrub() +
   ggtitle(expression(italic("Salix arctica"))) +
   ylab("Canopy height (cm)\n") +
   xlab("\nSample age") + theme(text=element_text(family="Helvetica Light")) )


CG_height_panel <- ggarrange(rich_heights_plot_new,
                                pul_heights_plot_new, 
                                arc_heights_plot_new, nrow = 1,
                                common.legend = TRUE, legend="none")

CG_height_panel

ggsave(CG_height_panel, filename ="outputs/figures/CG_height_panel.png", width = 14.67, height = 6.53, units = "in")


#common legend
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('Northern Garden', 'Southern Garden'), pch=16, pt.cex=2, cex=1.5, bty='n',
       col = c('#D55E00'), text(family="Helvetica Light"))
mtext("Site", at=0.2, cex=2, family = "Helvetica Light")

# STEM ELONG over time ------
# Salix richardsonii -------
elong_rich <- brms::brm(log(mean_stem_elong) ~ Sample_age*population +(1|Year),
                        data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_rich) # significant height growth over time
plot(elong_rich)
pp_check(elong_rich, type = "dens_overlay", nsamples = 100) 

# southern only 
elong_rich_south <- brms::brm(log(mean_stem_elong) ~ Sample_age + (1|Year),
                              data = all_CG_growth_ric_south,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_rich_south) # significant height growth over time
plot(elong_rich_south)
pp_check(elong_rich_south, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -----
elong_pul <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(1|Year),
                       data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_pul) # 
plot(elong_pul)
pp_check(elong_pul, type = "dens_overlay", nsamples = 100) 

# southern 
elong_pul_south <- brms::brm(log(mean_stem_elong) ~ Sample_age+(1|Year),
                             data = all_CG_growth_pul_south,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_pul_south) # 
plot(elong_pul_south)
pp_check(elong_pul_south, type = "dens_overlay", nsamples = 100) 


# Salix arctica -------
elong_arc <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(1|Year),
                       data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_arc) # significant growth over time
plot(elong_arc)
pp_check(elong_arc, type = "dens_overlay", nsamples = 100) 

# southern 
elong_arc_south <- brms::brm(log(mean_stem_elong) ~ Sample_age+(1|Year),
                             data = all_CG_growth_arc_south,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(elong_arc_south) # significant growth over time
plot(elong_arc_south)
pp_check(elong_arc_south, type = "dens_overlay", nsamples = 100) 

# 2.STEM ELONGATION -----
# Salix richardsonii -------
(rich_elong_plot_new <- all_CG_growth_ric_south %>%
  # group_by(population) %>%
   add_predicted_draws(elong_rich_south, allow_new_levels= TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(mean_stem_elong))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric_south) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Richardsonii stem elongation (log mm)\n") +
   xlab("\nSample age"))

# Salix pulchra ------
(pul_elong_plot_new <- all_CG_growth_pul_south %>%
  # group_by(population) %>%
   add_predicted_draws(elong_pul_south, allow_new_levels= TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(mean_stem_elong))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_pul_south) +
  scale_colour_viridis_d(begin = 0.1, end = 0.95) +
  scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Pulchra stem elongation (log mm)\n") +
   xlab("\nSample age"))


# Salix arctica------
(arc_elong_plot_new <- all_CG_growth_arc_south %>%
  # group_by(population) %>%
   add_predicted_draws(elong_arc_south, allow_new_levels= TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(mean_stem_elong))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc_south) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Arctica stem elongation (log mm)\n") +
   xlab("\nSample age"))

CG_height_panel <- grid.arrange(rich_heights_plot_new,
                                pul_heights_plot_new, 
                                arc_heights_plot_new, nrow = 1)
