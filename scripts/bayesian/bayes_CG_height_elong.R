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

# extracting model summary
height_rich_summ <- model_summ(height_rich)
summary(height_rich)
rownames(height_rich_summ) <- c("Intercept", "Sample age", "Southern population"
                           , "Sample age:Southern population", "Random intercept", 
                           "sd(Sample age)", "cor(Intercept, Sample age)", "sigma")

height_rich_summ$Rhat <- as.character(formatC(height_rich_summ$Rhat, digits = 2, format = 'f'))

height_rich_summ <- height_rich_summ %>%
  mutate(Species = "Salix richardsonii")%>%
  relocate("Species", .before = "Estimate")


# only southern random slopes
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

summary(height_pul)
height_pul_summ <- model_summ(height_pul)
rownames(height_pul_summ) <- c("Intercept      ", "Sample age      ", "Southern population "
                                , "Sample age:Southern population ", "Random intercept ", 
                                "sd(Sample age) ", "cor(Intercept, Sample age) ", "sigma ")
height_pul_summ$Rhat <- as.character(formatC(height_pul_summ$Rhat, digits = 2, format = 'f'))

height_pul_summ <- height_pul_summ %>%
  mutate(Species = "Salix pulchra")%>%
  relocate("Species", .before = "Estimate")

height_pul_ric <- rbind(height_rich_summ, height_pul_summ)

# only southern
# truncating to max height of pulchra 160 cm = log(160 )
height_pul_south <- brms::brm(log(Canopy_Height_cm) |trunc(lb = 0, ub =5.075174) ~ Sample_age+(Sample_age|SampleID_standard),
                               data = all_CG_growth_pul_south,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul_south) # 
plot(height_pul_south)
pp_check(height_pul_south, type = "dens_overlay", ndraws = 100) 

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
summary(height_arc) # significant growth over time
plot(height_arc)
pp_check(height_arc, type = "dens_overlay", nsamples = 100) 

height_arc_summ <- model_summ(height_arc)
rownames(height_arc_summ) <- c(" Intercept ", " Sample age ", " Southern population "
                               , " Sample age:Southern population ", " Random intercept ", 
                               " sd(Sample age) ", " cor(Intercept, Sample age) ", " sigma ")
height_arc_summ$Rhat <- as.character(formatC(height_arc_summ$Rhat, digits = 2, format = 'f'))

height_arc_summ <- height_arc_summ %>%
  mutate("Estimate_back" = exp("Estimate"),
         "Error_back"= exp("Est.Error"))%>%
  select(-"Estimate", -"Est.Error")%>%
  mutate(Species = "Salix arctica")%>%
  rename("Estimate" ="Estimate_back","Est.Error"= "Error_back") %>%
  relocate("Species", .before = "Estimate")

height_pul_ric_arc <- rbind(height_rich_summ, height_pul_summ,height_arc_summ )

kable_rich_pul_arc <- height_pul_ric_arc %>% 
  kbl(caption="Table. Heights over time of northern and southern shrubs in the common garden. ", 
      col.names = c("Species", "Estimate", "Error", "Lower 95% CI", "Upper 95% CI",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

# optional: making specific column text in italics
column_spec(kable_rich_pul_arc, 2, width = NULL, bold = FALSE, italic = TRUE) # 2 is my example column number 

save_kable(kable_rich_pul_arc,file = "outputs/tables/kable_rich_pul_arc.pdf", # or .png, or .jpeg, save in your working directory
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
rich_height_1 <- (conditional_effects(height_rich_south))
rich_height_data <- rich_height_1[[1]]
rich_height_data_2 <- rich_height_1[[2]]

# this graph below only lets me plot one line
(rich_height_plot <-ggplot() +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = log(Canopy_Height_cm), colour = population),
               alpha = 0.5)+
    geom_line(aes(x = effect1__, y = estimate__),
              linewidth = 1.5, data = rich_height_data) +
  geom_line(aes(x = effect1__, y = estimate__),
                               linewidth = 1.5, data = rich_height_data) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = .1) +
    ylab("Richardsonii canopy height (log cm)\n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub())

# this works well 
(rich_heights_plot_new <- all_CG_growth_ric %>%
    group_by(population) %>%
    add_predicted_draws(height_rich, allow_new_levels = TRUE) %>%
    ggplot(aes(x = Sample_age, y = Canopy_Height_cm, colour = population)) +
    stat_lineribbon(aes(y = exp(.prediction), fill = population), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_ric) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    ggtitle(expression(italic("Salix richardsonii"))) +
    ylab("Canopy height (cm)\n") +
    xlab("\nSample age"))
    #theme(legend.position = "none"))
(rich_heights_plot_new <- all_CG_growth_ric %>%
    group_by(population) %>%
    add_predicted_draws(height_rich, allow_new_levels = TRUE) %>%
    ggplot(aes(x = Sample_age, y = (Canopy_Height_cm), color = population, fill = population)) +
    stat_lineribbon(aes(y = exp(.prediction)), .width = c(.5), alpha = 1/4) +
    geom_point(data = all_CG_growth_ric) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    ylab("Canopy height (cm)\n") +
    labs(title = "Salix richardsonii") +
    xlab("\nSample age") +
    scale_x_continuous(breaks = seq(0, 9, by = 1)))

# Salix pulchra ------

(pul_heights_plot_new <- all_CG_growth_pul %>%
    group_by(population) %>%
    add_predicted_draws(height_pul, allow_new_levels = TRUE ) %>%
    ggplot(aes(x = Sample_age, y = Canopy_Height_cm, color = population)) +
    stat_lineribbon(aes(y = exp(.prediction), fill = population), .width = c(.50), alpha = 1/4) +
    geom_point(data = all_CG_growth_pul) +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_shrub() +
   ggtitle(expression(italic("Salix pulchra"))) +
   ylab("Canopy height (cm)\n") +
   xlab("\nSample age"))
  # theme(legend.position = "none"))

# Salix pulchra ------
pul_height_1 <- (conditional_effects(height_pul_south))
pul_height_data_1 <- pul_height_1[[1]]
pul_height_data_back <- pul_height_data_1 %>%
  dplyr::mutate(canopy_height_back = exp(log(Canopy_Height_cm)))

(pul_height_plot <-ggplot(pul_height_data_1) +
    geom_point(data = all_CG_growth_pul_south, aes(x = Sample_age,y = Canopy_Height_cm),
               alpha = 0.5, colour = "#0072B2")+
    geom_line(aes(x = effect1__, y = exp(estimate__)),
              linewidth = 1.5, colour = "#0072B2") +
    geom_ribbon(aes(x = effect1__, ymin = exp(lower__), ymax = exp(upper__)),
                alpha = .1, fill = "#0072B2") +
    labs(x = "\n Sample age",
         y = "Canopy height (cm)\n")+
    # ylab("Cover (/m2)\n") +
    # xlab("\n Sample age" ) +
    # scale_fill_brewer(palette = "orange") +
    scale_color_brewer(palette = "Dark2") +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) )

pulchra_CG_panel <- grid.arrange(pul_height_plot,pul_cover_plot, nrow=1)


# Salix arctica------
(arc_heights_plot_new <- all_CG_growth_arc %>%
  group_by(population) %>%
   add_predicted_draws(height_arc, allow_new_levels = TRUE ) %>%
   ggplot(aes(x = Sample_age, y = Canopy_Height_cm, color = population)) +
   stat_lineribbon(aes(y = exp(.prediction), fill = population), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc) +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_shrub() +
   ggtitle(expression(italic("Salix arctica"))) +
   ylab("Canopy height (cm)\n") +
   xlab("\nSample age"))


CG_height_panel <- ggarrange(rich_heights_plot_new,
                                pul_heights_plot_new, 
                                arc_heights_plot_new, nrow = 1,
                                common.legend = TRUE, legend="right")
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
