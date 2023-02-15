# Stem elongation vs mean july temperature
#### Script by Erica Zaja, created 01/02/23
### Last updated: 01/02/23

# Common garden july mean surface temp: 17.97°C 
# Based on 6 years data (TOMST+ HOBO)

# KP july mean surface temp: 9.25 °C 
# based on 2 years of data (TOMST)

# QHI july mean surface temp: 9.10 °C 
# based on 2022 TOMST and 2017 HOBO data 

# libraries
library(tidyverse)
library(readr)
library(lme4)
library(sjPlot)

# 1. Loading data ----
all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")

# 2. Data wrangling -----
#all_CG_source_growth_arctica <- all_CG_source_growth %>%
  #filter(Species == "Salix arctica") %>%
  #filter(Site == "Qikiqtaruk")

# add temp column
all_CG_source_growth_temp <- all_CG_source_growth %>%
  mutate(july_mean_temp = case_when(Site == "Common_garden" ~ rep(17.97),
                                    Site == "Kluane" ~ rep(9.25),
                                    Site == "Qikiqtaruk" ~ rep(9.10))) %>%
   filter(population %in% c("Southern", "source_north", "source_south"))       

unique(all_CG_source_growth_temp$population) # "Southern"     "source_south" "source_north"
unique(all_CG_source_growth_temp$july_mean_temp) # 17.97  9.25  9.10
all_CG_source_growth_temp$Species <- as.factor(all_CG_source_growth_temp$Species)
all_CG_source_growth_temp$Site <- as.factor(all_CG_source_growth_temp$Site)
all_CG_source_growth_temp$july_mean_temp <- as.factor(all_CG_source_growth_temp$july_mean_temp)

# remove first few years of stem elongation data
unique(all_CG_source_growth_temp$Sample_age) 

all_CG_source_growth_temp_edit_1 <- all_CG_source_growth_temp %>%
  filter(Site %in% c("Kluane", "Qikiqtaruk"))

all_CG_source_growth_temp_edit_2 <- all_CG_source_growth_temp %>%
   filter(Sample_age %in% c("3", "4", "5", "6"))

all_CG_source_growth_temp_edit <- rbind(all_CG_source_growth_temp_edit_2, all_CG_source_growth_temp_edit_1)
unique(all_CG_source_growth_temp_edit$Sample_age) # 3-6
unique(all_CG_source_growth_temp_edit$Site)

# extracting max values from only common garden data
max_cg_elong <- all_CG_source_growth_temp %>% 
  filter(Site == "Common_garden") %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(mean_stem_elong)) %>% 
  rename("max_stem_elong" = "mean_stem_elong")

# reclassing Species as factor
max_cg_elong$Species <- as.factor(max_cg_elong$Species)

# mean max height per population and species
max_cg_elog_spp <- max_cg_elong %>%
  group_by(population,Species) %>%
  summarise(mean_max_elong_mm = mean(max_stem_elong))

range(max_cg_elog_spp$mean_max_elong_mm)
# Artica (southern garden): 15.13232 mm
# Pulchra (southern g.): 170.39277 mm
# Rich (southern g.): 209.91390 mm

# merging with source pop data
max_cg_elong <- max_cg_elong %>%
 rename( "mean_stem_elong"="max_stem_elong")

all_cg_max_source <- rbind(max_cg_elong,all_CG_source_growth_temp_edit_1)
all_cg_max_source$Species <- as.factor(all_cg_max_source$Species)
all_cg_max_source$july_mean_temp <- as.factor(all_cg_max_source$july_mean_temp)

# creating overall mean stem elong value for each site
all_CG_source_growth_means <- all_cg_max_source %>%
  group_by(Site, Species) %>%
  summarise(mean_elong=mean(mean_stem_elong, na.rm=TRUE), 
            n = n(),
            sd=sd(mean_stem_elong,na.rm=TRUE)) %>%
  mutate(se = sd / sqrt(n))  %>%# Calculating standard error
  na.omit()

temps <- c(17.97, 17.97, 17.97, 9.25, 9.25, 9.10, 9.10, 9.10)

means_temps <- cbind(all_CG_source_growth_means, new_col = temps)

means_temps <- means_temps %>%
  rename("july_temps"="new_col")

view(means_temps)

# 3. Data visualisation -----

(box_elong_temp <- ggplot(all_cg_max_source) +
   geom_point(aes(x = july_mean_temp, y= mean_stem_elong, colour = Site, fill = Site), size = 3, alpha = 0.1) +
  geom_boxplot(aes(x = july_mean_temp, y= mean_stem_elong, colour = Site, fill = Site), size = 0.5, alpha = 0.5) +
   ylab("Mean stem elongation (mm)") +
   xlab("\nMean july temperature (degC)") +
   facet_wrap(~Species, scales = "free") +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
   theme_shrub() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black"))) 

# means
(scatter_elong_temp <- ggplot(means_temps) +
    geom_point(aes(x = july_temps, y= mean_elong, colour = Site, fill = Site, group = Site), size = 3, alpha = 0.8) +
    geom_smooth(aes(x = july_temps, y= mean_elong), method = "lm",  se = FALSE, colour = "black")  +
    ylab("Mean stem elongation (mm)") +
    xlab("\nMean july temperature (degC)") +
    facet_wrap(~Species, scales = "free") +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
    theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 


# Modelling -----
# I dont have arctica on KP!
# so let's remove arctica and only look at 2 tall spp separately

all_cg_max_source_arctica <- all_cg_max_source %>%
  filter(Species == ("Salix arctica"))

all_cg_max_source_rich <- all_cg_max_source %>%
  filter(Species == ("Salix richardsonii"))

all_cg_max_source_pulchra <- all_cg_max_source %>%
  filter(Species == ("Salix pulchra"))

all_cg_max_source_arctica$july_mean_temp <- as.numeric(all_cg_max_source_arctica$july_mean_temp)
all_cg_max_source_arctica$Year <- as.factor(all_cg_max_source_arctica$Year)

all_cg_max_source_rich$july_mean_temp <- as.numeric(all_cg_max_source_rich$july_mean_temp)
all_cg_max_source_rich$Year <- as.factor(all_cg_max_source_rich$Year)

all_cg_max_source_pulchra$july_mean_temp <- as.numeric(all_cg_max_source_pulchra$july_mean_temp)
all_cg_max_source_pulchra$Year <- as.factor(all_cg_max_source_pulchra$Year)

# Arctica -----
# no site random effect because i dont have 3 levels (no arctica on KP)
model_elong_temp_arctica <- lmer(mean_stem_elong ~ july_mean_temp + (1|Year), data = all_cg_max_source_arctica)
tab_model(model_elong_temp_arctica) # not significant 
plot(model_elong_temp_arctica)

# Richardsonii ------
model_elong_temp_rich <- lmer(mean_stem_elong ~ july_mean_temp + (1|Site) + (1|Year), data = all_cg_max_source_rich)
tab_model(model_elong_temp_rich ) # significant 
plot(model_elong_temp_rich )

# Pulchra -------
model_elong_temp_pulchra <- lmer(mean_stem_elong ~ july_mean_temp +  (1|Site) + (1|Year), data = all_cg_max_source_pulchra)
tab_model(model_elong_temp_pulchra ) # significant 
plot(model_elong_temp_pulchra )


                        