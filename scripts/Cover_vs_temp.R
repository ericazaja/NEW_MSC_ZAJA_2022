# Cover vs mean july temperature
#### Script by Erica Zaja, created 01/02/23
### Last updated: 01/02/23

# Libraries -----
library(tidyverse)
library(sjPlot)
library(lme4)

# 1. Loading data -----
july_enviro_chelsa <- read_csv("data/july_enviro_chelsa.csv") # chelsa temperature and precipitation data 
ITEX_shrubs_msc <- read_csv("data/ITEX/itex_EZ_shrubs_2023.csv") # newest 
all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")

# DATA WRANGLE ------

# 1. CHELSA data ---- 
unique(july_enviro_chelsa$site)

#rename column
july_enviro_chelsa <- july_enviro_chelsa %>%
  rename ("mean_temp_C" ="(mean_temp_C = (mean_temp/10 - 273.15))")

# TOOLIK july mean temp and precip
TOOLIK_july_temp <- july_enviro_chelsa %>%
  filter(site %in% c("TUSSOKGRID", "IMNAVAIT"))
  dplyr::select(site, year, mean_temp_C)
  
mean(TOOLIK_july_temp$mean_temp_C, na.rm=TRUE) # 10.41667
  
TOOLIK_july_precip <- july_enviro_chelsa %>%
    filter(site %in% c("TUSSOKGRID", "IMNAVAIT"))
  select(site, year, PrecipMeanJuly)

mean(TOOLIK_july_precip$PrecipMeanJuly, na.rm=TRUE) # 16182.26
  
# ANWR july mean temp and precip
ANWR_july_temp <- july_enviro_chelsa %>%
  filter(site == "ATIGUN") %>%
 select(site, year, mean_temp_C)

mean(ANWR_july_temp$mean_temp_C, na.rm=TRUE) # 10.75

ANWR_july_precip <- july_enviro_chelsa %>%
  filter(site == "ATIGUN") %>%
select(site, year, PrecipMeanJuly)

mean(ANWR_july_precip$PrecipMeanJuly, na.rm=TRUE) # 15193.14

# QHI july mean temp and precip
QHI_july_temp <- july_enviro_chelsa %>%
  filter(site == "QHI")%>%
  select(site, year, mean_temp_C)

mean(QHI_july_temp$mean_temp_C, na.rm=TRUE) # 6.15
# QHI july mean surface temp: 9.10 °C 
# based on 2022 TOMST and 2017 HOBO data 

QHI_july_precip <- july_enviro_chelsa %>%
  filter(site == "QHI")%>%
  dplyr::select(site, year, PrecipMeanJuly)

mean(QHI_july_precip$PrecipMeanJuly, na.rm=TRUE) # 9586.81
# QHI july mean surface temp: 9.10 °C 

# KP july mean temp and precip
KP_july_temp <- july_enviro_chelsa %>%
  filter(site == "Kluane_plateau")%>%
  dplyr::select(site, year, mean_temp_C)

mean(KP_july_temp$mean_temp_C, na.rm=TRUE) #7.311905

KP_july_precip <- july_enviro_chelsa %>%
  filter(site == "Kluane_plateau")%>%
  dplyr::select(site, year, PrecipMeanJuly)

mean(KP_july_precip$PrecipMeanJuly, na.rm=TRUE) # 7123.429

# CG july mean temp and precip
CG_july_temp <- july_enviro_chelsa %>%
  filter(site == "Common_garden") %>%
  dplyr::select(site, year, mean_temp_C)

mean(CG_july_temp$mean_temp_C, na.rm=TRUE) #13.67857

# Common garden july mean surface temp: 17.97°C 
# Based on 6 years data (TOMST+ HOBO)

CG_july_precip <- july_enviro_chelsa %>%
  filter(site == "Common_garden") %>%
  select(site, year, PrecipMeanJuly)

mean(CG_july_precip$PrecipMeanJuly, na.rm=TRUE) # 5287.333

# making a summarised dataset
july_enviro_means <- july_enviro_chelsa %>%
  group_by(site) %>% 
  summarise(mean_precip = mean(PrecipMeanJuly, na.rm=TRUE), 
            mean_temp = mean(mean_temp_C,na.rm=TRUE)) 

july_enviro_means$site <- as.factor(july_enviro_means$site)

july_enviro_means <- july_enviro_means %>%
  mutate(Site = case_when(site == "ATIGUN" ~ "ANWR",
            site %in% c("IMNAVAIT", "TUSSOKGRID") ~ "TOOLIK", 
            site == "QHI" ~ "QHI",
            site == "Common_garden" ~ "CG",
            site == "Kluane_plateau" ~ "KP")) %>%
  select(Site, mean_precip, mean_temp) %>%
  group_by(Site)  %>%
  summarise(mean_precip = mean(mean_precip, na.rm=TRUE), 
                      mean_temp = mean(mean_temp,na.rm=TRUE))  # so I only keep one value for toolik

# SOURCE POP DATA ----

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

# calculating cover change in the common garden from one year to the next
all_CG_growth_cover_change <- all_CG_growth_cover %>%
  filter(Site == "Common_garden") %>% 
  group_by(Species, SampleID_standard) %>% 
  arrange(Sample_age, .by_group = TRUE) %>%
  mutate(cover_increase = cover_percent-lag(cover_percent)) %>%
  mutate(cover_change_percent = (cover_increase/cover_percent)*100)
  
summary_all_CG_growth_cover_change <- all_CG_growth_cover_change %>%
  group_by(Species) %>%
  summarise(mean_cover_change = mean(cover_change_percent, na.rm=TRUE))%>%
  na.omit() %>%
  mutate(Site = rep("CG"))

# calculating cover change in the source populations from one year to the next
all_source_growth_cover_change <- all_CG_growth_cover %>%
  filter(Site %in% c("Qikiqtaruk", "Kluane")) %>% 
  group_by(Year, Species, Site) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate(cover_increase = cover_percent-lag(cover_percent)) %>%
  mutate(cover_change_percent = (cover_increase/cover_percent)*100)

summary_all_source_growth_cover_change <- all_source_growth_cover_change %>%
  group_by(Site, Species) %>%
  summarise(mean_cover_change = mean(cover_change_percent, na.rm=TRUE))%>%
  na.omit() %>%
  mutate(Site = case_when(Site == "Kluane" ~ "KP", 
                          Site == "Qikiqtaruk" ~ "QHI"))


# extracting max values from only common garden data
#all_max_CG_growth_cover_southern <- all_CG_growth_cover %>% 
 # filter(Site == "Common_garden") %>% 
 # group_by(SampleID_standard) %>%
 # slice(which.max(cover_percent)) %>% 
 # rename("cover_percent" = "cover_percent")

# remerging data
all_growth_cover_change <- rbind(summary_all_source_growth_cover_change, summary_all_CG_growth_cover_change)

# making summary data for cover
#CG_source_cover_summary <- all_growth_cover %>%
 # group_by(Year, Site, Species) %>% 
 # summarise(mean_cover = mean(cover_percent)) %>%
 # mutate(Site = case_when(Site == "Qikiqtaruk" ~ "QHI", # renaming so datasets match
           #               Site == "Common_garden" ~ "CG",
              #            Site == "Kluane" ~ "KP"))


# ITEX COVER DATA -----
ITEX_shrubs_cover_change <- ITEX_shrubs_msc %>%
  group_by(YEAR, SITE, SPECIES_NAME) %>%  
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(cover_increase = RelCover-lag(RelCover)) %>%
  mutate(cover_change_percent = (cover_increase/RelCover)*100)

summary_ITEX_shrubs_cover_change <- ITEX_shrubs_cover_change %>%
  group_by(SITE, SPECIES_NAME) %>%
  summarise(mean_cover_change = mean(cover_change_percent, na.rm=TRUE))%>%
  na.omit() %>%
  filter(SITE!= "QHI") %>%# I'm using the common garden data for QHI cover 
  rename("Site" = "SITE",
         "Species" = "SPECIES_NAME")

# merge all cover and temp data into summary means dataset
all_cover <- rbind (summary_ITEX_shrubs_cover_change, all_growth_cover_change)
all_cover_temps <- full_join(all_cover, july_enviro_means)

all_cover_temps$Site <- as.factor(all_cover_temps$Site)
all_cover_temps$Species <- as.factor(all_cover_temps$Species)
unique(all_cover_temps$Site) # ANWR   TOOLIK KP     QHI    CG    

# converting precip to mm
all_cover_temps <- all_cover_temps %>%
  mutate(mean_precip = mean_precip/100)

# saving the means dataset
write.csv(all_cover_temps, "data/all_cover_temps.csv" )

# merge full ( non summarised data)
all_CG_growth_cover_change_edit <- all_CG_growth_cover_change %>%
  dplyr::select(Year, Species, Site, Sample_age, cover_change_percent) %>%
  mutate(Site = rep("CG")) %>%
  na.omit()

all_source_growth_cover_change_edit <- all_source_growth_cover_change %>%
  dplyr::select(Year, Species, Site, cover_change_percent) %>%
  mutate(Site = case_when(Site == "Kluane" ~ "KP", 
                          Site == "Qikiqtaruk" ~ "QHI")) %>%
  na.omit()

ITEX_shrubs_cover_change_edit <- ITEX_shrubs_cover_change %>%
  filter(SITE!= "QHI") %>%
  dplyr::select(YEAR, SPECIES_NAME, SITE, cover_change_percent) %>%
  rename("Year"="YEAR", "Species" = "SPECIES_NAME", "Site"= "SITE") %>%
  na.omit()

all_cover_long <- rbind(ITEX_shrubs_cover_change_edit, all_source_growth_cover_change_edit,
                        all_CG_growth_cover_change_edit) %>%
  dplyr::select(-SampleID_standard)

all_cover_temps_long <- full_join(all_cover_long, july_enviro_means)

all_cover_temps_long$Year <- as.factor(all_cover_temps_long$Year)
all_cover_temps_long$Site <- as.factor(all_cover_temps_long$Site)
all_cover_temps_long$Species <- as.factor(all_cover_temps_long$Species)

all_cover_temps_long <- all_cover_temps_long %>%
  mutate(mean_precip = mean_precip/100) # converting precip in mm


# saving full dataset 
write.csv(all_cover_temps_long, "data/all_cover_temps_long.csv")

# doing what diana said: merging all_cover_long with all years of temp data -----
july_enviro_chelsa_new <- july_enviro_chelsa %>%
  mutate(site = case_when(site == "ATIGUN" ~ "ANWR",
                          site %in% c("IMNAVAIT", "TUSSOKGRID") ~ "TOOLIK", 
                          site == "QHI" ~ "QHI",
                          site == "Common_garden" ~ "CG",
                          site == "Kluane_plateau" ~ "KP")) %>%
  rename("Site"= "site", "Year"="year", "mean_precip"= "PrecipMeanJuly") %>%
  dplyr:: select(Site, Year, mean_precip, mean_temp_C)


all_cover_temps_new <- left_join(all_cover_long, july_enviro_chelsa_new, by=c('Site'='Site', 'Year'='Year'))

write.csv(all_cover_temps_new, "data/all_cover_temps_new.csv")

# MODELLING cover vs temp and precip ----
model_cover_temp <- lmer(cover_change_percent ~ mean_temp + Species + (1|Site), data = all_cover_temps_long)
tab_model(model_cover_temp)
plot(model_cover_temp)

# separate species models----
# pulchra cover change per unit temp 
all_cover_temps_long_pulchra <- all_cover_temps_long %>%
  filter(Species == "Salix pulchra")

model_cover_temp_pulchra <- lmer(cover_change_percent ~ mean_temp + (1|Site), data = all_cover_temps_long_pulchra)
tab_model(model_cover_temp_pulchra) # 3.62 % cover change per unit temp

# richardsonii cover change per unit temp 
all_cover_temps_long_rich <- all_cover_temps_long %>%
  filter(Species == "Salix richardsonii") 

model_cover_temp_rich <- lmer(cover_change_percent ~ mean_temp + (1|Site), data = all_cover_temps_long_rich)
tab_model(model_cover_temp_rich) # 13.81% cover change per unit temp

# mean cover change for rich and pulchra = 8.41 %

# arctica  cover change per unit temp 
all_cover_temps_long_arctica <- all_cover_temps_long %>%
  filter(Species == "Salix arctica")

model_cover_temp_arctica <- lmer(cover_change_percent ~ mean_temp + (1|Site), data = all_cover_temps_long_arctica)
tab_model(model_cover_temp_arctica) # 10.30 % cover change per unit temp

# can do same models but with precipitation
# doesnt run but will try again when bayesian
model_cover_precip <- lm(cover_change_percent ~ mean_precip + Species, data = all_cover_temps_long)
tab_model(model_cover_precip)
plot(model_cover_precip)

# separate species models----
# doesnt run with site random effect
model_cover_precip_pulchra <- lm(cover_change_percent ~ mean_precip, data = all_cover_temps_long_pulchra)
tab_model(model_cover_precip_pulchra) # -1.40 % cover change per unit precip

model_cover_precip_rich <- lmer(cover_change_percent ~ mean_precip + (1|Site), data = all_cover_temps_long_rich)
tab_model(model_cover_precip_rich) # -2.75 % cover change per unit precip

# mean cover change pulchra + rich :  -2.075 % cover change per unit precip
# doesnt run with site random effect
model_cover_precip_arctica <- lm(cover_change_percent ~ mean_precip, data = all_cover_temps_long_arctica)
tab_model(model_cover_precip_arctica) # -1.60	 % cover change per unit precip

# DATA VISUALISATION -----
# plotting means: cover change vs mean july temp
(scatter_cover_temp <- ggplot(all_cover_temps) +
   geom_point(aes(x = mean_temp, y= mean_cover_change, colour = Site, fill = Site, group = Site), size = 3, alpha = 0.8) +
   geom_smooth(aes(x = mean_temp, y= mean_cover_change), method = "lm",  se=F, colour = "black")  +
   ylab("Mean cover change (%)") +
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

# plotting means: cover change vs mean july precip 
(scatter_cover_precip <- ggplot(all_cover_temps) +
    geom_point(aes(x = mean_precip, y= mean_cover_change, colour = Site, fill = Site, group = Site), size = 3, alpha = 0.8) +
    geom_smooth(aes(x = mean_precip, y= mean_cover_change), method = "lm",  se=F,colour = "black")  +
    ylab("Mean cover change (%)") +
    xlab("\nMean july precip (mm)") +
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

# change spp. accordingly
(scatter_all_cover_temp_rich <- ggplot(all_cover_temps_long_rich) +
    geom_point(aes(x = mean_temp, y= cover_change_percent,colour = Site, fill = Site), size = 3) +
    geom_smooth(aes(x = mean_temp, y= cover_change_percent), method = "lm") +
    ylab("Cover (%)") +
    xlab("\nMean july temperature (degC)") +
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

