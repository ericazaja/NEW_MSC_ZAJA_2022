# Cover vs mean july temperature
#### Script by Erica Zaja, created 01/02/23
### Last updated: 01/02/23

# Libraries -----
library(tidyverse)

# 1. Loading data -----
july_enviro_chelsa <- read_csv("data/july_enviro_chelsa.csv") # chelsa temperature and precipitation data 
ITEX_shrubs_msc <- read_csv("data/ITEX/ITEX_shrubs_msc.csv") # ITEX cover data
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
  select(site, year, mean_temp_C)
  
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
  select(site, year, PrecipMeanJuly)

mean(QHI_july_precip$PrecipMeanJuly, na.rm=TRUE) # 9586.81
# QHI july mean surface temp: 9.10 °C 

# KP july mean temp and precip
KP_july_temp <- july_enviro_chelsa %>%
  filter(site == "Kluane_plateau")%>%
  select(site, year, mean_temp_C)

mean(KP_july_temp$mean_temp_C, na.rm=TRUE) #7.311905

KP_july_precip <- july_enviro_chelsa %>%
  filter(site == "Kluane_plateau")%>%
  select(site, year, PrecipMeanJuly)

mean(KP_july_precip$PrecipMeanJuly, na.rm=TRUE) # 7123.429

# CG july mean temp and precip
CG_july_temp <- july_enviro_chelsa %>%
  filter(site == "Common_garden") %>%
  select(site, year, mean_temp_C)

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

unique(all_CG_growth_cover_southern$population) #Southern     source_south source_north
  
all_CG_growth_cover <- all_CG_growth_cover_southern %>%
  mutate(cover = (Width_cm*Width_2_cm)/10000)%>%
  mutate(cover_percent = cover *100) %>%
  filter(cover_percent <=100) # setting max to 100% cover 

# remove CG data from full dataset
all_CG_growth_cover_edit <- all_CG_growth_cover %>%
  filter(Site %in% c("Qikiqtaruk", "Kluane")) 

# extracting max values from only common garden data
all_max_CG_growth_cover_southern <- all_CG_growth_cover %>% 
  filter(Site == "Common_garden") %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(cover_percent)) %>% 
  rename("cover_percent" = "cover_percent")

# remerging data
all_growth_cover <- rbind(all_max_CG_growth_cover_southern, all_CG_growth_cover_edit)

# making summary data for cover
CG_source_cover_summary <- all_growth_cover %>%
  group_by(Site, Species) %>% 
  summarise(mean_cover = mean(cover_percent)) %>%
  mutate(Site = case_when(Site == "Qikiqtaruk" ~ "QHI", # renaming so datasets match
                          Site == "Common_garden" ~ "CG",
                          Site == "Kluane" ~ "KP"))


# ITEX COVER DATA -----
ITEX_cover_summary <- ITEX_shrubs_msc %>%
  group_by(SITE, SPECIES_NAME) %>%
  summarise(mean_cover = mean(RelCover))  %>%
  rename("Site" = "SITE",
         "Species" = "SPECIES_NAME")  %>%
  filter(Site!= "QHI") # I'm using the common garden data for QHI cover 

# merge all cover and temp data into summary means dataset
all_cover <- rbind (ITEX_cover_summary, CG_source_cover_summary)
all_cover_temps <- full_join(all_cover, july_enviro_means)

all_cover_temps$Site <- as.factor(all_cover_temps$Site)
all_cover_temps$Species <- as.factor(all_cover_temps$Species)

# merge full datasets for model
ITEX_shrubs_edit <- ITEX_shrubs_msc %>%
  dplyr::select(YEAR, SITE, SPECIES_NAME, RelCover) %>%
  rename("Site" = "SITE",
         "Species" = "SPECIES_NAME",
         "Year" = "YEAR", 
         "cover_percent" = "RelCover")  %>%
  filter(Site!= "QHI") # I'm using the common garden data for QHI cover 

all_CG_growth_cover_edit <- all_growth_cover %>%
  dplyr::select(Year, Site, Species, cover_percent) %>%
  mutate(Site = case_when(Site == "Qikiqtaruk" ~ "QHI", # renaming so datasets match
                          Site == "Common_garden" ~ "CG",
                          Site == "Kluane" ~ "KP"))

all_cover_long <- rbind(all_CG_growth_cover_edit, ITEX_shrubs_edit)
all_cover_temps_long <- full_join(all_cover_long, july_enviro_means)

all_cover_temps_long$Year <- as.factor(all_cover_temps_long$Year)
all_cover_temps_long$Site <- as.factor(all_cover_temps_long$Site)
all_cover_temps_long$Species <- as.factor(all_cover_temps_long$Species)


# MODELLING cover vs temp and precip ----
model_cover_temp <- lmer(cover_percent ~ mean_temp + Species + (1|Site) + (1|Year), data = all_cover_temps_long)
tab_model(model_cover_temp)
plot(model_cover_temp)

model_cover_precip <- lmer(cover_percent ~ mean_precip + Species + (1|Site) + (1|Year), data = all_cover_temps_long)
tab_model(model_cover_precip)
plot(model_cover_precip)

# separate species
all_cover_temps_arctica <- all_cover_temps_long %>%
  filter(Species == "Salix arctica")

model_cover_temp_arctica <- lmer(cover_percent ~ mean_temp + (1|Site), data = all_cover_temps_arctica)
tab_model(model_cover_temp_arctica)
model_cover_precip_arctica <- lmer(cover_percent ~ mean_precip + (1|Site), data = all_cover_temps_arctica)
tab_model(model_cover_precip_arctica)

all_cover_temps_rich <- all_cover_temps_long %>%
  filter(Species == "Salix richardsonii")

model_cover_temp_rich <- lm(cover_percent ~ mean_temp + Site, data = all_cover_temps_rich)
tab_model(model_cover_temp_rich) # significant 
model_cover_precip_rich <- lm(cover_percent ~ mean_precip + Site, data = all_cover_temps_rich)
tab_model(model_cover_precip_rich)

all_cover_temps_pulchra <- all_cover_temps_long %>%
  filter(Species == "Salix pulchra")

model_cover_temp_pulchra <- lmer(cover_percent ~ mean_temp + (1|Site) + (1|Year), data = all_cover_temps_pulchra)
tab_model(model_cover_temp_pulchra)
model_cover_precip_pulchra<- lmer(cover_percent ~ mean_precip + (1|Site) + (1|Year), data = all_cover_temps_pulchra)
tab_model(model_cover_precip_pulchra)


# DATA VISUALISATION -----

# can visualise things but they are not comparable because
# cover in the garden is since cuttings put down VS cover in source pops is of mature shurbs
(box_cover_temp <- ggplot(all_cover_temps_long) +
   geom_point(aes(x = mean_temp, y= cover_percent, colour = Site, fill = Site), size = 3, alpha = 0.1) +
   geom_boxplot(aes(x = mean_temp, y= cover_percent, colour = Site, fill = Site), size = 0.5, alpha = 0.5) +
   ylab("Cover (%)") +
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
(scatter_cover_temp <- ggplot(all_cover_temps) +
   geom_point(aes(x = mean_temp, y= mean_cover, colour = Site, fill = Site, group = Site), size = 3, alpha = 0.8) +
   geom_smooth(aes(x = mean_temp, y= mean_cover), method = "lm",  se=F, colour = "black")  +
   ylab("Mean cover (%)") +
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
(scatter_cover_precip <- ggplot(all_cover_temps) +
    geom_point(aes(x = mean_precip, y= mean_cover, colour = Site, fill = Site, group = Site), size = 3, alpha = 0.8) +
    geom_smooth(aes(x = mean_precip, y= mean_cover), method = "lm",  colour = "black")  +
    ylab("Mean cover (%)") +
    xlab("\nMean july precip ()") +
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
(scatter_all_cover_temp_rich <- ggplot(all_cover_temps_rich) +
    geom_point(aes(x = mean_temp, y= cover_percent,colour = Site, fill = Site), size = 3) +
    geom_smooth(aes(x = mean_temp, y= cover_percent), method = "lm") +
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

