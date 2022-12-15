### Making allometric equations for Salix spp.
### Script by Erica Zaja
### Last updated: 05/12/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(readxl)
library(sjPlot)

# 2. LOADING DATA ----

# Biomass harvests (grams) and heights from Cunliffe et al 2020: QHI 
# NB data only for 50 x 50 cm plots (aka might not be all of the shrub)
Andy_biomass <- read_csv("data/allometry/Andy_paper/biomass.csv")
Andy_heights <- read_excel("data/allometry/Andy_paper/heights.xlsx")
# Andy_main_database <- read_excel("data/allometry/Andy_paper/main_database.xlsx")

# Biomass harvests and heights from Myers-Smith PhD: Pika  
# NB data only for 50 x 50 cm plots (aka might not be all of the shrub)
Biomass_harvest_Pika <- read_excel("data/allometry/Isla_phd/Biomass_harvest_Pika.xlsx")
# Heights_Regression_Pika <- read_excel("data/allometry/Isla_phd/Heights_Regression_Pika.xlsx")
Pika_heights_edit <- read_excel("data/allometry/Isla_phd/Pika_biomass_heights_edit.xlsx")
Biomass_Calculations_Pika <- read_excel("data/allometry/Isla_phd/Biomass Calculations Pika.xlsx")
Percent_cover_Pika <- read_excel("data/allometry/Isla_phd/Percent_cover_Pika.xlsx")

# Biomass harvests and heights from Berner's 2015 paper
# N.B. data for the full shrub 
Logan_data_biomass <- read_excel("data/allometry/Berner/Logan-data-biomass.xlsx")

# 3. DATA WRANGLING -----
# Workflow for Andy and Isla's data: keeping max values (of height, biomass and cover),
# incorporating cover into the relationship by dividing biomass by cover in the 
# 50x50cm quadrat and then multiplying by 100 (i.e what biomass of the full shrub would be?)
# once i have indexed biomass, I multiply by 4 to obtain biomass in g/m2. 
# NB for point framing, I divide the count by the tot number of point and then multiply by 100 for % cover, 
# I then I go on to calculate indexed biomass
 
# 3.1. QHI SALIX PULCHRA and SALIX ARCTICA (Andy) ----

Andy_biomass <- Andy_biomass[-1,] # removing row with words

Andy_heights_salix <- Andy_heights %>%
  filter(Species %in% c("Salix arctica", "Salix richardsonii" ))

Andy_heights_salix$Height <- as.numeric(Andy_heights_salix$Height)

Andy_heights_salix <- Andy_heights_salix %>% na.omit()  

# multiple heights are measured (point framing), so retaining maximum heights (aka shrubs)
# the shrubs are always the tallest thing in the plots if there are shrubs there.
# Don't really need to worry about knowing what species it is.  
# The shrub is always Salix richardsonii or Salix arctica.  
# Only use the Salix arctica data if there is no richardsonii in the plot.
Andy_maxheights_salix <- Andy_heights_salix %>%
  select(Species, Height, Count, PlotN)%>%
  group_by(PlotN)%>%
  mutate(max_height = max(Height),
         max_count= max(Count)) %>%
  select(-Height, -Species, -Count)%>%
  distinct()%>%
  na.omit() %>%
  rename("PlotID" = "PlotN")

# merging heights and biomass datasets
QHI_all_biomass <- full_join(Andy_maxheights_salix, Andy_biomass, 
                             by = "PlotID")
# renaming columns
QHI_all_biomass <- QHI_all_biomass %>%
  rename("Woody_stem_biomass" = "Woody stem biomass",
         "Shrub_leaf_biomass" = "Shrub leaf biomass")

# reclassing variables
QHI_all_biomass$PlotID <- as.factor(QHI_all_biomass$PlotID)
QHI_all_biomass$Shrub_leaf_biomass <- as.numeric(QHI_all_biomass$Shrub_leaf_biomass)
QHI_all_biomass$Woody_stem_biomass<- as.numeric(QHI_all_biomass$Woody_stem_biomass)
QHI_all_biomass$max_count<- as.numeric(QHI_all_biomass$max_count)

# making a total biomass column summing stem and leaf biomass
QHI_all_shrub_biomass <- QHI_all_biomass %>%
  select(max_count, PlotID, max_height, Woody_stem_biomass, Shrub_leaf_biomass)%>%
  mutate(tot_shrub_biomass = sum (Woody_stem_biomass, Shrub_leaf_biomass)) %>%
  mutate(percent_cover = (max_count/36)*100) %>% # calculating % cov from point framing (36 points tot)
  mutate(biomass_index =  (tot_shrub_biomass/percent_cover)*100) %>%
  distinct() %>%
  mutate(biomass_per_m2 = (biomass_index*4)) # times 4 to make biomass/m2

# Adding zeros row: if height 0, biomass 0
PlotID <- "P00"
max_height <- 0
Woody_stem_biomass <- 0
Shrub_leaf_biomass <- 0
tot_shrub_biomass <- 0
percent_cover <- 0
biomass_index <- 0
biomass_per_m2 <- 0

zeros <- data.frame(PlotID, max_height, 
                    Woody_stem_biomass, Shrub_leaf_biomass,
                    tot_shrub_biomass, percent_cover, 
                    biomass_index, biomass_per_m2)

# adding zero row
QHI_all_shrub_biomass <- rbind(QHI_all_shrub_biomass, zeros)

# 3.2. PIKA SALIX PULCHRA and SALIX RICHARDSONII (Isla) ----

# renaming columns
Biomass_Pika <- Biomass_Calculations_Pika %>%
  select(Plot...2, `Tall Shrubs...12`) %>%
  rename("Tall_shrub_biomass"= "Tall Shrubs...12",
         "Plot" = "Plot...2")

# removing zeros
Biomass_Pika[Biomass_Pika == 0.000] <- NA

Biomass_Pika <- Biomass_Pika %>%
  na.omit()

# renaming columns
Percent_cov_Pika <- Percent_cover_Pika %>%
  select(Plot, `Tall Shrubs...10`) %>%
  na.omit() %>%
  rename("Tall_shrub_cov"= "Tall Shrubs...10")

# removing zeros 
Percent_cov_Pika[Percent_cov_Pika == 0.0000000] <- NA

Percent_cov_Pika <- Percent_cov_Pika %>%
  na.omit()

# keeping relevant columns
Heights_Pika <- Pika_heights_edit %>%
  select(Plot, Species, Shrub_Height_cm)

# reclassing variables
Heights_Pika$Plot <- as.factor(Heights_Pika$Plot)
Percent_cov_Pika$Plot <- as.factor(Percent_cov_Pika$Plot)
Biomass_Pika$Plot <- as.factor(Biomass_Pika$Plot)

# joinin heights and biomass datasets
Pika_all <- print(list(Heights_Pika,Biomass_Pika, Percent_cov_Pika) %>% 
                    reduce(left_join, by='Plot')) %>%
              distinct() 

# multiple biomass values per height, so I kept max biomass per height
Pika_all_shrub_biomass <- Pika_all %>%
  group_by(Plot) %>%
  mutate(max_biomass = max(Tall_shrub_biomass), 
          max_cover = max(Tall_shrub_cov)) %>%
  mutate(biomass_index = (max_biomass/max_cover)*100) %>%
  select(Plot, Shrub_Height_cm, max_biomass, max_cover, biomass_index)%>%
  distinct() %>%
  mutate(biomass_per_m2 = (biomass_index*4)) # times 4 to make biomass/m2

# Adding zeros row: if height 0, biomass 0
Plot <- "00"
Shrub_Height_cm <- 0
max_biomass <- 0
max_cover <- 0
biomass_index <- 0
biomass_per_m2 <- 0

zeros_pika <- data.frame(Plot, Shrub_Height_cm, max_biomass, max_cover, biomass_index, biomass_per_m2)

Pika_all_shrub_biomass <- rbind(Pika_all_shrub_biomass, zeros_pika)


# 3.3. ALASKA SALIX PULCHRA (Logan Berner)----

Logan_salix_pulchra <- Logan_data_biomass %>% 
  filter(Genus == "Salix" & Species == "pulchra") %>%
  select(Genus, Species, Region, Ecosystem, "Height (cm)", "AGB (g)") %>%
  na.omit() %>%
  rename("AGB_g" = "AGB (g)",
         "Height_cm" = "Height (cm)")

# 4. MODELLING: regression  biomass ~ height  

# Andy: Salix richardsonii
andy_model <- lm(biomass_per_m2 ~ max_height, data = QHI_all_shrub_biomass)
summary(andy_model)
tab_model(andy_model)

(plot_andy_model <- ggplot(QHI_all_shrub_biomass) +
    geom_point(aes(x = max_height, y= biomass_per_m2), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = max_height, y= biomass_per_m2), colour = "brown",method = "lm") +
    ylab("full shrub AGB (g/m2)") +
    xlab("\nHeight (cm)") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))


# Isla: Salix pulchra and richardsonii
isla_model <- lm(biomass_per_m2 ~ Shrub_Height_cm, data = Pika_all_shrub_biomass)
summary(isla_model)
tab_model(isla_model)

(plot_isla_model <- ggplot(Pika_all_shrub_biomass) +
    geom_point(aes(x = Shrub_Height_cm, y= biomass_per_m2), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = Shrub_Height_cm, y= biomass_per_m2), colour = "brown",method = "lm") +
    ylab("Full shrub AGB (g/m2)") +
    xlab("\nHeight (cm)") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))


# Logan: Salix pulchra
logan_model <- lm(AGB_g ~ Height_cm, data = Logan_salix_pulchra)
summary(logan_model)
tab_model(logan_model)

(plot_logan_model <- ggplot(Logan_salix_pulchra) +
    geom_point(aes(x = Height_cm, y= AGB_g), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = Height_cm, y= AGB_g), colour = "brown",method = "lm") +
    ylab("AGB (g)") +
    xlab("\nHeight (cm)") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))
