### Making species-specific allometric equations for Salix spp. (richardsonii, pulchra, arctica)
### Script by Erica Zaja
### Last updated: 29/12/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(readxl)
library(sjPlot)
library(gridExtra)

# 2. LOADING DATA ----

# a. Biomass harvests (grams) and heights (cm) from Andrew Cunliffe et al 2020: QHI 
# N.B. data only for 50 x 50 cm plots (i.e biomass might not be for the full shrub)
Andy_biomass <- read_csv("data/allometry/Andy_paper/biomass.csv")
Andy_heights <- read_excel("data/allometry/Andy_paper/heights.xlsx")
# Andy_main_database <- read_excel("data/allometry/Andy_paper/main_database.xlsx") # no need to read this in

# b. Biomass harvests and heights from Isla Myers-Smith PhD: Pika Camp - date? 
# N.B. data only for 50 x 50 cm plots (i.e biomass might not be for the full shrub)
Pika_heights_edit <- read_excel("data/allometry/Isla_phd/Pika_biomass_heights_edit.xlsx")
Biomass_Calculations_Pika <- read_excel("data/allometry/Isla_phd/Biomass Calculations Pika.xlsx")
Percent_cover_Pika <- read_excel("data/allometry/Isla_phd/Percent_cover_Pika.xlsx")
# Heights_Regression_Pika <- read_excel("data/allometry/Isla_phd/Heights_Regression_Pika.xlsx") # no need to read in
# Biomass_harvest_Pika <- read_excel("data/allometry/Isla_phd/Biomass_harvest_Pika.xlsx") # no need to read in

# c. Biomass harvests and heights from Logan Berner's 2015 paper: Alaska 
# N.B. biomass (g) data for the full shrub - or is it g/m2?
Logan_data_biomass <- read_excel("data/allometry/Berner/Logan-data-biomass.xlsx")

# 3. DATA WRANGLING -----

# Workflow explained -----

# For Andy and Isla's data: 
# I am keeping max values (of height, biomass and cover),
# when there are multiple values per plot. 
# N.B. We are using maximum heights because that is the data that we have from the common garden. 

# Incorporating cover into the relationship (height VS biomass) by dividing biomass by cover in the 
# 50x50cm quadrat and then multiplying by 100 (i.e what biomass of the full shrub would be?).

# Once i have indexed biomass, I multiply by 4 to obtain biomass in g/m2. 
# N.B. For point framing (Andy's data), I divide the "count" by the total number of points (36) 
# and then multiply by 100 to obtain % cover. 
# I then I go on to calculate indexed biomass and convert to g/m2.

# 3.1. QHI SALIX PULCHRA and SALIX ARCTICA (Andy) ----

Andy_biomass <- Andy_biomass[-1,] # removing row with words

# retaining salix target species
Andy_heights_salix <- Andy_heights %>%
  filter(Species %in% c("Salix arctica", "Salix richardsonii"))

# reclassing height 
Andy_heights_salix$Height <- as.numeric(Andy_heights_salix$Height)

# removing NAs 
Andy_heights_salix <- Andy_heights_salix %>% na.omit()  

# reclassing variables
Andy_heights_salix$PlotN <- as.factor(Andy_heights_salix$PlotN)
Andy_heights_salix$Count <- as.numeric(Andy_heights_salix$Count)
Andy_heights_salix$Species <- as.factor(Andy_heights_salix$Species)

# Multiple heights are measured (while point framing), so retaining maximum heights (which are shrubs).
# The salix rich. shrubs are always the tallest thing in the plots.
# The shrub is always Salix richardsonii or Salix arctica.  
# Only use the Salix arctica data if there is no/less richardsonii in the plot.
# Calculating max heights and max count per species per plot

Andy_maxcount_maxheight <- Andy_heights_salix %>% 
  select(Species, Height, Count, PlotN) %>% # selecting columns of interest
  group_by(PlotN, Species) %>%
  mutate(max_count = max(Count), max_height = max(Height)) %>%
  select(Species, PlotN, max_count, max_height) %>%
  distinct() # only keeping one set of indentical observations

# Filter salix richardsonii and salix arctica
Andy_salric <- Andy_maxcount_maxheight %>%
  filter(Species == "Salix richardsonii")%>%
  na.omit() %>%
  rename("PlotID" = "PlotN")

Andy_salarc <- Andy_maxcount_maxheight %>%
  filter(Species == "Salix arctica") %>%
  rename("PlotID" = "PlotN")

# removing plot 1, 3 and 35 from sal arc. (because those plots have higher sal rich. count)
Andy_salarc <- Andy_salarc[-c(1,3,7),]

# removing plot 2 from sal. rich. because high sal arc in that plot
Andy_salric <- Andy_salric[-2,]

# merging salix arc heights and biomass datasets by PlotID
QHI_salarc_biomass <- full_join(Andy_salarc , Andy_biomass, 
                                by = "PlotID")%>%
                      na.omit()

# merging salix rich heights and biomass datasets by PlotID
QHI_salric_biomass <- full_join(Andy_salric , Andy_biomass, 
                             by = "PlotID")%>%
                      na.omit()

# renaming columns
QHI_salarc_biomass <- QHI_salarc_biomass %>%
  rename("Woody_stem_biomass" = "Woody stem biomass",
         "Shrub_leaf_biomass" = "Shrub leaf biomass")

QHI_salric_biomass <- QHI_salric_biomass %>%
  rename("Woody_stem_biomass" = "Woody stem biomass",
         "Shrub_leaf_biomass" = "Shrub leaf biomass")

# reclassing variables
QHI_salarc_biomass$PlotID <- as.factor(QHI_salarc_biomass$PlotID)
QHI_salarc_biomass$Shrub_leaf_biomass <- as.numeric(QHI_salarc_biomass$Shrub_leaf_biomass)
QHI_salarc_biomass$Woody_stem_biomass<- as.numeric(QHI_salarc_biomass$Woody_stem_biomass)
QHI_salarc_biomass$max_count<- as.numeric(QHI_salarc_biomass$max_count)

QHI_salric_biomass$PlotID <- as.factor(QHI_salric_biomass$PlotID)
QHI_salric_biomass$Shrub_leaf_biomass <- as.numeric(QHI_salric_biomass$Shrub_leaf_biomass)
QHI_salric_biomass$Woody_stem_biomass<- as.numeric(QHI_salric_biomass$Woody_stem_biomass)
QHI_salric_biomass$max_count<- as.numeric(QHI_salric_biomass$max_count)

# making a total biomass column adding stem and leaf biomass, calculating % cover,
# indexing biomass and converting to g/m2 
QHI_salarc_shrub_biomass <- QHI_salarc_biomass %>%
  select(max_count, PlotID, max_height, Woody_stem_biomass, Shrub_leaf_biomass, Species)%>%
  mutate(tot_shrub_biomass = sum (Woody_stem_biomass, Shrub_leaf_biomass)) %>%
  mutate(percent_cover = (max_count/36)*100) %>% # calculating % cover from point framing (36 points tot)
  mutate(biomass_index =  (tot_shrub_biomass/percent_cover)*100) %>%
  distinct() %>%
  mutate(biomass_per_m2 = (biomass_index*4)) # times 4 to make biomass/m2

QHI_salric_shrub_biomass <- QHI_salric_biomass %>%
  select(max_count, PlotID, max_height, Woody_stem_biomass, Shrub_leaf_biomass, Species)%>%
  mutate(tot_shrub_biomass = sum (Woody_stem_biomass, Shrub_leaf_biomass)) %>%
  mutate(percent_cover = (max_count/36)*100) %>% # calculating % cover from point framing (36 points tot)
  mutate(biomass_index =  (tot_shrub_biomass/percent_cover)*100) %>%
  distinct() %>%
  mutate(biomass_per_m2 = (biomass_index*4)) # times 4 to make biomass/m2

# Adding zeros row to set intercept to zero: i.e. if height 0, biomass 0
PlotID <- "P00"
max_height <- 0
Woody_stem_biomass <- 0
Shrub_leaf_biomass <- 0
Species <- "Salix richardsonii" # N.B. change name here to spp. needed and re run
tot_shrub_biomass <- 0
percent_cover <- 0
biomass_index <- 0
biomass_per_m2 <- 0
max_count <- 0

# making into vector of zeros
zeros_salric <- data.frame(PlotID, max_height, 
                    Woody_stem_biomass, Shrub_leaf_biomass, Species,
                    tot_shrub_biomass, percent_cover, 
                    biomass_index, biomass_per_m2, max_count)

zeros_salarc <- data.frame(PlotID, max_height, 
                            Woody_stem_biomass, Shrub_leaf_biomass, Species,
                            tot_shrub_biomass, percent_cover, 
                            biomass_index, biomass_per_m2, max_count)

# adding zero vector to the full dataset
QHI_salric_shrub_biomass <- rbind(QHI_salric_shrub_biomass, zeros_salric)
QHI_salarc_shrub_biomass <- rbind(QHI_salarc_shrub_biomass, zeros_salarc)

# ANDY DATA DONE 

# 3.2. PIKA SALIX PULCHRA and SALIX RICHARDSONII (Isla) ----

# renaming columns of biomass dataset
Biomass_Pika <- Biomass_Calculations_Pika %>%
  select(Plot...2, `Tall Shrubs...12`) %>%
  rename("Tall_shrub_biomass"= "Tall Shrubs...12",
         "Plot" = "Plot...2")

# removing zeros or NAs
Biomass_Pika[Biomass_Pika == 0.000] <- NA
Biomass_Pika <- Biomass_Pika %>%
  na.omit()

# renaming columns of percentage cover dataset
Percent_cov_Pika <- Percent_cover_Pika %>%
  select(Plot, `Tall Shrubs...10`) %>%
  na.omit() %>%
  rename("Tall_shrub_cov"= "Tall Shrubs...10")

# removing zeros or NAs
Percent_cov_Pika[Percent_cov_Pika == 0.0000000] <- NA
Percent_cov_Pika <- Percent_cov_Pika %>%
  na.omit()

# keeping relevant columns of height dataset
Heights_Pika <- Pika_heights_edit %>%
  select(Plot, Species, Shrub_Height_cm)

# reclassing variables
Heights_Pika$Plot <- as.factor(Heights_Pika$Plot)
Percent_cov_Pika$Plot <- as.factor(Percent_cov_Pika$Plot)
Biomass_Pika$Plot <- as.factor(Biomass_Pika$Plot)

# joining heights and biomass datasets by Plot
Pika_all <- print(list(Heights_Pika,Biomass_Pika, Percent_cov_Pika) %>% 
                    reduce(left_join, by='Plot')) %>%
              distinct() 

# there are multiple biomass and cover values per height, so I kept max biomass per height.
# indexing biomass and converting to g/m2 
Pika_all_shrub_biomass <- Pika_all %>%
  group_by(Plot) %>%
  mutate(max_biomass = max(Tall_shrub_biomass), 
          max_cover = max(Tall_shrub_cov)) %>%
  mutate(biomass_index = (max_biomass/max_cover)*100) %>%
  select(Plot, Shrub_Height_cm, max_biomass, max_cover, biomass_index)%>%
  distinct() %>% # only keeping one set of identical observations
  mutate(biomass_per_m2 = (biomass_index*4)) # times 4 to make biomass/m2

# Adding zeros row to set intercept to zero: if height 0, biomass 0
Plot <- "00"
Shrub_Height_cm <- 0
max_biomass <- 0
max_cover <- 0
biomass_index <- 0
biomass_per_m2 <- 0
# making zeros vector
zeros_pika <- data.frame(Plot, Shrub_Height_cm, max_biomass, max_cover, biomass_index, biomass_per_m2)

# adding zero vector to full dataset
Pika_all_shrub_biomass <- rbind(Pika_all_shrub_biomass, zeros_pika)

# ISLA DATA DONE 

# 3.3. ALASKA SALIX PULCHRA (Logan Berner)----

Logan_salix_pulchra <- Logan_data_biomass %>% 
  filter(Genus == "Salix" & Species == "pulchra") %>% # keeping target salix
  select(Genus, Species, Region, Ecosystem, "Height (cm)", "AGB (g)") %>% # columns of interest
  na.omit() %>%
  rename("AGB_g" = "AGB (g)",
         "Height_cm" = "Height (cm)")

# LOGAN DATA DONE

# 4. MODELLING and DATA VIS: regression  biomass ~ height ---- 

# Andy: Salix richardsonii
andy_model_salric <- lm(biomass_per_m2 ~ max_height, data = QHI_salric_shrub_biomass)
summary(andy_model_salric)
tab_model(andy_model_salric)
# biomass increases with height

# Scatter salix richardsonii
(plot_andy_model_salric <- ggplot(QHI_salric_shrub_biomass) +
    geom_point(aes(x = max_height, y= biomass_per_m2), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = max_height, y= biomass_per_m2), colour = "brown",method = "lm") +
    ylab("Full shrub AGB (g/m2)") +
    xlab("\nHeight (cm)") +
    ggtitle("Cunliffe, 2020: Salix richardsonii, QHI") +
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

# Andy: Salix arctica
andy_model_salarc <- lm(biomass_per_m2 ~ max_height, data = QHI_salarc_shrub_biomass)
summary(andy_model_salarc)
tab_model(andy_model_salarc)
# biomass increases with height

# Scatter salix arctica
(plot_andy_model_salarc <- ggplot(QHI_salarc_shrub_biomass) +
    geom_point(aes(x = max_height, y= biomass_per_m2), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = max_height, y= biomass_per_m2), colour = "brown",method = "lm") +
    ylab("Full shrub AGB (g/m2)") +
    xlab("\nHeight (cm)") +
    ggtitle("Cunliffe, 2020: Salix arctica, QHI") +
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

# panel
panel_Andy <- grid.arrange(plot_andy_model_salarc, plot_andy_model_salric, nrow=1)

# Isla: Salix pulchra and richardsonii
isla_model <- lm(biomass_per_m2 ~ Shrub_Height_cm, data = Pika_all_shrub_biomass)
summary(isla_model)
tab_model(isla_model)
# biomass increases with height. But not significant relationship

# Scatter
(plot_isla_model <- ggplot(Pika_all_shrub_biomass) +
    geom_point(aes(x = Shrub_Height_cm, y= biomass_per_m2), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = Shrub_Height_cm, y= biomass_per_m2), colour = "brown",method = "lm") +
    ylab("Full shrub AGB (g/m2)") +
    xlab("\nHeight (cm)") +
    ggtitle("Myers-Smith, date?: Salix richardsonii and pulchra, Pika Camp") +
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
# biomass increases with height 

# Scatter
(plot_logan_model <- ggplot(Logan_salix_pulchra) +
    geom_point(aes(x = Height_cm, y= AGB_g), size = 3, alpha = 0.5) +
    geom_smooth(aes(x = Height_cm, y= AGB_g), colour = "brown",method = "lm") +
    ylab("Full shrub AGB (g)") +
    xlab("\nHeight (cm)") +
    ggtitle("Berner 2015: Salix pulchra, Alaska ") +
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

# panel
all_allometry <- grid.arrange(plot_andy_model, plot_isla_model, plot_logan_model,
                               ncol=1)

