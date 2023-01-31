# ITEX shrub cover over time 
#### Script by Erica Zaja, created 30/01/23
### Last updated: 31/01/23

# Workflow:

# Libraries:
library(tidyverse)
library(readr)

# 1. Loading data -------
# load("~/Desktop/Github_repos.tmp/NEW_MSC_ZAJA_2022/data/ITEX/ITEX_database.RData")
ITEX_veg_msc <- read_csv("data/ITEX/ITEX_veg_msc.csv")

# 2. DATA WRANGLING ----

# Exploration 
max(ITEX_EZ_diss$YEAR) # latest year: 2020
min(ITEX_EZ_diss$YEAR) # earliest year: 1981
unique(ITEX_EZ_diss$SITE) # Unique site names

# Retaining only QHI, Arctic National Wildlife Refuge (ANWR), Toolik site 
ITEX_veg_msc <- ITEX_EZ_diss %>%
  filter(SITE %in% c("ANWR", "TOOLIK", "QHI")) %>% 
  na.omit()

# write.csv(ITEX_veg_msc, "data/ITEX/ITEX_veg_msc.csv")

# exploring the new dataset
range(ITEX_veg_msc$YEAR) # Range of years of data: 1996-2019
length(unique(ITEX_veg_msc$YEAR)) # 23 years
unique(ITEX_veg_msc$YEAR) # Unique years
length(unique(ITEX_veg_msc$PLOT)) #85 plots
length(unique(ITEX_veg_msc$SiteSubsitePlotYear)) # 871 unique plot and year combos

# Group the dataframe by year to see the number of plots per year
itex_plots <- ITEX_veg_msc %>%
  group_by(YEAR) %>%
  summarise(plot.n = length(unique(SiteSubsitePlot))) %>% 
  ungroup() # different amount of plots each year

unique(ITEX_veg_msc$FuncGroup) # Unique functional groups names
# [1] "Shrub"     "Lichen"    "Moss"      "Forb"      "Graminoid"
length(unique(ITEX_veg_msc$GENUS)) # 127 genera

unique(ITEX_veg_msc$gridcell) #"[1] "_68.5_-149.5" "_68.5_-149"   "_68_-149"    
# "_69.5_-143.5" "_69.5_-138.5"
# 5 grid cells, could use as random effect?

# Filtering shrub only data: salix arctica and pulchra, no rich.
ITEX_shrubs_msc <-  ITEX_veg_msc %>% 
  filter (FuncGroup == "Shrub") %>% 
  filter(GENUS == "Salix") %>% 
  filter(SPECIES_NAME %in% c("Salix arctica", "Salix pulchra"))

ITEX_shrubs_msc$SPECIES_NAME <- as.factor(ITEX_shrubs_msc$SPECIES_NAME )

# separating pulchra and arctica
ITEX_pulchra <- ITEX_shrubs_msc %>%
  filter(SPECIES_NAME == "Salix pulchra")

ITEX_arctica <- ITEX_shrubs_msc %>%
  filter(SPECIES_NAME == "Salix arctica")%>%
  filter(SITE %in% c("ANWR", "QHI" ))


# 3. DATA VIS ------
# THEME ----
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}


# Visualising distribution with a histogram
(hist_arctica <- ITEX_arctica %>%
    ggplot(aes(x = RelCover, fill = SITE)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
    labs(x = "\nCover (%)", y = "Frequency\n") +
    scale_fill_manual(values = c("#332288", "#DDCC77", "dark green"), name = "Species")+
    theme_shrub() +
    theme(legend.text = element_text(size=20),
          legend.title = element_text(size=25)) )

(hist_pulchra <- ITEX_pulchra %>%
    ggplot(aes(x = RelCover, fill = SITE)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
    labs(x = "\nCover (%)", y = "Frequency\n") +
    scale_fill_manual(values = c("#332288", "#DDCC77", "dark green"), name = "Species")+
    theme_shrub() +
    theme(legend.text = element_text(size=20),
          legend.title = element_text(size=25)) )

(scatter_arctica <- ggplot() +
    geom_point(aes(x = YEAR , y= RelCover, colour = SITE, fill = SITE), size = 3, alpha = 0.5, data = ITEX_arctica) +
    geom_smooth(aes(x = YEAR , y= RelCover,  colour = SITE, fill = SITE), method = "lm", data = ITEX_arctica) +
    ylab("Relative cover (%)") +
    xlab("\nYear") +
    #facet_wrap(~SITE, scales = "free") +
    scale_colour_viridis_d(begin = 0.6, end = 0.1) +
    scale_fill_viridis_d(begin = 0.6, end = 0.1) + 
    theme_shrub() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

(scatter_pulchra <- ggplot() +
    geom_point(aes(x = YEAR , y= RelCover, colour = SITE, fill = SITE), size = 3, alpha = 0.5, data = ITEX_pulchra) +
    geom_smooth(aes(x = YEAR , y= RelCover,  colour = SITE, fill = SITE), method = "lm", data = ITEX_pulchra) +
    ylab("Relative cover (%)") +
    xlab("\nYear") +
    #facet_wrap(~SITE, scales = "free") +
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





