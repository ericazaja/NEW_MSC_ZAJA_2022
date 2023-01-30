# CG cover and biomass script
#### Script by Erica Zaja, created 30/01/23
### Last updated: 30/01/23

# Workflow: 
# calculate % cover  of each individual shrub in the garden
# based on their widths and imagining there is a 1m2 plot around each shrub.
# i.e. calculate % of this plot occupied by each shrub each year

# 1. Libraries -------
library(tidyverse)
library(readr)

# 2. Loading data -------
# all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")
all_CG_growth <- read_csv("data/common_garden_shrub_data/all_CG_growth.csv")


# 3. Data wrangling ------
# filter dataset to retain only population "northern" and "southern" i.e. common garden only
all_CG_growth <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))

# all_CG_growth <- all_CG_growth[, -c(1:5)]

# write.csv(all_CG_growth, "data/common_garden_shrub_data/all_CG_growth.csv")

# reclassing variables
all_CG_growth$Species <- as.factor(all_CG_growth$Species)
all_CG_growth$SampleID_standard <- as.factor(all_CG_growth$SampleID_standard)
all_CG_growth$population <- as.factor(all_CG_growth$population)
all_CG_growth$Site <- as.factor(all_CG_growth$Site)
all_CG_growth$Sample_Date <- as.POSIXct(all_CG_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_growth$Year <- as.factor(all_CG_growth$Year)
all_CG_growth$Sample_age <- as.factor(all_CG_growth$Sample_age)
unique(all_CG_growth$population)
unique(all_CG_growth$Site)
unique(all_CG_growth$Species)
view(all_CG_growth)

# calculate cover based on widths
all_CG_growth_cover <- all_CG_growth %>%
  mutate(cover = (Width_cm*Width_2_cm)/10000)%>%
  mutate(cover_percent = cover *100) %>%
  filter(cover_percent <=100) # setting max to 100% cover 

# 3. Data visualisation -----

# Cover over time CG (2013-2022) ----
(scatter_cover_CG <- ggplot(all_CG_growth_cover) +
   geom_smooth(aes(x = Sample_age, y =cover_percent, colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= cover_percent, colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Cover in 1m2 (%)") +
   xlab("\n Sample age (n years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.8) +
   scale_fill_viridis_d(begin = 0.1, end = 0.8) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))





