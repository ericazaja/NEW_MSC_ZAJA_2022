# CG cover and biomass calculation and visualisation script
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
unique(all_CG_growth$population)
unique(all_CG_growth$Site)
unique(all_CG_growth$Species)
view(all_CG_growth)

# calculate cover based on widths
all_CG_growth_cover <- all_CG_growth %>%
  mutate(cover = (Width_cm*Width_2_cm)/10000)%>%
  mutate(cover_percent = cover *100) %>%
  filter(cover_percent <=100) # setting max to 100% cover 

all_CG_growth_cover$Sample_age <- as.numeric(all_CG_growth_cover$Sample_age )

# calculate biomass for each species based on allometric equations
# Salix richardsonii: Biomass =  17.971*height + 11.882*cover +- 444.5
all_CG_growth_cover_biomass_SALRIC <- all_CG_growth_cover %>%
  filter(Species == "Salix richardsonii" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (17.971*Canopy_Height_cm) + (11.882*cover_percent), 
         biomass_error_high = biomass_per_m2 + 444.5,
         biomass_error_low = biomass_per_m2 - 444.5)

# Salix pulchra: Biomass = -23.74*height + 61.75*cover +- 480.8
all_CG_growth_cover_biomass_SALPUL <- all_CG_growth_cover %>%
  filter(Species == "Salix pulchra" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (-23.74*Canopy_Height_cm) + (61.75*cover_percent), 
         biomass_error_high = biomass_per_m2 + 480.8,
         biomass_error_low = biomass_per_m2 - 480.8)

# Salix arctica: Biomass = 2.10*height + 14.35*cover +- 28.46 
all_CG_growth_cover_biomass_SALARC <- all_CG_growth_cover %>%
  filter(Species == "Salix arctica" & population == "Southern") %>% # only keeping southern shrubs
  mutate(biomass_per_m2 = (2.10*Canopy_Height_cm) + (14.35*cover_percent), 
         biomass_error_high = biomass_per_m2 + 28.46 ,
         biomass_error_low = biomass_per_m2 - 28.46  )

all_CG_growth_cover_biomass <- rbind(all_CG_growth_cover_biomass_SALARC, 
                                     all_CG_growth_cover_biomass_SALPUL,
                                     all_CG_growth_cover_biomass_SALRIC)

# 3. Modelling growth over time
# 3.1. COVER (only southern shrubs) over time -----
all_CG_growth_cover$Species <- as.factor(all_CG_growth_cover$Species)
all_CG_growth_cover$Year <- as.factor(all_CG_growth_cover$Year)
unique(all_CG_growth_cover$population)

all_CG_growth_cover_southern <- all_CG_growth_cover %>%
  filter(population == "Southern")

# cover in the garden over time, NB only southern population shrubs!
cover_mod <- lmer(cover_percent ~ Sample_age + (1|Species), data = all_CG_growth_cover_southern)
tab_model(cover_mod)
plot(cover_mod)

# 3.2. BIOMASS (only southern shrubs) over time -----
biomass_mod <- lmer(biomass_per_m2 ~ Sample_age + (1|Species), data =all_CG_growth_cover_biomass)
tab_model(biomass_mod)
plot(biomass_mod)

# 4. Data visualisation -----

# Cover over time CG (2013-2022):both northern and southern shrubs
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

# Cover over time CG (2013-2022):only southern shrubs
(scatter_cover_CG <- ggplot(all_CG_growth_cover_southern) +
    geom_smooth(aes(x = Sample_age, y =cover_percent, colour = Species, fill = Species, method = "glm")) +
    geom_point(aes(x = Sample_age, y= cover_percent, colour = Species, fill = Species), size = 1.5, alpha = 0.5) +
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

# biomass over time CG (2013-2022) 
(scatter_cover_CG <- ggplot(all_CG_growth_cover_biomass) +
    geom_smooth(aes(x = Sample_age, y =biomass_per_m2, colour = Species, fill= Species, method = "glm")) +
    geom_point(aes(x = Sample_age, y= biomass_per_m2, colour = Species, fill = Species), size = 1.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Biomass (g/m2)") +
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

# predicted biomass: salix rich.
(scatter_biomass_CG_SALRICH <- ggplot(all_CG_growth_cover_biomass_SALRIC) +
    geom_point(aes(x = Sample_age, y= biomass_per_m2), size = 1.5, alpha = 0.5, colour ="dark green" ) +
    geom_smooth(aes(x = Sample_age, y= biomass_per_m2), method = "lm", colour = "dark green", fill = "dark green")) +
    ylab("Salix richardsonii biomass (g/m2)") +
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
          axis.text.y = element_text(size = 15, colour = "black"))

# predicted biomass: salix pulchra
(scatter_biomass_CG_SALPUL <- ggplot(all_CG_growth_cover_biomass_SALPUL) +
    geom_point(aes(x = Sample_age, y= biomass_per_m2), size = 1.5, alpha = 0.5, colour ="dark green" ) +
    geom_smooth(aes(x = Sample_age, y= biomass_per_m2) ,method = "lm", colour = "dark green", fill = "dark green")) +
  ylab("Salix pulchra biomass (g/m2)") +
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
        axis.text.y = element_text(size = 15, colour = "black"))

# predicted biomass: salix arctica
(scatter_biomass_CG_SALARC <- ggplot(all_CG_growth_cover_biomass_SALARC) +
    geom_point(aes(x = Sample_age, y= biomass_per_m2), size = 1.5, alpha = 0.5, colour ="dark green" ) +
    geom_smooth(aes(x = Sample_age, y= biomass_per_m2), method = "lm",  colour = "dark green", fill = "dark green")) +
  ylab("Salix arctica biomass (g/m2)") +
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
        axis.text.y = element_text(size = 15, colour = "black"))






