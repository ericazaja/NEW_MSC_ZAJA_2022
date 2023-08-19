# Script for random bits of code not used 
# please ignore

ITEX_shrubs_msc <- read_csv("data/ITEX/itex_EZ_shrubs_2023.csv") # newest 
all_CG_source_growth <- read_csv("data/common_garden_shrub_data/all_CG_source_growth.csv")


# EXTRAS random slope models below (ignore)------

# RANDOM SLOPE MODELS (not using in thesis) 
CG_ALL_cover_biomass$Species <- as.factor(CG_ALL_cover_biomass$Species)
# different slopes of the fixed effect (the one at the top, sample age) 
# per defined categorical group (the one at the bottom, species)

# all spp
cover_random_slopes <- brms::brm((cover_percent/100) ~ Sample_age + (Sample_age|Species),
                                 data = CG_ALL_cover_biomass,  family = "beta", chains = 3,
                                 iter = 5000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cover_random_slopes) #  significant cover growth over time
plot(cover_random_slopes)
pp_check(cover_random_slopes, type = "dens_overlay", nsamples = 100) 

# only tall shrubs
pulchra_ric_cover_biomass <- CG_ALL_cover_biomass %>%
  filter(Species %in% c("Salix pulchra", "Salix richardsonii"))

tall_cover_random_slopes <- brms::brm((cover_percent/100) ~ Sample_age + (Sample_age|Species),
                                      data = pulchra_ric_cover_biomass,  family = "beta", chains = 3,
                                      iter = 5000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(tall_cover_random_slopes) #  significant cover growth over time
plot(cover_random_slopes)
pp_check(cover_random_slopes, type = "dens_overlay", nsamples = 100) 

# random slope model plot
(biomass_random_slopes_plot <- CG_ALL_cover_biomass %>%
    bind_cols(as_tibble(fitted(biom_random_slopes))) %>%
    group_by(Species) %>%
    ggplot() +
    geom_point(aes(x = Sample_age, y = (log(biomass_per_m2)))) +
    geom_point(aes(x = Sample_age, y = Estimate), shape = 1, size = 4, stroke = 1.5) +
    geom_smooth(aes(x = Sample_age, y = (log(biomass_per_m2)), colour = Species, fill = Species)) +
    labs(x = "Sample age", y = "Shrub biomass (log g/m2)",
         subtitle = "Black points are observed values. Black circles are fitted values.") +
    # scale_x_continuous(expand = c(.075, .075), breaks = 0:3) +
    #facet_wrap(~Species, nrow = 1) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +   
    theme_shrub() +
    theme(plot.title = element_text(hjust = .5)))

# random slope model plot NO ARCTICA
(tall_biom_random_slopes_plot <- pulchra_ric_cover_biomass %>%
    bind_cols(as_tibble(fitted(tall_biom_random_slopes))) %>%
    group_by(Species) %>%
    ggplot() +
    geom_point(aes(x = Sample_age, y = (log(biomass_per_m2)), colour = Species), size = 4, alpha = .75) +
    geom_point(aes(x = Sample_age, y = Estimate), shape = 1, size = 4, stroke = 1.5) +
    geom_smooth(aes(x = Sample_age, y = (log(biomass_per_m2)), colour = Species, fill = Species)) +
    labs(x = "Sample age", y = "Shrub biomass (log, g/m2)",
         subtitle = "Black circles are fitted values.") +
    # scale_x_continuous(expand = c(.075, .075), breaks = 0:3) +
    #facet_wrap(~Species, nrow = 1) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +   
    theme_shrub() +
    theme(plot.title = element_text(hjust = .5)))

# RANDOM SLOPE MODELS (not using in thesis) 
# all spp.
biom_random_slopes <- brms::brm(log(biomass_per_m2) ~ Sample_age + (Sample_age|Species),
                                data = CG_ALL_cover_biomass,  family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(biom_random_slopes) #  significant cover growth over time
plot(biom_random_slopes)
pp_check(biom_random_slopes, type = "dens_overlay", nsamples = 100) 

# only tall shrubs
tall_biom_random_slopes <- brms::brm(log(biomass_per_m2) ~ Sample_age + (Sample_age|Species),
                                     data = pulchra_ric_cover_biomass,  family = gaussian(), chains = 3,
                                     iter = 5000, warmup = 1000, 
                                     control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(tall_biom_random_slopes) #  significant cover growth over time
plot(tall_biom_random_slopes)
pp_check(tall_biom_random_slopes, type = "dens_overlay", nsamples = 100) 

# random slope model plot 
(cover_random_slopes_plot <- CG_ALL_cover_biomass %>%
    bind_cols(as_tibble(fitted(cover_random_slopes))) %>%
    group_by(Species) %>%
    ggplot() +
    geom_point(aes(x = Sample_age, y = (cover_percent/100)), size = 4, alpha = .75, color = "dodgerblue2") +
    geom_point(aes(x = Sample_age, y = Estimate), shape = 1, size = 4, stroke = 1.5) +
    geom_smooth(aes(x = Sample_age, y = (cover_percent/100), colour = Species, fill = Species)) +
    labs(x = "Sample age", y = "Shrub cover (proportion)",
         subtitle = "Blue points are observed values. Black circles are fitted values.") +
    # scale_x_continuous(expand = c(.075, .075), breaks = 0:3) +
    #facet_wrap(~Species, nrow = 1) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +   
    theme_shrub() +
    theme(plot.title = element_text(hjust = .5)))

# random slope model plot NO ARCTICA
(tall_cover_random_slopes_plot <- pulchra_ric_cover_biomass %>%
    bind_cols(as_tibble(fitted(tall_cover_random_slopes))) %>%
    group_by(Species) %>%
    ggplot() +
    geom_point(aes(x = Sample_age, y = (cover_percent/100), colour = Species), size = 4, alpha = .75) +
    geom_point(aes(x = Sample_age, y = Estimate), shape = 1, size = 4, stroke = 1.5) +
    geom_smooth(aes(x = Sample_age, y = (cover_percent/100), colour = Species, fill = Species)) +
    labs(x = "Sample age", y = "Shrub cover (proportion)",
         subtitle = "Black circles are fitted values.") +
    # scale_x_continuous(expand = c(.075, .075), breaks = 0:3) +
    #facet_wrap(~Species, nrow = 1) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +   
    theme_shrub() +
    theme(plot.title = element_text(hjust = .5)))

# IGNORE Slope vs slope models -----
# S. Pulchra slope vs slope -----
temp_time_pul <- temp_time_random_year %>%
  filter(Site %in% c("QHI", "TOOLIK"))

temp_cover_pul <- full_join(temp_time_pul, cov_time_pul_random_new, by = c("Site"="Site"))

view(temp_cover_pul)

temp_time <- brms::brm(sitesubsiteplot_index_year_estimate ~ index_year_estimate + (1|Site),
                       data = temp_cover_pul,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(temp_time)
pp_check(temp_time, type = "dens_overlay", nsamples = 100) 

(temp_time_plot <- temp_cover_pul %>%
    group_by(Site) %>%
    add_predicted_draws(temp_time) %>%
    ggplot(aes(x =index_year_estimate, y =sitesubsiteplot_index_year_estimate , color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = temp_cover_pul) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Cover change (scaled) \n") +
    xlab("\nTemperature change (scaled)")+ theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica slope vs slope -----
temp_time_arc <- temp_time_random_year %>%
  filter(Site %in% c("QHI", "ANWR"))

temp_cover_arc <- full_join(temp_time_arc, cov_time_arc_random_new, by = c("Site"="Site"))

view(temp_cover_arc)

temp_time_arc_mod <- brms::brm(sitesubsiteplot_index_year_estimate ~ index_year_estimate + (1|Site),
                               data = temp_cover_arc,  family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(temp_time_arc_mod)
pp_check(temp_time_arc_mod, type = "dens_overlay", nsamples = 100) 

(temp_time_arc_plot <- temp_cover_arc %>%
    group_by(Site) %>%
    add_predicted_draws(temp_time_arc_mod) %>%
    ggplot(aes(x =index_year_estimate, y =sitesubsiteplot_index_year_estimate , color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = temp_cover_arc) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Cover change (scaled) \n") +
    xlab("\nTemperature change (scaled)")+ theme_shrub() +
    labs(title = "Salix arctica"))

# IGNORE BELOW -----
# 2. cover over time only CG 
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

all_CG_source_growth_cover <- all_CG_growth_cover %>%
  dplyr::select(Species, SampleID_standard, population, Sample_age, cover_percent) %>%
  mutate(Site = case_when(population =="Southern" ~ "CG",
                          population == "source_south" ~ "KP", 
                          population == "source_north" ~ "QHI"))%>%
  rename("Plot" = "SampleID_standard", "Year"="Sample_age") %>%
  dplyr::select(-population)%>%
  na.omit()


ITEX_shrubs_cover <- ITEX_shrubs_msc %>%
  filter(SurveyedArea %in% c(1.0000, 1))%>% # keeping only 1m2 quadrat
  # filter(SITE!= "QHI") %>%
  dplyr::select(YEAR, PLOT, SPECIES_NAME, SITE, RelCover) %>%
  rename("Year"="YEAR", "Plot"= "PLOT", "Species" = "SPECIES_NAME", "Site"= "SITE", "cover_percent"= "RelCover") %>%
  na.omit()


all_cover_long <- rbind(ITEX_shrubs_cover, all_CG_source_growth_cover)
all_cover_long <- all_cover_long %>%
  mutate(cover_prop = cover_percent/100)%>%
  filter(Site == "CG")

all_cover_long$Site <- as.factor(all_cover_long$Site)                    
all_cover_long$Species <- as.factor(all_cover_long$Species)
all_cover_long$Plot <- as.factor(all_cover_long$Plot)

hist(all_cover_long$cover_prop)

all_cover_long_rich <- all_cover_long %>%
  filter(Species == "Salix richardsonii") # no richardsonii in 3 sites 
all_cover_long_pul <- all_cover_long %>%
  filter(Species == "Salix pulchra")
all_cover_long_arc <- all_cover_long %>%
  filter(Species == "Salix arctica")

# richardsonii only in CG
cov_time_rich <- brms::brm(cover_prop ~ Year + (1|Plot),
                           data = all_cover_long_rich,  family = "beta", chains = 3,
                           iter = 5000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(cov_time_rich)

(cov_time_plot <- all_cover_long_rich %>%
    add_predicted_draws(cov_time_rich) %>%
    ggplot(aes(x = Year, y =cover_prop, colour= Site, fill =Site)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = all_cover_long_rich) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    ylab("Cover (proportion) \n") +
    xlab("\nYear (scaled)")+ theme_shrub()+
    labs(title = "Salix richardsonii in CG"))

cov_time_CG_rich <- as.data.frame(ranef(cov_time_rich)) # extract random eff. slopes 


# Something is wrong here - check with Mariana
test <- QHI_1999_2022 %>% 
  mutate(Year_index = I(YEAR - 1998)) %>%
  group_by(SUBSITE, PLOT, YEAR) %>%
  summarise(HeightMax = max(Height))

# DATA WRANGLE 
# make a subsite plot year x y column 
QHI_2022 <- QHI_1999_2022 %>% select(SUBSITE, PLOT, YEAR, X, Y, SPP, STATUS, Height) %>% 
  filter(SPP == "Salix pulchra" & STATUS == "LIVE")

QHI_2022_sum_max <- QHI_2022 %>% 
  group_by(SUBSITE, PLOT, YEAR, SPP) %>%
  summarise(HeightMax = max(Height)) %>%
  mutate(Height_mm = case_when(HeightMax >= 100 ~ "TRUE", HeightMax < 100 ~ "FALSE")) %>%
  select(-HeightMax)


QHI_2022_cm <- QHI_2022 %>% 
  left_join(QHI_2022_sum_max) %>% 
  mutate(Height_cm = case_when(Height_mm == "TRUE" ~ Height/10, 
                               Height_mm == "FALSE"~ Height)) %>% 
  # filter(Height_cm > 10) %>% # This will remove things like roots and basal stems
  select(-Height) %>%
  na.omit(Height_cm) 

# keeping only max values
QHI_2022_max <- QHI_2022_cm %>%
  group_by(SUBSITE, PLOT, YEAR, SPP, X, Y) %>%
  summarise(max_heights_cm = max(Height_cm)) %>%
  distinct() %>% # keeping one unique value
  na.omit(max_heights_cm) %>%
  mutate(max_heights_integer = round(max_heights_cm))

range(QHI_2022_max$max_heights_cm) # 0.2 39.3 cm 
hist(QHI_2022_max$max_heights_cm, breaks = 30) # not normal at all

QHI_2022_max$SubsitePlot <- with(QHI_2022_max, paste0(SUBSITE, PLOT))

# calculating a plot mean per year
QHI_2022_max_2 <- QHI_2022_max %>%
  group_by(SubsitePlot, YEAR) %>%
  summarise(plot_mean = mean(max_heights_cm))
hist(QHI_2022_max_2$plot_mean, breaks=30)

# calculating a plot max
QHI_2022_max_3 <- QHI_2022_max %>%
  group_by(SubsitePlot, YEAR) %>%
  summarise(plot_max = max(max_heights_cm))
hist(QHI_2022_max_3$plot_max, breaks = 30)

#QHI_2022_max <- QHI_2022_max %>%
# mutate(log_max_heights = log(max_heights_cm))

# MODELLING
# Model QHI pulchra heights over time

range(QHI_2022_max$max_heights_cm) #  0.0 39.3
hist(QHI_2022_max$max_heights_cm, breaks = 30)
hist(QHI_2022_max$log_max_heights, breaks = 30)
hist(QHI_2022_max$max_heights_integer, breaks = 30)

QHI_2022_max$SubsitePlot <- with(QHI_2022_max, paste0(SUBSITE, PLOT))
QHI_2022_max <- QHI_2022_max %>% 
  mutate(Year_index = I(YEAR - 1998)) 

QHI_2022_max$SubsitePlot <- as.factor(QHI_2022_max$SubsitePlot)
unique(QHI_2022_max$SubsitePlot)

QHI_height_time <- brms::brm(max_heights_cm ~ Year_index + (Year_index|SubsitePlot),
                             data = QHI_2022_max,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))


(pulchra_height_plot <- QHI_2022_max %>%
    group_by(SubsitePlot)%>%
    add_predicted_draws(QHI_height_time) %>%
    ggplot(aes(x = Year_index, y = max_heights_cm)) +
    stat_lineribbon(aes(y = .prediction), .width = .50, alpha = 1/4) +
    geom_point(data = QHI_2022_max) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Salix pulchra height (cm) \n") +
    xlab("\nYear (scaled)") +
    theme_shrub())

QHI_height_random <- as.data.frame(ranef(QHI_height_time)) # extract random eff. slopes 
QHI_height_random$Plot <- row.names(QHI_height_random) # Apply row.names function
rownames(QHI_height_random) <- NULL
colnames(QHI_height_random)[5] <- "plot_height_index_year_estimate" 
colnames(QHI_height_random)[6] <- "plot_height_index_year_error" 
colnames(QHI_height_random)[7] <- "plot_height_index_year_Q_25" 
colnames(QHI_height_random)[8] <- "plot_height_index_year_Q_97"
view(QHI_height_random)

QHI_height_random_year <- QHI_height_random %>%
  dplyr::select("Plot","plot_height_index_year_estimate" ,"plot_height_index_year_error", "plot_height_index_year_Q_25",
                "plot_height_index_year_Q_97")%>%
  mutate(Site = rep("QHI"))
view(QHI_height_random_year)




# hdd.cdd.2023 = crop(tasmax.2023, boundary) # crop to the extent of the PCH range
# df_2023_july_85 <- as.data.frame(r3, xy=TRUE)

# Aggregated Temperature

indices = which((tasmax.dates_2 >= as.Date(paste0("2060-01-01"))) &
                  (tasmax.dates_2 <= as.Date(paste0("2060-12-31"))))

tasmax.2060 = tasmax.scenes[[indices[1]]]

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2060) = pmax(values(tasmax.2060), values(scene)) }

plot(tasmax.2060, main="2060", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))


# Extract 

weather_station = st_sfc(st_point(c(-88.27778, 40.03972)), crs=4326)

y = sapply(tasmax.scenes, function(scene)  extract(scene, as_Spatial(poly)))

x = 1970 + (as.numeric(tasmax.dates) / 365.25)

tasmax.series = ts(y, start=floor(min(x)), end=floor(max(x)), deltat=1/12)

plot(tasmax.series, col="darkred", ylab="Monthly Mean High Temperatures (F)",
     type="l", lwd=3, bty="n", las=1, fg=NA)

grid(nx=NA, ny=NULL, lty=1)

decomposition = stl(tasmax.series, s.window=240, t.window=120)

plot(decomposition)

# Aggregated Baseline Rasters (2020)

indices = which((tasmax.dates <= as.Date(paste0("2020-12-31"))) & 
                  (tasmax.dates >= as.Date(paste0("2020-01-01"))))

tasmax.2020 = tasmax.scenes[[indices[1]]]


plot(hdd.cdd.2060, main="2020", col = colorRampPalette(c('navy', 'lightgray', 'red'))(32))

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
  values(tasmax.2020) = pmax(values(hdd.cdd.2060), values(scene)) }



# EXTRAS ----
#p50_2020_resample_df <- extract(p50_2020_resample, xy, cellnumbers = T)
#view(p50_2020_resample_df)

#p50_2020_random <- as.data.frame(sampleRandom(p50_2020_resample, 264, na.rm=TRUE, ext=NULL, 
#                                           cells=TRUE, rowcol=FALSE, xy = TRUE)) 
#view(p50_2020_random)
#hist(p50_2020_random$pft_agb_deciduousshrub_p50_2020_wgs84)
gdal_polygonizeR <- function(inputraster, 
                             outshape,
                             gdalformat = 'ESRI Shapefile',
                             pypath     = NULL,
                             pyexe      = 'python',
                             overwrite  = FALSE,
                             directory  = "") {
  
  inputraster <- paste0(directory, inputraster)
  outshape <- paste0(directory, outshape)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep = '.'))
    if (any(f.exists)) {
      if (overwrite == FALSE) {
        stop(sprintf('File already exists: %s',
                     toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                    sep = '.')[f.exists])), call.=FALSE)
      } else (
        unlink(paste(outshape, c('shp', 'shx', 'dbf'), sep = '.'))
      )
    }
    system("OSGeo4W", input = paste0(sprintf('%1$s "%2$s" "%3$s" -f "%4$s" "%5$s.shp"', pyexe, pypath, inputraster, gdalformat, outshape), " -fieldname id"))
  }
}


poly_gdal <- polygonizer(p50_2020, r)





