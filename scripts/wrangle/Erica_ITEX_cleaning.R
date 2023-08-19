# ITEX cleaning of only data I need (point framing only)

## FUNCTIONS ----
`%notin%` <- Negate(`%in%`)

## DATA ----
load("data/ITEX/pfxy_all.RData")
load("data/ITEX/pfplot_all.RData")
load("data/ITEX/perccov_all.RData")

# Updated QHI data (needs to be replaced in ITEX)
qhi <- read.csv("data/ITEX/qhi-1999-2022-clean-nov22.csv")


#### Point-framing (XY) ####

# There are XY data that only have one coordinate - I'm filling in the NA cell so we avoid problems with cover calculation
na.x <- pfxy_all6 %>% filter(is.na(X)) # all filled in
na.y <- pfxy_all6 %>% filter(is.na(Y)) # 139256 obs

# Replace Y coords that are NA by 0s, create a unique coordinate
pfxy_all7 <- pfxy_all6 %>% tidyr::replace_na(list(Y = "0")) %>% 
  unite(XY, c("X", "Y"), sep = "_", remove = FALSE)

# No point on checking that 1 row = 1 species because multiple entries of the same species per plotXyear

# STEP 1: Convert species abundance to presence/absence 
# (2D, not considering multiple hits of the same species at each xy coord, just 1)
pfxy_all_pa <- pfxy_all7 %>% mutate(ABUNDANCE = ifelse(ABUNDANCE > 1, 1, ABUNDANCE)) %>%
  filter(ABUNDANCE > 0) %>%
  group_by(SiteSubsitePlotYear, XY) %>% distinct(SPECIES_NAME, .keep_all = TRUE) 

#pfxy_all_pa <- pfxy_all7 %>% mutate(ABUNDANCE = ifelse(ABUNDANCE > 1, 1, ABUNDANCE)) %>%
# filter(ABUNDANCE > 0)

# STEP 2: Calculate unique species hits per plot and total unique species hits per plot
pfxy_all_pa2 <- pfxy_all_pa %>% group_by(SiteSubsitePlotYear) %>%
  dplyr::mutate(UniqueSpHitsPlot = n()) %>%
  distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>%
  ungroup() %>% select(., -c(X, Y, XY, HIT)) %>% 
  group_by(SiteSubsitePlotYear) %>%
  mutate(TotalUniqueSpHitsPlot = sum(UniqueSpHitsPlot)) %>% 
  ungroup()

#pfxy_all_pa2 <- pfxy_all_pa %>% group_by(SiteSubsitePlotYear) %>%
# mutate(UniqueSpHitsPlot = n()) %>%
# distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>%
# ungroup() %>% select(., -c(X, Y, XY, HIT)) %>% group_by(SiteSubsitePlotYear) %>%
#mutate(TotalUniqueSpHitsPlot = sum(UniqueSpHitsPlot)) %>% ungroup()

glimpse(pfxy_all_pa2)
#pfxy_all_pa2 <- pfxy_all_pa %>% 
#  filter(SPECIES_NAME == "Salix pulchra")%>% 
# filter(SITE %in% c("TOOLIK", "QHI"))%>% 
#group_by(SiteSubsitePlotYear, SPECIES_NAME) %>% 
# dplyr::mutate(UniqueSpHitsPlot = n()) %>% 
# distinct(SiteSubsitePlotYear, SPECIES_NAME, .keep_all = TRUE) %>% 
#ungroup() %>% 
#select(., -c(X, Y, XY, HIT)) %>% 
# group_by(SiteSubsitePlotYear) %>%
# dplyr::mutate(TotalUniqueSpHitsPlot = sum(UniqueSpHitsPlot)) %>% ungroup()

# STEP 3: Calculate cover per species
pfxy_all_cov <- pfxy_all_pa2 %>% mutate(RelCover = (UniqueSpHitsPlot/TotalUniqueSpHitsPlot)*100)%>% 
  filter(SPECIES_NAME == "Salix pulchra") %>%  
  filter(SITE %in% c("QHI", "TOOLIK"))

unique(pfxy_all_cov$SPECIES_NAME)
view(pfxy_all_cov)

# Confirm that total cover values add up to 100 in every plotXyear
pfxy.check <- pfxy_all_cov %>% group_by(SiteSubsitePlotYear) %>% 
  mutate(TotalCover = sum(RelCover)) %>% 
  distinct(SiteSubsitePlotYear, .keep_all = TRUE) %>% 
  filter(SPECIES_NAME == "Salix pulchra") %>%  
  filter(SITE %in% c("QHI", "TOOLIK"))


view(pfxy.check)

# calculate mean per year
pfxy.check_mean <- ddply(pfxy.check,.(YEAR, SiteSubsite), summarise,
                         mean_cov = mean(RelCover))

pfxy.check_max <- ddply(pfxy.check,.(YEAR, SiteSubsite), summarise,
                        max_cov = max(RelCover))

view(pfxy.check_mean)

# quick plot
(plot <- ggplot(pfxy.check_mean) +
    geom_point(aes(x =YEAR , y = mean_cov, color= SiteSubsite, fill =SiteSubsite))+
    geom_line(aes(x =YEAR , y = mean_cov, color= SiteSubsite, fill =SiteSubsite)))


write.csv(pfxy.check_mean, "data/ITEX/pfxy.check_mean.csv")
write.csv(pfxy.check_max, "data/ITEX/pfxy.check_max.csv")

# Updated metadata file November 2022
metadata0 <- read.csv("data/ITEX/TVC_SITE_SUBSITE_UPDATED2022.csv")

# Keep only relevant columns
metadata <- metadata0 %>% unite(SiteSubsite, c("SITE", "SUBSITE"), sep = ":", remove = FALSE) %>% 
  select(SiteSubsite, MOISTURE, LAT, LONG, ELEV, AZONE, SurveyedArea)

# Merge with composition data
itex.meta.mean <- left_join(pfxy.check_mean, metadata, by = "SiteSubsite")
itex.meta.max <- left_join(pfxy.check_max, metadata, by = "SiteSubsite")

itex.meta.mean <-itex.meta.mean %>% filter(SurveyedArea >= 1)
itex.meta.max <-itex.meta.max %>% filter(SurveyedArea >= 1)

# quick plot
(plot <- ggplot(itex.meta.mean) +
    geom_point(aes(x =YEAR , y = mean_cov, color= SiteSubsite, fill =SiteSubsite))+
    geom_line(aes(x =YEAR , y = mean_cov, color= SiteSubsite, fill =SiteSubsite)))

(plot <- ggplot(itex.meta.max) +
    geom_point(aes(x =YEAR , y = max_cov, color= SiteSubsite, fill =SiteSubsite))+
    geom_line(aes(x =YEAR , y = max_cov, color= SiteSubsite, fill =SiteSubsite)))

write.csv(itex.meta.mean, "data/ITEX/itex.meta.mean.csv")
write.csv(itex.meta.max, "data/ITEX/itex.meta.max.csv")
