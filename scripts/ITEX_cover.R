# ITEX shrub cover over time 
#### Script by Erica Zaja, created 30/01/23
### Last updated: 30/01/23


# Workflow:

# Loading data
load("~/Desktop/Github_repos.tmp/NEW_MSC_ZAJA_2022/data/ITEX/ITEX_database.RData")

# DATA WRANGLING ----

# Exploration 
max(ITEX_EZ_diss$YEAR) # latest year: 2020
min(ITEX_EZ_diss$YEAR) # earliest year: 1981
unique(ITEX_EZ_diss$SITE) # Unique site names

# Retaining only QHI, Arctic National Wildlife Refuge (ANWR), Toolik site 
ITEX_veg_msc <- ITEX_EZ_diss %>%
  filter(SITE %in% c("ANWR", "TOOLIK", "QHI")) %>% 
  na.omit()

write.csv(ITEX_veg_msc, "data/ITEX/ITEX_veg_msc.csv")

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

