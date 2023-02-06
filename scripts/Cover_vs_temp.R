# Cover vs mean july temperature
#### Script by Erica Zaja, created 01/02/23
### Last updated: 01/02/23


# Loading chelsa temperature and precipitation data -----
july_enviro_chelsa <- read_csv("data/july_enviro_chelsa.csv")


# DATA WRANGLE ------
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


