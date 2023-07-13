# CHELSA CMPI5 explore script
# Created on 06/03/2023 by Erica

# libraries -----
library(ncdf4)
library(raster)
library(tidyr)
library(magrittr)

# setting working directory to a hard drive where data stored
setwd("/Volumes/Kluane_22/CHELSA_CMIP5")

# download data (from hard drive)
CMPI5_2006_29 <- nc_open("CHELSAcmip5ts_tasmax_ACCESS1-3_rcp85_2006-2029_V1.1.nc") 
CMPI5_2006_29 # explore info

# start
# subsetting to area of interest (Lat lon from Katies map)
LonIdx <- which(CMPI5_2006_29$dim$lon$vals < -135.29 & CMPI5_2006_29$dim$lon$vals >= -149.9) #n=408 long
LatIdx <- which(CMPI5_2006_29$dim$lat$vals <= 70.1 & CMPI5_2006_29$dim$lat$vals > 64.8 ) #n=240; 240*408=97920
timex <- which(CMPI5_2006_29$dim$time$vals >= "2006-1-01")
unique(timex)
# CMPI5_2006_29$dim$time$vals <- as.POSIXct(CMPI5_2006_29$dim$time$vals, format="%Y-%m-%d", origin = "2006-1-01 00:00:00" )
time_d <- as.Date(time, format="%Y-%m-%d", origin = "2006-1-01 00:00:00")

z <- ncvar_get(nc = CMPI5_2006_29,
               varid = CMPI5_2006_29$var$air_temperature,
               start = c(LonIdx[1],
                         LatIdx[1], 
                         timex[1]),
               count = c(length(LonIdx),
                         length(LatIdx), 
                         timex[1]),
               verbose = T)

lon <- CMPI5_2006_29$dim$lon$vals[LonIdx]
lat <- CMPI5_2006_29$dim$lat$vals[LatIdx]
time <- CMPI5_2006_29$dim$time$vals[timex]
rownames(z) <- as.character(lon)
colnames(z) <- as.character(lat)

ztbl <- as_tibble(z, rownames = "lon")
dat <- ztbl %>% pivot_longer(-lon, names_to = "lat", values_to = "air_temp_K")  %>%
  mutate(air_temp_C = air_temp_K - 273.15)

datafinal <- merge(time_d, dat)
write.csv(datafinal, "/Volumes/Kluane_22/CHELSA_CMIP5/CMPI5_extract_2029.csv")


# create dataframe
# finish ----

library(dplyr)

CMPI5_2006_29_dat <- CMPI5_2006_29$var[[1]]
data1 <- ncvar_get( CMPI5_2006_29, CMPI5_2006_29_dat) 

data1 <- raster(data1)


# extract variable name, size and dimension
v <- CMPI5_2006_29$var[[1]]
size <- v$varsize
dims <- v$ndims
nt <- size[dims]              # length of time dimension
lat <- CMPI5_2006_29$dim$latitude$vals   # latitude position
lon <- CMPI5_2006_29$dim$longitude$vals  # longitude position

# read sst variable
r<-list()

for (i in 1:nt) {
  start <- rep(1,dims)     # begin with start=(1,1,...,1)
  start[dims] <- i             # change to start=(1,1,...,i) to read    timestep i
  count <- size                # begin with count=(nx,ny,...,nt), reads entire var
  count[dims] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
  
  dt<-ncvar_get(CMPI5_2006_29, varid = 'air_temperature', start = start, count = count)
  
  # convert to raster
  r[i]<-raster(dt)
}

r<-stack(r)

# transpose the raster to have correct orientation
rt<-t(r)
extent(rt)<-extent(c(range(lon), range(lat)))


# extracting lat lon
lon <- ncvar_get(CMPI5_2006_29, varid = "longitude")
lat <- ncvar_get(CMPI5_2006_29, varid = "latitude")
tas <- ncvar_get(CMPI5_2006_29, varid = "air_temperature")

summary(lon)
#  Min.    1st Qu.     Median       Mean    3rd Qu. 
#-180.01688  -90.03088   -0.04489   -0.04489   89.94110 
#Max. 
#179.92710 

summary(lat) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-90.018 -46.530  -3.041  -3.041  40.448  83.937 

# exploring time
CMPI5_2006_29$dim$time$units #  "days since 2006-1-15 00:00:00"
CMPI5_2006_29$dim$time$calendar # "standard"

tas <- ncvar_get(CMPI5_2006_29, "air_temperature")


# exploring resolution 
res(CMPI5_2006_29) # resolution 

# exploring projection
projection(CMPI5_2006_29)

# crop to the extent of Katies maps

## CROPPING -----
# Cropping shrub raster to the PCH range 
crop_1 <- crop(CMPI5_2006_29, extent(p50_1985))
crop_CMPI5_2006_29 <- mask(crop_1, p50_1985)
plot(crop_CMPI5_2006_29) # plot cropped raster
