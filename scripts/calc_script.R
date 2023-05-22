# All the caculations go in this script!

# TEMPERATURES ------
# Temp at year 22 for QHI=8.761188
# Temp at year 0 for QHI=3.539306
# Delta = 8.761188-3.539306=5.221882
# Temp change per year 5.221882 deg C/21 years = 0.248661 deg C/year

#  temp at year 22 for toolik=11.826728
#  temp at year 0 for toolik=9.003889
# delta 11.826728-9.003889 =  2.822839 deg C/year
#  temp change per year = 2.822839/22 = 0.1283109 decC/year

# mean temp change (0.248661+0.1283109)/2 = 0.188486 degC/year in natural populations

# COVER -----

# MEAN COVER 
# actual mean cover value at year 10 (1999) toolik: 0.09632101 --> 9.63 %
# actual mean cover value at year 34 (2022) toolik:0.08991054 --> 8.99%
# actual mean cover value at year 10 qhi:0.14057432 ---> 14.05%
# actual mean cover value at year 34 qhi:0.19638900--> 19.64 %

# mean mean cover value at year 10 = (9.63 + 14.05)/2= 11.84 %
# mean mean cover value at year 34 = (8.99 + 19.64)/2= 14.31%

# MAX COVER
# actual max cover value at year 10 toolik: 0.3605562 --> 36.05%
# actual max cover value at year 34 toolik:0.4741059 --> 47.41 %
# actual max cover value at year 10 qhi:0.2699834 --> 26.99% 
# actual max cover value at year 34 qhi:0.2822647 --> 28.22%

# mean max cover value at year 10 = (36.05 + 26.99)/2= 31.52 %
# mean max cover value at year 34 = (47.41 + 28.22)/2= 37.815%

# HEIGHT ------
# MEAN HEIGHT cm
# estimate at year 1 = 4.94 cm
# estimate at year 24 = 12.75 cm

# MAX HEIGHT cm
# estimate at year 1 = 8.105129 cm 
# estimate at year 24 = 24.729984 cm 

# ALLOMETRIC -----
#Salix pulchra	Shrub AGB = (1.1*height ±  5.0 ) + (18.1 *cover ± 8.2)

# MEAN BIOMASS at year 1
#Biomass at year 1 = 1.1*height ±  5.0  + (18.1 *cover ± 8.2)
(1.1*4.94) + (18.1*11.84) 
# 219.738

# MEAN BIOMASS at year 24
#Biomass at year 24 = 1.1*height ±  5.0  + (18.1 *cover ± 8.2)
(1.1*12.75) + (18.1*14.31) 
# 273.036

# DELTA = MEAN BIOMASS at year 24-MEAN BIOMASS at year 1
273.036-219.738
# 53.298 g/m2

# MEAN Biomass per year =DELTA/24
53.298/24
# 2.22075g/m2 

# Biomass change per degree
2.22075/0.188486
# 11.78204 g/m2/degC for MEAN NATURAL scenario

# MAX BIOMASS at year 1
#Biomass at year 1 = 1.1*height ±  5.0  + (18.1 *cover ± 8.2)

# MAX BIOMASS at year 24
#Biomass at year 24 = 1.1*height ±  5.0  + (18.1 *cover ± 8.2)

# DELTA = MAX BIOMASS at year 24-MAX BIOMASS at year 1
# MAX Biomass per year = DELTA/24

