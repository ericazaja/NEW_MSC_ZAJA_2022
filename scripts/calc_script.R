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

# CG TEMP: 
# difference in temp between KP and CG = 6.4 degC
# 6.4/9 years of growth in the CG
# 0.71 degC/year in CG

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
# mean max cover value at year 34 = (47.41 + 28.22)/2= 37.81%

# HEIGHT ------
# MEAN HEIGHT cm
# estimate at year 1 = 4.94 cm
# estimate at year 24 = 12.75 cm

# MAX HEIGHT cm
# estimate at year 1 = 8.10 cm 
# estimate at year 24 = 24.73 cm 

# MEAN AND MAX NATURAL SCENARIOS -----
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
# 2.22075 g/m2 

# Biomass change per degree
2.22075/0.188486
# 11.78204 g/m2/degC for MEAN NATURAL scenario

# MAX BIOMASS at year 1
#Biomass at year 1 = 1.1*height ±  5.0  + (18.1 *cover ± 8.2)
(1.1*8.10) + (18.1 *31.52)
# 579.422

# MAX BIOMASS at year 24
#Biomass at year 24 = 1.1*height ±  5.0  + (18.1 *cover ± 8.2)
(1.1*24.73) + (18.1 *37.81)
# 711.564

# DELTA = MAX BIOMASS at year 24-MAX BIOMASS at year 1
711.564-579.422
#132.142 g/m2

# MAX Biomass per year = DELTA/24
132.142/24
# 5.505917

# Biomass change per degree
5.505917/0.188486
# 29.21128 g/m2/degC for MAX NATURAL scenario


# NOVEL SCENARIO (CG) pulchra only ----
# HEIGHT cm (start at year 2 because that is when cover starts)
# estimate at year 2 =  19.35 cm
# estimate at year 9 =  20.79 cm

# COVER %
# estimate at year 2 = 0.05032221 --> 5.03 %
# estimate at year 9 =  0.22003389 --> 22.0 %

# BIOMASS at year 2
(1.1*19.35) + (18.1 *5.03)
# 112.328 g/m2

# BIOMASS at year 9
(1.1*20.79) + (18.1 *22.0)
# 421.069

# Delta biomass 
421.069-112.328
# 308.741 g/m2

# Divide by 7 years
308.741/7
# 44.10586

# Biomass change per degC change
44.10586/0.71
# 62.12093 g/m2/degC for NOVEL SCENARIO

# NOVEL SCENARIO (CG) pulchra + rich ----
# HEIGHT cm (start at year 2 because that is when cover starts)

# RIC estimate at year 3 =  23.51 cm
# RIC estimate at year 9 =  105.32 cm

# COVER %
# RIC estimate at year 3 = 0.03940055 --> 3.94 %
# RIC estimate at year 9 =  0.67738921 --> 67.74 %


# BIOMASS at year 3
# RICHARDSONII FINAL EQUATION: Biomass =  (18.0*height +- 5.1) + (11.9 *cover +-  18.0)

(18.0*23.51) + (11.9 *3.94)
# 470.066 g/m2

# BIOMASS at year 9
(18.0*105.32) + (11.9 *67.74)
# 2701.866

# Delta biomass 
2701.866-470.066
# 2231.8 g/m2

# Divide by 6 years
2231.8/6
# 371.9667

# Biomass change per degC change
371.9667/0.71
# 523.8968 g/m2/degC for NOVEL SCENARIO

# average of Rich and pul
(523.8968+62.12093)/2
# 293.0089 g/m2/degC
