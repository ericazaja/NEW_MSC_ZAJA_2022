# Carbon content 
### Script by Erica Zaja
### Last updated: 06/12/2022

# 1. LOADING DATA ----

# carbon ratios in the tundra trait database
load("~/Desktop/Github_repos.tmp/NEW_MSC_ZAJA_2022/data/allometry/carbon/try_ttt.RData")

# 2. DATA EXPLORING ----
str(try.ttt)
unique(try.ttt$TraitShort)
unique(try.ttt$AccSpeciesName)

# 3. DATA WRANGLING
try.ttt$AccSpeciesName <- as.factor(try.ttt$AccSpeciesName )

# subsetting to salixes of interest
CN_Salix <- try.ttt %>%
  filter(AccSpeciesName %in% c("Salix arctica", "Salix pulchra")) %>%
  filter(TraitShort == "C.N.ratio")
# CN ratio: a ratio of 20 means that there are 20 g of carbon for each 1 g of nitrogen in that organic matter
# do I keep other traits? LeafN, LeafC ?

