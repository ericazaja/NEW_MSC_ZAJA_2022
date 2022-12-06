# Carbon content 
### Script by Erica Zaja
### Last updated: 06/12/2022

# 1. LOADING DATA ----

# carbon ratios in the tundra trait database
load("~/Desktop/Github_repos.tmp/NEW_MSC_ZAJA_2022/data/allometry/carbon/try_ttt.RData")

# 1. DATA EXPLORING ----
str(try.ttt)
unique(try.ttt$TraitShort)
unique(try.ttt$AccSpeciesName)
