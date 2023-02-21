# NEW repository
## Masters by Research repository by Erica Zaja
### GeoSciences Individual Project (2022-2023)
#### Supervised by Prof Isla Myers-Smith and Dr Mariana García Criado (Team Shrub)
#### Last update: 09/11/2022
*******

#### Research title: Experimental evidence of shrub encroachment and influences on wildlife habitat

####### Understanding climate change-driven vegetation shifts through a common garden experiment in the Yukon Territory: implications for Porcupine Caribou Herd summer range habitat 

 
#### Repository structure

- #### [data](https://github.com/ericazaja/MSc_ZAJA_2022/tree/main/data)
    - [allometry](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/allometry) including: (a) Isla's PhD data from Pika Camp (biomass harvests and heights); (b) Andy's paper data including biomass harvests and heights from QHI; (c) [carbon content](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/allometry/carbon) (%); (d) Berner 2015 height and biomass harvest data; (e) [long-term point framing plots from Hershel Vegetation type in QHI](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/data/allometry/qhi-1999-2022-clean-nov22.csv)
    - [common_garden_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/common_garden_shrub_data) including start, middle and end of season (2022) and wrangled full dataset (2022) and weekly subsets (2022).
    - [katie_maps](not uploaded yet)
    - [source_pop_Kluane_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/source_pop_Kluane_shrub_data): 2022 subsets from Kluane Plateau
    - [source_pop_QHI_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/source_pop_QHI_shrub_data): 2022 subsets from QHI
    - [tomst](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/tomst): common garden, KP and QHI tomst 2022.
    - [phenology](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/phenology): common garden phenology groundtruthing weekly 2022.
    
    
- #### [scripts](https://github.com/ericazaja/MSc_ZAJA_2022/tree/main/scripts)
  - [allometry.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/allometry.R): script to make allometric equations of 3 target speices.
  - [CG_cover_biomass.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/CG_cover_biomass.R): script to calculate % cover of shrubs in the garden and convert to biomass using allometric equations.
    - [CG_elong_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/CG_elong_temp.R): script to model annual stem elongation as a function of annual temperature. INFORMATIVE analysis, not used in the allom. equations. Just to see if we see more growth in warmer years.
    - [Cover_vs_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/Cover_vs_temp.R): Mean cover change - temperature relationships. Cover vs mean july temperature at CG, QHI, KP, ANWR, TOOLIK.
    - [ITEX_cleaning_dec2022.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/ITEX_cleaning_dec2022.R): cleaning script by Mariana updated in december 2022.
    - [ITEX_cover.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/ITEX_cover.R): explorative analysis of ITEX cover over time
    - [stem_elong_vs_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/stem_elong_vs_temp.R): Stem elongation - temperature relationships. Mean stem elong vs mean july temperature at CG, QHI, KP, ANWR, TOOLIK.
    - [carbon.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/carbon.R): script I will use to convert shrubs growth estimates to carbon content.
    - [raster_starter.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/raster_starter.R): script to read and modify Katie's maps
    
- #### [img]()
- #### [preregistration]()

*******

#### Requirements
- `RStudio` version 1.2.5001 or greater
- packages `ggplot2`...

#### Learning objectives
- Working with large datasets
- Learning new statistical methods
- Writing a manuscript
- Writing a literature review 

#### Feedback Etiquette

- Please use either an issue or a pull request.
- Please use "###" and your initial before your feedback comments.
- Please feel free to comment on anything at all! 
- I'm happy to answer any questions you might have about my work.

*******

#### Acknowledgements
I thank the data providers, my supervisors Dr Isla Myers Smith and Dr Mariana García Criado, and Team Shrub members for their support. 

