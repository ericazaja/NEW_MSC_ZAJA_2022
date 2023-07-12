## Masters by Research repository by Erica Zaja
### GeoSciences Individual Project (2022-2023)
#### Supervised by Prof Isla Myers-Smith and Dr Mariana García Criado (Team Shrub)
#### Last update: 12/07/2023
*******

#### Research title: Projecting Arctic wildlife habitat change with warming using natural and experimental shrub growth scenarios

 
#### Repository structure

- #### [data](https://github.com/ericazaja/MSc_ZAJA_2022/tree/main/data)
    - [allometry](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/allometry) including: (a) Isla's PhD data from Pika Camp (biomass harvests and heights); (b) Andy's paper data including biomass harvests and heights from QHI; (c) [carbon content](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/allometry/carbon) (%); (d) Berner 2015 height and biomass harvest data; (e) [long-term point framing plots from Hershel Vegetation type in QHI](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/data/allometry/qhi-1999-2022-clean-nov22.csv)
    - [common_garden_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/common_garden_shrub_data) including start, middle and end of season (2022) and wrangled full dataset (2022) and weekly subsets (2022).
    - [katie_maps](not uploaded yet)
    - [source_pop_Kluane_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/source_pop_Kluane_shrub_data): 2022 subsets from Kluane Plateau
    - [source_pop_QHI_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/source_pop_QHI_shrub_data): 2022 subsets from QHI
    - [tomst](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/tomst): common garden, KP and QHI tomst 2022.
    - [phenology](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/phenology): common garden phenology groundtruthing weekly 2022.

    
    
- #### [bayesian scripts](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/bayesian)
  - [bayes_allometry.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_allometry.R): script to make allometric equations of 3 target speices. - FINAL
  - [bayes_CG_cover_biomass.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_cover_biomass.R): script to calculate % cover of shrubs in the garden and convert to biomass using allometric equations.
    - [bayes_CG_growth.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_growth.R): script to model max height of commmon garden shrubs. 
   - [bayes_CG_height_elong.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_height_elong.R): script to model height over time of commmon garden shrubs. 
    - [bayes_CG_pheno.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_pheno.R): script to model phenology of commmon garden shrubs. 
    - [bayes_heights_over_time_IMS.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_heights_over_time_IMS.R): modelling QHI shrub max and mean heights over time (long term monitoring plots)
    - [bayes_ITEX_cover.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_ITEX_cover.R): explorative analysis of ITEX cover over time
    - [calc_script](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/calc_script.R): script with all final calculations, encompassing statistics from bayesian modelling and mapping, to estimate biomass in the future.
  
- #### [maps](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/maps)
  - [raster_starter.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/raster_starter.R): script to read and modify Orndhal et al 2022 maps.
  - [avg_natural_growth_warm.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/agv_natural_growth_warm.R): script to project shrub biomass to 2100 using mean natural shrub growth rate scenario.
  - [max_natural_growth_warm.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/max_natural_growth_warm.R): script to project shrub biomass to 2100 using max natural shrub growth rate scenario.
  - [novel_growth_warm.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/novel_growth_warm.R): script to project shrub biomass to 2100 using novel (common garden) shrub growth rate scenario.
  - [base_map.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/base_map.R): script to make map of North America and overlay shrub biomass rasters.
  - [base_2020_map.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/base_2020_map.R): script to read and extract information from Orndhal et al 2022 latest shrub biomass map (2020)
  - [Climate_extract.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/climate/Climate_extract.R): script to extract and stack temperature rasters from global climate models.
  - [Climate_models.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/climate/Climate_models.R): script relating climate in 2020 and biomass in 2020 in the study area, and analysing future temperature increases.
  - [CMPI6_tut.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/climate/CMPI6_tut.R): script to download and extract usable data from global climate models. Using instructions from tutorial by Michael Minn: [here](https://michaelminn.net/tutorials/r-climate/index.html)


- #### [wrangle](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/wrangle)
    - [ITEX_cleaning_dec2022.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/wrangle/ITEX_cleaning_dec2022.R): cleaning script by Mariana updated in december 2022.
    - [Erica_ITEX_cleaning.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/wrangle/Erica_ITEX_cleaning.R): itex data cleaning of the relevant data for my MSc
    
    
    
- #### [extra_scripts](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/extra_scripts): these are exploratory/data wrangling/extra analyses scripts not directly used in final thesis. Including: 
   - [bayes_CG_elong_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/bayesian/bayes_CG_elong_temp.R): script to model annual stem elongation as a function of annual temperature. INFORMATIVE analysis, not used in the allom. equations NOR IN FINAL THESIS. Just to see if we see more growth in warmer years.
     - [bayes_cover_vs_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/bayesian/bayes_Cover_vs_temp.R): Exploratory script showing mean cover change - temperature relationships. Cover vs mean july temperature at CG, QHI, KP, ANWR, TOOLIK. 
    - [bayesian_stem_elong_vs_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/bayesian/bayes_stem_elong_vs_temp.R): Stem elongation - temperature relationships. Mean stem elong vs mean july temperature at CG, QHI, KP, ANWR, TOOLIK.
    - [carbon.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/carbon.R): script I will use to convert shrubs growth estimates to carbon content.
 
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

