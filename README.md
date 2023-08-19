## Masters by Research repository by Erica Zaja
### GeoSciences Individual Project (2022-2023)
#### Supervised by Prof Isla Myers-Smith and Dr Mariana García Criado (Team Shrub)
#### Last update: 12/07/2023
#### Research title: Projecting Arctic wildlife habitat change with warming using natural and experimental shrub growth scenarios

*******

### Repository structure:

### Data   
- #### [data](https://github.com/ericazaja/MSc_ZAJA_2022/tree/main/data)
    - [allometry](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/allometry) including: (a) Isla's PhD data from Pika Camp (biomass harvests and heights); (b) Andy's paper data including biomass harvests and heights from QHI (c) [long-term point framing plots from Hershel Vegetation type in QHI](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/data/allometry/qhi-1999-2022-clean-nov22.csv)
     - [climate_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/climate_data): all extracted climate data from CHELSA.
     - [CMPI6](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/CMPI6): all files of CMPI6 global circulation models of estimated air temperature under SSP 8.5.
    - [common_garden_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/common_garden_shrub_data): including all necessary datasets for CG analyses. Also including start, middle and end of season (2022) and wrangled full dataset (2022) and weekly subsets (2022).
    - [source_pop_Kluane_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/source_pop_Kluane_shrub_data): 2022 subsets from Kluane Plateau
    - [source_pop_QHI_shrub_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/source_pop_QHI_shrub_data): 2022 subsets from QHI
    - [tomst](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/tomst): common garden, KP and QHI tomst 2022.
     - [hobo](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/hobo): common garden HOBO records.
    - [phenology](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/phenology): common garden phenology groundtruthing weekly 2022.
     - [ITEX](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/ITEX): all ITEX data used in natural populations growth analyses.
     - [maps_data](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/data/maps_data): extrcted data from Orndahl et al's map (2020) including 2020 tiff file.
    - Other datasets required which cannot be uploaded (too large): Katie Orndahl's 2022 raster maps and border shapefiles, CMPI6 GCMs (.nc files). Please contact Erica Zaja if required.

### Scripts   
- #### [bayesian scripts](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/bayesian)
  - [bayes_allometry.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_allometry.R): script to make allometric equations of 3 target speices. - FINAL
  - [bayes_CG_cover_biomass.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_cover_biomass.R): script to calculate % cover of shrubs in the garden and convert to biomass using allometric equations.
    - [bayes_CG_growth.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_growth.R): script to model max height of commmon garden shrubs. 
   - [bayes_CG_height_elong.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_height_elong.R): script to model height over time of commmon garden shrubs. 
    - [bayes_CG_pheno.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_CG_pheno.R): script to model phenology of commmon garden shrubs. 
    - [bayes_heights_over_time_IMS.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_heights_over_time_IMS.R): modelling QHI shrub max and mean heights over time (long term monitoring plots)
    - [bayes_ITEX_cover.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_ITEX_cover.R): explorative analysis of ITEX cover over time
    - [calc_script](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/calc_script.R): script with all final calculations, encompassing statistics from bayesian modelling and mapping, to estimate biomass in the future.
     - [bayes_temp_change.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/bayesian/bayes_temp_change.R): modelling temperature over time at different sites (CG, KP, QHI, TOOLIK, ANWR)
  
- #### [maps](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/maps)
  - [raster_starter.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/raster_starter.R): script to read and modify Orndahl et al 2022 maps.
  - [avg_natural_growth_warm.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/agv_natural_growth_warm.R): script to project shrub biomass to 2100 using mean natural shrub growth rate scenario.
  - [max_natural_growth_warm.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/max_natural_growth_warm.R): script to project shrub biomass to 2100 using max natural shrub growth rate scenario.
  - [novel_growth_warm.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/novel_growth_warm.R): script to project shrub biomass to 2100 using novel (common garden) shrub growth rate scenario.
  - [base_map.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/base_map.R): script to make map of North America and overlay shrub biomass rasters.
  - [base_2020_map.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/warming_project/base_2020_map.R): script to read and extract information from Orndahl et al 2022 latest shrub biomass map (2020)
  - [Climate_extract.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/climate/Climate_extract.R): script to extract and stack temperature rasters from global climate models.
  - [Climate_models.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/climate/Climate_models.R): script relating climate in 2020 and biomass in 2020 in the study area, and analysing future temperature increases.
  - [CMPI6_tut.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/maps/climate/CMPI6_tut.R): script to download and extract usable data from global climate models. Using instructions from tutorial by Michael Minn: [here](https://michaelminn.net/tutorials/r-climate/index.html)


- #### [wrangle](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/wrangle)
    - [ITEX_cleaning_dec2022.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/wrangle/ITEX_cleaning_dec2022.R): cleaning script by Mariana updated in december 2022.
    - [Erica_ITEX_cleaning.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/wrangle/Erica_ITEX_cleaning.R): itex data cleaning of the relevant data for my MSc
    
- #### [TOMST](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/TOMST) 
   - scripts to download and wrangle TOMST logger data from QHI, Kluane Plateau and Common Garden (CG) used to make informative tables.
    
- #### [extra_scripts](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/scripts/extra_scripts): these are exploratory/data wrangling/extra analyses scripts not directly used in final thesis. Including: 
   - [bayes_CG_elong_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/bayesian/bayes_CG_elong_temp.R): script to model annual stem elongation as a function of annual temperature. INFORMATIVE analysis, not used in the allom. equations NOR IN FINAL THESIS. Just to see if we see more growth in warmer years.
     - [bayes_cover_vs_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/bayesian/bayes_Cover_vs_temp.R): Exploratory script showing mean cover change - temperature relationships. Cover vs mean july temperature at CG, QHI, KP, ANWR, TOOLIK. 
    - [bayesian_stem_elong_vs_temp.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/bayesian/bayes_stem_elong_vs_temp.R): Stem elongation - temperature relationships. Mean stem elong vs mean july temperature at CG, QHI, KP, ANWR, TOOLIK.
    - [carbon.R](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/blob/main/scripts/extra_scripts/carbon.R): script I will use to convert shrubs growth estimates to carbon content.
 
### Outputs
- [outputs](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs)
   - [models](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs/models): saved model outputs (as rds files) to easily import 
   - [figures](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs/figures): all saved figures in different formats (pdf, png etc)
   - [tables](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs/tables):saved model output tables (using kable)
   - [final_maps](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs/final_maps): final scenarios maps (shrub biomass + threshold maps)
   - [CMPI6_rasters](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs/CMPI6_rasters): climate rasters of multiple years
   - [CMPI6_pdf](https://github.com/ericazaja/NEW_MSC_ZAJA_2022/tree/main/outputs/CMPI6_pdf): climate rasters of 2100 in pdf format

*******

#### Requirements
- `RStudio` version 1.2.5001 or greater

#### Learning objectives
- Working with large datasets 
- Learning new statistical methods (Bayesian)
- Learning mapping and GIS techniques in R
- Writing a manuscript
- Writing a literature review 

#### Feedback Etiquette

- Please use either an issue or a pull request.
- Please use "###" and your initial before your feedback comments.
- Please feel free to comment on anything at all! 
- I'm happy to answer any questions you might have about my work.

*******

#### Acknowledgements
I thank the data providers, my supervisors Prof Isla Myers Smith and Dr Mariana García Criado, and Team Shrub members for their support. Thank you Kathleen Orndahl for the data she shared with me, which is the baseline of all my work.

