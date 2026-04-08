This repository contains the analysis code used in the manuscript:  Fire risk to structures nearly triples by mid-century under current trajectories, by Iglesias et al. (in review).

The workflow integrates historical fire observations, projected fire activity under climate models, modeled structure distributions, and empirical structure-loss relationships to estimate future structure exposure and destruction risk under current trajectories.

Data sources
  All data used in this study are publicly available:

- Fire data: Monitoring Trends in Burn Severity (MTBS);
  DOI: [https://doi.org/10.5066/P9IED7RZ](https://doi.org/10.5066/P9IED7RZ)

- Structure data: HISDAC-US (Historical Settlement Data Compilation for the United States);
  DOI: [https://doi.org/10.7910/DVN/45B8IU](https://doi.org/10.7910/DVN/45B8IU)

- Population projections: ICLUS v2 (Integrated Climate and Land Use Scenarios);
  EPA/600/R-16/366F

- Fire structure loss records: ICS-209 Plus;
  DOI: 10.6084/m9.figshare.19858927.v3

Workflow and script order

The scripts are intended to be run as a staged pipeline. Several later steps depend directly on outputs created earlier, so the order matters.

1. Build the exclusion layer
Script: 1_build_exclusion_layers.R
This script creates the national 250 m exclusion raster for the conterminous United States. It combines PAD-US ownership classes with major lakes and reservoirs and produces the base exclusion layer used to separate private land from other ownership and water classes.
Outputs:
Data/Processed/protected_all_classes.tif
Data/Processed/exclude.tif

2. Summarize structures by ownership class and ecoregion
Script: 2_summarize_BUPR_by_ownership_and_ecoregion.R
This script overlays annual structure rasters with the exclusion raster from Step 1 and summarizes structure counts by ownership class within each Level IV ecoregion. In practice, the downstream modeling focuses on private land structure counts extracted from these summaries.
Input from previous step:
Data/Processed/exclude.tif
Outputs:
Data/Processed/structures_ts/*.csv

3. Fit temporal structure models
Script: 3_fit_structure_GAM_models.R
This script fits temporal GAMs to private-land structure counts for each Level IV ecoregion. It compares Poisson and negative binomial formulations, evaluates held-out validation years, and generates annual predictions through 2060.
Input from previous steps:
Data/Processed/structures_ts/*.csv
Outputs:
Data/Processed/structures/Model/predictions/*.csv
Data/Processed/structures/Model/summaries/*.csv

4. Select final structure models
Script: 4_select_final_structure_models.R
This script chooses the final model for each ecoregion from the candidate GAM outputs, screens out implausible forecasts, attaches regional metadata, and computes uncertainty bounds. The result is the master table of historical and projected structure counts.
Inputs from previous steps:
Data/Processed/structures/Model/predictions/*.csv
Data/Processed/structures/Model/summaries/*.csv
Output:
Data/Processed/structures/Model/structures_complete.csv

5. Validate final structure models
Script: 5_validate_final_structure_models.R
This script evaluates the selected structure models using adjusted deviance-explained metrics for both training and validation subsets. It is a validation step rather than a dependency for later scripts, but it should be run once the final structure model table exists.
Inputs from previous steps:
Data/Processed/structures/Model/structures_complete.csv
Data/Processed/structures/Model/summaries/*.csv
Output:
Data/Processed/structures/Model/deviance_validation.csv

6. Summarize population by ecoregion
Script: 6_summarize_population_by_ecoregion.R
This script rasterizes ICLUS population polygons to the 250 m grid and summarizes population totals within Level IV ecoregions. It supports demographic context and interpretation of projected exposure patterns.
Output:
Data/Processed/Population_summaries.csv
This step is analytically useful but not required for the fire-risk pipeline below unless population summaries are being used in additional figures or interpretation.

7. Summarize historical area burned by ecoregion
Script: 7_summarize_mtbs_by_ecoregion.R
This script assigns MTBS events to Level IV and Level III ecoregions using ignition points, computes annual burned area, and calculates the proportion of each ecoregion burned each year.
Outputs:
Data/Processed/mtbs_eco.csv
Data/Processed/mtbs_l4.csv
Data/Processed/mtbs_l3.csv

8. Summarize projected burned area by ecoregion
Script: 8_summarize_fire_projections_by_ecoregion.R
This script processes climate-model fire projections for each selected model, converts projected burned area to hectares, and computes the annual proportion of each Level IV ecoregion projected to burn.
Run this script once for each climate model used in the study.
Outputs:
Data/Processed/Fire_projections/Fire_projections_mtbs_<model>.csv
Data/Processed/Fire_projections/Fire_projections_<model>_l4.csv

9. Combine historical and projected burned area
Script: 9_combine_mtbs_and_fire_projections_l4.R
This script merges historical MTBS burned-area data with projected burned-area estimates for each climate model at the Level IV ecoregion scale. It standardizes the fields and creates a combined fire table for later risk calculations.
Inputs from previous steps:
Data/Processed/mtbs_l4.csv
Data/Processed/Fire_projections/Fire_projections_CNRM_l4.csv
Data/Processed/Fire_projections/Fire_projections_MRI_l4.csv
Data/Processed/Fire_projections/Fire_projections_HadGEM2-ES_l4.csv
Outputs:
Data/Processed/Fire_projections/Fire_combo_CNRM_l4.csv
Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv
Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv

10. Summarize ICS-209 structure losses by ecoregion
Script: 10_summarize_ics209_structures_by_ecoregion.R
This script assigns ICS-209-PLUS incidents to Level III and Level IV ecoregions, filters and cleans incident records, and aggregates annual totals of structures damaged and destroyed.
Outputs:
Data/Processed/ics_l3-_l4_full.csv
Data/Processed/ics_l3_1.csv
Data/Processed/ics_l4_1.csv

11. Create historical/interpolated structure summaries for ecoregions
Script: 11_summarize_zillow_structures_by_ecoregion.R
This script summarizes annual structure counts, occupied area, and structure density at Level III and Level IV ecoregion scales and interpolates missing years. These outputs support the empirical fire-loss calibration and descriptive comparisons.
Outputs:
Data/Processed/zillow_l3_2.csv
Data/Processed/zillow_l4_2.csv

12. Estimate empirical damage ratios and uncertainty
Script: 12_estimate_structure_damage_ratio_and_uncertainty.R
This script combines burned area, modeled structures, and ICS-209 structure-loss records to estimate the ratio of structures damaged or destroyed per unit burned area and exposed structure stock. It also bootstraps uncertainty around those ratios.
Inputs from previous steps:
Data/Processed/Fire_projections/fire_full.csv
(or an equivalent combined fire table prepared upstream)
Data/Processed/structures/Model/structures_complete.csv
Data/Processed/ics_l3_1.csv
Outputs:
Data/Processed/damage_ratio.csv
Data/Processed/damage_ratio_uncert.csv
Data/Processed/sigma_loss.csv

13. Estimate projected structure-loss risk
Script: 13_estimate_projected_structure_loss_risk.R
This script combines projected burned area, modeled structure counts, and empirical damage ratios to estimate exposed and destroyed structures through time across ecoregions. This is the central risk-integration step.
Inputs from previous steps:
Data/Processed/Fire_projections/fire_full.csv
Data/Processed/structures/Model/structures_complete.csv
Data/Processed/damage_ratio.csv
Main output:
Data/Processed/risk.csv

14. Decompose spatial driver contributions
Script: 14_compute_spatial_driver_contributions_by_ecoregion.R
This script uses the risk table to quantify the relative contribution of three factors to risk across Level III ecoregions: structure exposure, burned-area fraction, and structure-loss intensity.
Input from previous step:
Data/Processed/risk.csv
Output:
Data/Processed/spatial_driver_contributions_by_type_period_ecoregion.csv

15. Vulnerability
Script: 15_vulnerability.R
This script converts ecoregion-scale projected risk to tract-scale estimates by spatially weighting Level III ecoregion risk values across census tract overlaps.
Input from previous step:
Data/Processed/risk.csv
Outputs:
Data/Processed/census_tracts_with_risk.shp
Data/Processed/census_tracts_with_risk.csv
