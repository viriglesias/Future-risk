# =============================================================================
# Script: select_final_structure_models.R
# Purpose:
#   Select a final temporal model for each Level IV ecoregion from the GAM
#   outputs generated previously, identify and correct extreme forecast
#   outliers, attach regional metadata, calculate 95% confidence intervals,
#   and export a complete table of structure projections.
#
#   The script starts from model summary files and prediction files, chooses
#   either the negative binomial or Poisson model for each ecoregion based
#   primarily on AIC, then applies an additional outlier screen to remove
#   implausibly large projected growth. The final product is a unified table
#   of annual observed and projected structure counts with uncertainty bounds.
#
# Inputs required:
#   1. Data/Processed/structures/Model/summaries/
#      - Model summary CSVs produced by the GAM fitting script.
#      - Must contain, at minimum:
#          aic_nb
#          aic_poiss
#
#   2. Data/Processed/structures/Model/predictions/
#      - Prediction CSVs produced by the GAM fitting script.
#      - Must contain, at minimum:
#          year
#          struct_model
#          struct_valid
#          fit_nb.x
#          se_nb.x
#          fit_poiss
#          se_poiss
#
#   3. Data/Raw/us_eco_l4_state_boundaries
#      - Vector layer of U.S. Level IV ecoregion boundaries.
#      - Used to attach Level I / Level IV identifiers and assign broad
#        regional classes (West, East, Central).
#
# Outputs produced:
#   1. Data/Processed/structures/Model/structures_complete.csv
#      - Final table of observed and projected structure counts by ecoregion
#        and year, including selected model output and 95% confidence bounds.
#
# Main processing steps:
#   1. Read model summaries and predictions.
#   2. Select the lower-AIC model for each ecoregion.
#   3. Identify forecast outliers where projected 2050 values exceed
#      three times the 2020 value.
#   4. For those outliers, switch to the alternate model.
#   5. Re-screen forecasts and remove remaining outlier ecoregions.
#   6. Attach ecoregion metadata and broad regional labels.
#   7. Compute upper and lower 95% confidence bounds.
#   8. Export the final complete table.
# =============================================================================
