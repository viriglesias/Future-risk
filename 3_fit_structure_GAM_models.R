# =============================================================================
# Script: fit_structure_GAM_models.R
# Purpose:
#   Fit generalized additive models (GAMs) to annual structure counts on
#   private land within each Level IV ecoregion. The models describe temporal
#   trends in structures from 1900–2020 and generate predictions through 2060.
#
#   Two model families are fit for comparison:
#       1. Negative binomial GAM
#       2. Poisson GAM
#
#   The script performs model fitting, prediction, and validation for each
#   ecoregion time series produced in the previous step.
#
# Inputs required:
#   1. Data/Processed/structures_ts/
#      - Directory produced by the previous script.
#      - Contains one CSV per Level IV ecoregion with annual structure totals
#        by ownership class.
#
#   2. CSV structure format expected:
#        year
#        structures
#        dlu            (land ownership class)
#        eco_l4_id
#        eco_l1
#
# Outputs produced:
#   1. Data/Processed/structures/Model/predictions/
#      - Predicted structure counts for each ecoregion from 1900–2060,
#        including model fits and standard errors for both model families.
#
#   2. Data/Processed/structures/Model/summaries/
#      - Model evaluation metrics for each ecoregion, including:
#           AIC
#           BIC
#           deviance explained
#           validation deviance explained
#           validation correlation
#
# Modeling approach:
#   - Temporal trends are modeled using a cubic regression spline:
#
#       structures ~ s(year, bs = "cr", k = 12)
#
#   - Models are fit using mgcv::gam().
#
#   - Model fitting uses a subset of years (yrs_fit) for training and
#     reserves remaining years for validation.
#
# Notes:
#   - Modeling is restricted to private land only.
#   - Ecoregions with no private land observations are flagged and skipped.
#   - If an ecoregion has zero structures in 2020, all predicted values are
#     set to zero to avoid unstable model fits.
# =============================================================================

