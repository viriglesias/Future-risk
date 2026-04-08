# =============================================================================
# Script: validate_final_structure_models.R
# Purpose:
#   Evaluate the final selected structure projections by calculating adjusted
#   deviance explained for both the training and validation subsets within each
#   Level IV ecoregion. The script also records which model family
#   (negative binomial or Poisson) was selected for each ecoregion and writes
#   the validation summary to disk.
#
# Inputs required:
#   1. Data/Processed/structures/Model/structures_complete.csv
#      - Final compiled projections table produced by the previous script.
#      - Must contain, at minimum:
#          year
#          region
#          id
#          struct_model
#          struct_valid
#          model_out
#
#   2. Data/Processed/structures/Model/summaries/
#      - Directory containing model summary CSVs from the GAM fitting step.
#      - Used to recover which candidate model had the lower AIC for each
#        ecoregion.
#
# Outputs produced:
#   1. Data/Processed/structures/Model/deviance_validation.csv
#      - Table of adjusted deviance-explained metrics for the training and
#        validation subsets, by ecoregion and broad region, with the selected
#        model family appended.
#
# Validation approach:
#   - Restrict the final projection table to years after 1939.
#   - For each ecoregion, calculate adjusted D-squared for:
#       1. training observations (struct_model vs model_out)
#       2. validation observations (struct_valid vs model_out)
#   - Compute D-squared under two formulations:
#       a. Poisson-family D-squared
#       b. Generic / Gaussian-style D-squared (family not specified)
#   - Join these results with the recorded selected model family.
# =============================================================================
# Load required packages ---------------------------------------------------
# tidyverse: data manipulation
# foreach / doParallel: iterative processing
# modEvA: Dsquared() model-evaluation metric
# sf: loaded here but not directly used in this script
pacman::p_load(tidyverse, foreach, doParallel, modEvA, sf)

# Validation ---------------------------------------------------------------
# Input:
#   Data/Processed/structures/Model/structures_complete.csv
# Purpose:
#   Read the final compiled structure projections and retain years after 1939
#   for model evaluation.
t1 <- read.csv('Data/Processed/structures/Model/structures_complete.csv') 
t1 <- filter(t1, year > 1939)

# Training-set evaluation --------------------------------------------------
# Purpose:
#   For rows with observed training data (struct_model), calculate adjusted
#   deviance explained for each ecoregion under:
#     1. a Poisson formulation
#     2. a generic formulation (family not specified)
train <- t1 %>% 
  filter(!is.na(struct_model)) %>% 
  group_by(region, id) %>% 
  summarize(
    dev_train_pois = Dsquared(
      obs = struct_model,
      pred = model_out,
      family = 'poisson',
      dismo.version = TRUE,
      adjust = TRUE,
      npar = 1
    ),
    dev_train_g = Dsquared(
      obs = struct_model,
      pred = model_out,
      dismo.version = TRUE,
      adjust = TRUE,
      npar = 1
    )
  )

# Validation-set evaluation ------------------------------------------------
# Purpose:
#   For rows with observed validation data (struct_valid), calculate adjusted
#   deviance explained for each ecoregion under the same two formulations.
valid <- t1 %>% 
  filter(!is.na(struct_valid)) %>% 
  group_by(region, id) %>% 
  summarize(
    dev_valid_pois = Dsquared(
      obs = struct_valid,
      pred = model_out,
      family = 'poisson',
      dismo.version = TRUE,
      adjust = TRUE,
      npar = 1
    ),
    dev_valid_g = Dsquared(
      obs = struct_valid,
      pred = model_out,
      dismo.version = TRUE,
      adjust = TRUE,
      npar = 1
    )
  )

# Combine training and validation metrics ---------------------------------
# Purpose:
#   Join training and validation deviance summaries by region and ecoregion.
combo <- full_join(train, valid)

# Recover selected model family -------------------------------------------
# Input:
#   Data/Processed/structures/Model/summaries/
# Purpose:
#   Read each model summary file and record whether the negative binomial or
#   Poisson model had the lower AIC for that ecoregion.
# Note:
#   Files marked "no_private_land" are skipped.
get_mod <- function(model_sum){
  if(grepl('no_private_land', model_sum)){
    return()
  } else {
    ms <- read.csv(model_sum)
    a <- data.frame(
      id = gsub('.csv', '', basename(model_sum)),
      model = ifelse(ms$aic_nb < ms$aic_poiss, 'Negative binomial', 'Poisson')
    )
    return(a)
  }
}

# Apply model lookup to all summary files.
files <- list.files(
  'Data/Processed/structures/Model/summaries',
  full.names = TRUE
)

a <- foreach(i = 1:length(files), .combine = rbind) %do%
  get_mod(files[[i]])

# Append selected model family to deviance results.
combo <- left_join(combo, a)

# Output:
#   Data/Processed/structures/Model/deviance_validation.csv
# Description:
#   Validation table containing training and validation deviance-explained
#   metrics plus the selected model family for each ecoregion.
write.csv(combo, 'Data/Processed/structures/Model/deviance_validation.csv', row.names = FALSE)
