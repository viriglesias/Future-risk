# =============================================================================
# Script: 5_validate_final_structure_models.R
# Description:
#   Evaluates the final selected structure projection models by calculating
#   adjusted deviance-explained metrics for both training and validation data
#   within each Level IV ecoregion. The workflow also recovers the selected
#   model family for each ecoregion from the candidate model summaries and
#   exports a validation table summarizing model performance across regions.
# Inputs:
#   Data/Processed/structures/Model/structures_complete.csv;
#   Data/Processed/structures/Model/summaries/*.csv
# Outputs:
#   Data/Processed/structures/Model/deviance_validation.csv
# =============================================================================

# Load required packages ---------------------------------------------------

pacman::p_load(tidyverse, foreach, doParallel, modEvA, sf)

# Validation ---------------------------------------------------------------

t1 <- read.csv('Data/Processed/structures/Model/structures_complete.csv') 
t1 <- filter(t1, year > 1939)

# Training-set evaluation --------------------------------------------------

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
#   Join training and validation deviance summaries by region and ecoregion.
combo <- full_join(train, valid)

# Recover selected model family -------------------------------------------

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

write.csv(combo, 'Data/Processed/structures/Model/deviance_validation.csv', row.names = FALSE)
