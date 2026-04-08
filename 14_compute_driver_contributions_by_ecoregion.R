# =============================================================================
# Script: 14_compute_spatial_driver_contributions_by_ecoregion.R
# Description:
#   Quantifies the relative contribution of three risk components—structure
#   exposure, burned-area fraction, and structure-loss intensity (EASLI)—to
#   projected structure-loss risk within Level III ecoregions. The script
#   aggregates risk inputs into multi-year periods, transforms components to
#   log space, computes proportional driver contributions, identifies the
#   dominant driver in each region and period, and exports the summarized
#   contribution table.
# Inputs:
#   Data/Processed/risk.csv
# Outputs:
#   Data/Processed/spatial_driver_contributions_by_type_period_ecoregion.csv
# =============================================================================

# Load packages for data manipulation and formatting utilities
pacman::p_load(tidyverse, scales) 

# Read combined fire–structure risk dataset
combo <- read.csv('Data/Processed/risk.csv')

# Remove incomplete MTBS observation for 2023
combo <- combo %>% 
  filter(!(type %in% 'MTBS' & Year %in% 2023))

# Numerical tolerance constant (not used later but retained for consistency)
tol <- 1e-12

# Helper function: assign each year to a multi-year period (default 2-year bins)
make_period <- function(year, start = 2001, width = 2) {
  
  pstart <- start + floor((year - start) / width) * width
  pend <- pstart + width - 1
  
  paste0(pstart, "-", pend)
}

# Prepare dataset with character region IDs and computed period labels
dat <- combo %>%
  mutate(
    US_L3CODE = as.character(US_L3CODE),
    type = as.character(type),
    period = make_period(Year)
  )

# Aggregate structures, fire fraction, EASLI, and risk by region and period
eco_period <- dat %>%
  group_by(type, US_L3CODE, period) %>%
  summarise(
    structures = mean(structures, na.rm = TRUE),
    fire = mean(prop_area_burned, na.rm = TRUE),
    easli = mean(damaged_dest_ratio_adj, na.rm = TRUE),
    risk = mean(str_destroyed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    structures > 0,
    fire > 0,
    easli > 0,
    risk > 0
  )

# Transform core drivers to log scale
eco_period <- eco_period %>%
  mutate(
    log_structures = log(structures),
    log_fire = log(fire),
    log_easli = log(easli),
    log_risk = log(risk)
  )

# Compute proportional contributions of each driver in log space
eco_period <- eco_period %>%
  mutate(
    total_log = log_structures + log_fire + log_easli,
    
    contrib_structures = log_structures / total_log,
    contrib_fire = log_fire / total_log,
    contrib_easli = log_easli / total_log
  )

# Alternative contribution metric based on absolute log magnitudes
eco_period <- eco_period %>%
  mutate(
    abs_total = abs(log_structures) + abs(log_fire) + abs(log_easli),
    
    share_structures = abs(log_structures) / abs_total,
    share_fire = abs(log_fire) / abs_total,
    share_easli = abs(log_easli) / abs_total
  )

# Identify the dominant driver for each region and period
eco_period <- eco_period %>%
  mutate(
    dominant_driver = case_when(
      share_structures >= share_fire &
        share_structures >= share_easli ~ "Structures",
      
      share_fire >= share_structures &
        share_fire >= share_easli ~ "Fire",
      
      TRUE ~ "EASLI"
    )
  )

# Keep key contribution fields for export
eco_period <- eco_period %>% 
  select(type, US_L3CODE, period, share_structures, share_fire, share_easli)

# Write final contribution dataset
write.csv(
  eco_period,
  'Data/Processed/spatial_driver_contributions_by_type_period_ecoregion.csv',
  row.names = FALSE
)
