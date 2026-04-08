# =============================================================================
# Script: combine_mtbs_and_fire_projections_l4.R
#
# Purpose:
#   Combine historical MTBS burned-area data with modeled fire projections at
#   the Level IV ecoregion scale for each climate model (CNRM, MRI,
#   HadGEM2-ES). The script standardizes column names, fills missing MTBS years,
#   attaches the historical maximum burned area by ecoregion, and writes one
#   combined file per climate model for use in later plotting and analysis.
#
# Inputs required:
#   1. Data/Processed/mtbs_l4.csv
#      - Historical annual burned area by Level IV ecoregion.
#      - Expected columns before renaming:
#          US_L4CODE
#          year
#          area_burned_l4
#          area_eco_l4
#          prop_area_burned_eco_l4
#
#   2. Data/Processed/Fire_projections/Fire_projections_CNRM_l4.csv
#      - Projected annual burned area by Level IV ecoregion for CNRM.
#
#   3. Data/Processed/Fire_projections/Fire_projections_MRI_l4.csv
#      - Projected annual burned area by Level IV ecoregion for MRI.
#
#   4. Data/Processed/Fire_projections/Fire_projections_HadGEM2-ES_l4.csv
#      - Projected annual burned area by Level IV ecoregion for HadGEM2-ES.
#
# Outputs produced:
#   1. Data/Processed/Fire_projections/Fire_combo_CNRM_l4.csv
#   2. Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv
#   3. Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv
#
# Output contents:
#   Each combined file contains both historical MTBS rows and projected rows,
#   with:
#     - US_L4CODE
#     - Year
#     - area_burned
#     - area_eco_l4
#     - prop_area_burned_l4
#     - type
#     - mx               (maximum historical MTBS burned area by ecoregion)
#     - area_burned_mod  (area_burned with missing values replaced by zero)
#
# Notes:
#   - MTBS years are restricted to 1985–2023.
#   - Missing MTBS ecoregion-years are filled with explicit rows using
#     tidyr::complete().
#   - The historical maximum burned area (mx) is computed from MTBS only and
#     joined to each combined dataset.
# =============================================================================


# Load required packages ---------------------------------------------------

# tidyverse: data manipulation and completion of missing years
# sf: loaded but not directly used in this script
# foreach: loaded but not directly used in this script
pacman::p_load(tidyverse, sf, foreach)


# Read and prepare historical MTBS data -----------------------------------

# Input:
#   Data/Processed/mtbs_l4.csv
# Purpose:
#   Load annual historical burned area by Level IV ecoregion and standardize
#   column names for later combination with modeled fire projections.
mtbs <- read.csv("Data/Processed/mtbs_l4.csv")

# Standardize column names
names(mtbs) <- c(
  'US_L4CODE',
  'Year',
  'area_burned',
  'area_eco_l4',
  'prop_area_burned_l4'
)

# Keep only 1985–2023 and ensure every ecoregion has a complete annual series
# over that range.
mtbs <- mtbs %>% 
  filter(Year > 1984 & Year < 2024) %>% 
  group_by(US_L4CODE) %>%
  complete(Year = 1985:2023) %>% 
  mutate(type = 'MTBS')


# Compute historical maximum burned area by ecoregion ---------------------

# Purpose:
#   Calculate the maximum historical MTBS burned area in each Level IV
#   ecoregion. This is later joined to each model-combined table.
mtbs_sum <- mtbs %>% 
  group_by(US_L4CODE) %>% 
  summarise(mx = max(area_burned))


# -------------------------------------------------------------------------
# Climate model: CNRM
# -------------------------------------------------------------------------

# Read projected burned area for CNRM
cnrm <- read.csv('Data/Processed/Fire_projections/Fire_projections_CNRM_l4.csv')

# Label rows by source type
cnrm <- cnrm %>% 
  mutate(type = 'CNRM')

# Combine historical MTBS with projected CNRM data
combo <- rbind(mtbs, cnrm)

# Join historical maximum burned area by ecoregion
combo <- left_join(combo, mtbs_sum)

# Replace missing burned-area values with zero
combo <- combo %>% 
  mutate(area_burned_mod = ifelse(is.na(area_burned), 0, area_burned))

# Write combined MTBS + CNRM table
write.csv(
  combo,
  'Data/Processed/Fire_projections/Fire_combo_CNRM_l4.csv',
  row.names = FALSE
)


# -------------------------------------------------------------------------
# Climate model: MRI
# -------------------------------------------------------------------------

# Read projected burned area for MRI
mri <- read.csv('Data/Processed/Fire_projections/Fire_projections_MRI_l4.csv')

# Label rows by source type
mri <- mri %>% 
  mutate(type = 'mri')

# Combine historical MTBS with projected MRI data
combo <- rbind(mtbs, mri)

# Join historical maximum burned area and replace missing mx values with zero
combo <- left_join(combo, mtbs_sum) %>% 
  mutate(mx = ifelse(is.na(mx), 0, mx))

# Replace missing burned-area values with zero
combo <- combo %>% 
  mutate(area_burned_mod = ifelse(is.na(area_burned), 0, area_burned))

# Write combined MTBS + MRI table
write.csv(
  combo,
  'Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv',
  row.names = FALSE
)


# -------------------------------------------------------------------------
# Climate model: HadGEM2-ES
# -------------------------------------------------------------------------

# Read projected burned area for HadGEM2-ES
HadGEM2 <- read.csv('Data/Processed/Fire_projections/Fire_projections_HadGEM2-ES_l4.csv')

# Label rows by source type
HadGEM2 <- HadGEM2 %>% 
  mutate(type = 'HadGEM2-ES')

# Combine historical MTBS with projected HadGEM2-ES data
combo <- rbind(mtbs, HadGEM2)

# Join historical maximum burned area and replace missing mx values with zero
combo <- left_join(combo, mtbs_sum) %>% 
  mutate(mx = ifelse(is.na(mx), 0, mx))

# Replace missing burned-area values with zero
combo <- combo %>% 
  mutate(area_burned_mod = ifelse(is.na(area_burned), 0, area_burned))

# Write combined MTBS + HadGEM2-ES table
write.csv(
  combo,
  'Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv'
)
