# =============================================================================
# Script: 9_combine_mtbs_and_fire_projections_l4.R
# Description:
#   Combines historical MTBS burned-area data with projected burned-area
#   estimates from multiple climate models (CNRM, MRI, HadGEM2-ES) at the
#   Level IV ecoregion scale. Historical and projected datasets are merged,
#   missing MTBS years are filled, and the maximum historical burned area per
#   ecoregion is attached for reference. The resulting combined datasets are
#   written for each climate model for downstream analysis and visualization.
# Inputs:
#   Data/Processed/mtbs_l4.csv;
#   Data/Processed/Fire_projections/Fire_projections_CNRM_l4.csv;
#   Data/Processed/Fire_projections/Fire_projections_MRI_l4.csv;
#   Data/Processed/Fire_projections/Fire_projections_HadGEM2-ES_l4.csv
# Outputs:
#   Data/Processed/Fire_projections/Fire_combo_CNRM_l4.csv;
#   Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv;
#   Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv
# =============================================================================

# Load required packages ---------------------------------------------------
pacman::p_load(tidyverse, sf, foreach)


# Read and prepare historical MTBS data -----------------------------------
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

mtbs_sum <- mtbs %>% 
  group_by(US_L4CODE) %>% 
  summarise(mx = max(area_burned))


# -------------------------------------------------------------------------
# Climate model: CNRM

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
