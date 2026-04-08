# =============================================================================
# Script: 11_summarize_zillow_structures_by_ecoregion.R
# Description:
#   Summarizes annual structure counts, occupied area, and structure density
#   from the compiled structures table at Level III and Level IV ecoregion
#   scales. The script combines training and validation values, derives
#   ecoregion identifiers, fills missing years from 1984–2023 by interpolation,
#   and exports annual ecoregion summary tables for downstream analysis.
# Inputs:
#   Data/Processed/Summaries/structures.csv
# Outputs:
#   Data/Processed/zillow_l3_2.csv;
#   Data/Processed/zillow_l4_2.csv
# =============================================================================

# Load packages used for data manipulation and grouped completion
pacman::p_load(tidyverse, foreach)

# Read compiled structure summaries
zill <- read.csv('Data/Processed/Summaries/structures.csv')

# Keep rows with observed training or validation structure values
zill <- filter(zill, !is.na(struct_training) | !is.na(struct_valid))

# Restrict to years used in the main historical analysis period
zill <- filter(zill, year > 1983)

# Derive Level IV and Level III ecoregion codes, combine training/validation
# values into single structure and occupied-area fields, and compute structure
# density as structures per hectare
zill <- zill %>% 
  mutate(
    US_L4CODE = sapply(eco_l4, function(x) str_split_fixed(x, '_', 2)[1,1]),
    US_L3CODE = sapply(US_L4CODE, function(x) { gsub("[^0-9]", "", x) }),
    structures = ifelse(is.na(struct_training), struct_valid, struct_training),
    area_occ = ifelse(is.na(exps_training), exps_valid, exps_training),
    str_density = 10000 * structures / area_occ
  )

# Replace undefined densities from 0/0 cases with zero
zill$str_density[is.na(zill$str_density)] <- 0


# -------------------------------------------------------------------------
# Level III ecoregion summaries

# Aggregate annual structures, occupied area, and median structure density
# within each Level III ecoregion
zill_l3 <- zill %>% 
  group_by(year, US_L3CODE) %>% 
  summarise(
    structures = sum(structures),
    area_occ = sum(area_occ),
    str_density = median(str_density)
  )

# Fill missing years from 1984–2023 within each Level III ecoregion and
# interpolate missing annual values
zillow_int_l3 <- zill_l3 %>%
  group_by(US_L3CODE) %>%
  complete(year = 1984:2023) %>%
  arrange(US_L3CODE, year) %>%
  group_by(US_L3CODE) %>%
  mutate(
    structures = approx(year, structures, xout = year, rule = 2)$y,
    area_occ = approx(year, area_occ, xout = year, rule = 2)$y,
    str_density = approx(year, str_density, xout = year, rule = 2)$y
  ) %>%
  ungroup()

# Rename year field to match other annual summary outputs
names(zillow_int_l3)[2] <- 'START_YEAR'

# Write Level III annual summary table
write.csv(zillow_int_l3, "Data/Processed/zillow_l3_2.csv", row.names = FALSE)


# -------------------------------------------------------------------------
# Level IV ecoregion summaries

# Aggregate annual structures, occupied area, and median structure density
# within each Level IV ecoregion
zill_l4 <- zill %>% 
  group_by(year, US_L4CODE) %>% 
  summarise(
    structures = sum(structures),
    area_occ = sum(area_occ),
    str_density = median(str_density)
  )

# Fill missing years from 1984–2023 within each Level IV ecoregion and
# interpolate missing annual values
zillow_int_l4 <- zill_l4 %>%
  group_by(US_L4CODE) %>%
  complete(year = 1984:2023) %>%
  arrange(US_L4CODE, year) %>%
  group_by(US_L4CODE) %>%
  mutate(
    structures = approx(year, structures, xout = year, rule = 2)$y,
    area_occ = approx(year, area_occ, xout = year, rule = 2)$y,
    str_density = approx(year, str_density, xout = year, rule = 2)$y
  ) %>%
  ungroup()

# Rename year field to match other annual summary outputs
names(zillow_int_l4)[2] <- 'START_YEAR'

# Write Level IV annual summary table
write.csv(zillow_int_l4, "Data/Processed/zillow_l4_2.csv", row.names = FALSE)
