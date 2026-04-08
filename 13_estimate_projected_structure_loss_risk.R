# =============================================================================
# Script: 13_estimate_projected_structure_loss_risk.R
# Description:
#   Combines projected burned area, modeled structure counts, and previously
#   estimated damage ratios to calculate projected structure exposure and
#   expected structure loss across ecoregions. The script aggregates fire and
#   housing data to the Level III ecoregion scale, computes burned-area-based
#   exposure, estimates destroyed structures, attaches regional metadata, and
#   writes the resulting risk table for downstream analysis.
# Inputs:
#   Data/Processed/Fire_projections/fire_full.csv;
#   Data/Processed/structures/Model/structures_complete.csv;
#   Data/Processed/damage_ratio.csv;
#   Data/Raw/us_eco_l4_state_boundaries
# Outputs:
#   Data/Processed/risk.csv
# =============================================================================

# Load packages for data manipulation, spatial attributes, and summaries
pacman::p_load(tidyverse, sf, Hmisc)

# Read processed fire projections and keep key fields
fire <- read.csv('Data/Processed/Fire_projections/fire_full.csv')
fire <- fire %>% 
  select(US_L4CODE, Year, type, area_burned, prop_area_burned_l4) %>% 
  distinct()

# Read ecoregion boundaries and create Level III lookup plus area totals
eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') 
cw <- eco_shp %>% 
  st_drop_geometry() %>% 
  mutate(region = str_extract_all(L1_KEY, "\\d+")) %>% 
  select(US_L4CODE, US_L3CODE, region, Shape_Area)

# Compute total Level III ecoregion area in hectares
area_l3 <- cw %>% 
  group_by(US_L3CODE) %>% 
  summarise(area_l3 = sum(Shape_Area, na.rm = TRUE) / 10000)

# Build a compact lookup table with broad regional classes
cw <- cw[,1:3] %>% 
  distinct() %>% 
  mutate(region = ifelse(region %in% c(6, 7, 10, 11, 12, 13), 'West', 
                         ifelse(region %in% c(8, 15, 5), 'East', 
                                ifelse(region %in% c(9), 'Central', 'Double-check')))) 

# Attach Level III and region identifiers to fire records
fire <- fire %>% 
  right_join(cw[,1:3])

# Aggregate annual burned area by region, Level III ecoregion, and fire type
fire <- fire %>% 
  group_by(region, US_L3CODE, Year, type) %>%
  summarise(area_burned = sum(area_burned, na.rm = TRUE),
            .groups = "drop") 

# Convert burned area to proportion of Level III ecoregion area burned
fire <- fire %>% 
  left_join(area_l3) %>% 
  mutate(prop_area_burned = area_burned / area_l3)

# Reattach Level III area for later use
fire <- fire %>% 
  left_join(area_l3)

# Recompute proportion burned after join
fire <- fire %>% 
  mutate(prop_area_burned = area_burned / area_l3)

# Drop rows without a fire type label
fire <- filter(fire, !is.na(type))

# Read modeled structure counts and keep years after 1984
houses <- read.csv('Data/Processed/structures/Model/structures_complete.csv') 
houses <- houses %>% 
  filter(year > 1984) %>% 
  select(id, eco_l4, year, model_out)

# Attach Level III identifiers to structure projections
houses <- houses %>% 
  left_join(cw[,1:2], by = c('eco_l4' = 'US_L4CODE'))

# Aggregate annual modeled structures to Level III ecoregions
houses <- houses %>% 
  group_by(US_L3CODE, year) %>% 
  summarise(structures = sum(model_out, na.rm = TRUE)) %>% 
  ungroup()

# Join fire and structure data and compute exposed structures
combo <- left_join(fire, houses, by = c('US_L3CODE', 'Year' = 'year'))
combo <- combo %>% 
  mutate(str_exposed = prop_area_burned * structures)

# Read previously estimated damage ratios
damage_l <- read.csv('Data/Processed/damage_ratio.csv')
damage_l <- damage_l %>% 
  select(US_L3CODE, damaged_dest_ratio_adj)

# Attach damage ratios to the combined fire-structure table
combo <- combo %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE)) %>% 
  left_join(damage_l)

# Estimate destroyed structures from exposure and adjusted damage ratio
combo <- combo %>% 
  mutate(str_destroyed = str_exposed * damaged_dest_ratio_adj)

# Create a lookup from Level III ecoregion to Level I region key
eco_shp <- eco_shp %>%
  st_drop_geometry() %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE)) %>% 
  select(US_L3CODE, L1_KEY) %>% 
  distinct()

# Attach Level I regional metadata
combo <- left_join(combo, eco_shp)

# Write final projected risk table
write.csv(combo, 'Data/Processed/risk.csv', row.names = FALSE)

# Keep only records with nonzero burned proportion for later uncertainty work
combo <- filter(combo_l3, prop_area_burned > 0) 

# Summarize uncertainty intervals in destroyed structures by Level IV region
# and fire type from sensitivity-analysis draws
reg_ci_by_type <- bind_rows(lapply(names(sens_by_type), function(ty) {
  
  sens <- sens_by_type[[ty]]$sensitivity
  
  reg_all <- bind_rows(lapply(seq_along(sens$reg_draws), function(i) {
    x <- sens$reg_draws[[i]]
    x$iter <- i
    x
  }))
  
  reg_ci <- reg_all %>%
    group_by(US_L4CODE) %>%
    summarise(
      low  = quantile(str_destroyed, 0.025, na.rm = TRUE),
      med  = quantile(str_destroyed, 0.50,  na.rm = TRUE),
      high = quantile(str_destroyed, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  reg_ci$type <- ty
  reg_ci
}))

# Run space-time variance decomposition by broad region
st_by_type <- lapply(split(combo, combo$region), space_time_vardecomp, window = 10)

# Run rolling temporal variance decomposition by fire type
temp_by_type <- lapply(
  split(combo, combo$type),
  rolling_temporal_vardecomp
)
