# =============================================================================
# Script: summarize_mtbs_by_ecoregion.R
#
# Purpose:
#   Summarize MTBS wildfire burned area by Level IV and Level III ecoregions.
#   The script assigns MTBS fire events to ecoregions using ignition-point
#   locations, aggregates annual burned area by ecoregion, and calculates the
#   proportion of each ecoregion burned per year.
#
# Inputs required:
#   1. Data/Raw/us_eco_l4_state_boundaries
#      - Vector layer of U.S. Level IV ecoregion boundaries.
#      - Also contains Level III identifiers used for aggregation.
#
#   2. Data/Raw/mtbs_perimeter_data 2
#      - MTBS fire perimeter dataset.
#      - Used to obtain Event_ID, ignition year, and burned area.
#
#   3. Data/Raw/mtbs_fod_pts_data
#      - MTBS point dataset (event ignition points).
#      - Used to assign events to ecoregions via spatial join.
#
# Outputs produced:
#   1. Data/Processed/mtbs_eco.csv
#      - Event-level MTBS wildfire table with ecoregion identifiers.
#
#   2. Data/Processed/mtbs_l4.csv
#      - Annual burned area and proportion burned by Level IV ecoregion.
#
#   3. Data/Processed/mtbs_l3.csv
#      - Annual burned area and proportion burned by Level III ecoregion.
#
# Notes:
#   - Burned area is stored in hectares.
#   - Only events classified as Wildfire or Unkown are retained, matching the
#     original code.
#   - Missing years within each ecoregion are filled with zeros for 1984–2024.
# =============================================================================


# Load required packages ---------------------------------------------------

# tidyverse: data manipulation
# sf: vector data handling
pacman::p_load(tidyverse, sf)


# Read ecoregion boundaries ------------------------------------------------

# Input:
#   Data/Raw/us_eco_l4_state_boundaries
# Purpose:
#   Load ecoregion polygons used for spatial joins and area calculations.
eco_l4 <- st_read('Data/Raw/us_eco_l4_state_boundaries')


# Read MTBS fire perimeter data -------------------------------------------

# Input:
#   Data/Raw/mtbs_perimeter_data 2
# Purpose:
#   Load MTBS fire perimeters, transform to the ecoregion CRS, calculate
#   burned area in hectares, and retain wildfire events.
mtbs_sf <- st_read('Data/Raw/mtbs_perimeter_data 2') %>% 
  st_transform(st_crs(eco_l4))

# Calculate burned area in hectares
mtbs_sf <- mtbs_sf %>% 
  mutate(area_burned_ha = as.numeric(st_area(.) * 0.0001))

# Keep only wildfire / unknown wildfire-type events
mtbs_sf <- mtbs_sf %>% 
  filter(Incid_Type %in% c('Wildfire', 'Unkown'))

# Extract ignition year
mtbs_sf <- mtbs_sf %>% 
  mutate(year = year(Ig_Date))

# Keep the event-level attributes needed downstream
mtbs <- mtbs_sf %>% 
  st_drop_geometry() %>% 
  select(Event_ID, year, area_burned_ha)


# Read MTBS ignition-point data -------------------------------------------

# Input:
#   Data/Raw/mtbs_fod_pts_data
# Purpose:
#   Load MTBS point data and use ignition-point locations to assign each event
#   to an ecoregion.
mtbs_pt <- st_read('Data/Raw/mtbs_fod_pts_data') %>% 
  st_transform(st_crs(eco_l4)) %>% 
  filter(Incid_Type %in% c('Wildfire', 'Unkown')) %>% 
  select(Event_ID, Ig_Date)

# Spatially join points to Level IV ecoregions
mtbs_eco <- st_join(mtbs_pt, eco_l4, join = st_within)

# Drop events that do not intersect an ecoregion polygon
mtbs_eco <- mtbs_eco %>% 
  filter(!is.na(L1_KEY))


# Combine perimeter attributes with ecoregion assignment -------------------

# Purpose:
#   Join event-level burned area and year from the perimeter data to the
#   ecoregion assignment derived from the ignition points.
mtbs <- right_join(mtbs, mtbs_eco)
mtbs <- st_drop_geometry(mtbs)

# Retain the fields used in later summaries
mtbs <- mtbs[, c(1:5, 7)]

# Write event-level output
write.csv(mtbs, "Data/Processed/mtbs_eco.csv", row.names = FALSE)
# Output units: hectares
# This file contains event-level wildfire records with ecoregion assignment.


# Re-read event-level table ------------------------------------------------

# Purpose:
#   Read the saved event-level file for downstream aggregation.
mtbs <- read.csv("Data/Processed/mtbs_eco.csv")


# Calculate ecoregion areas ------------------------------------------------

# Purpose:
#   Compute total area of each Level IV and Level III ecoregion in hectares
#   to allow calculation of annual proportion burned.
eco <- st_drop_geometry(eco_l4)

eco <- eco %>% 
  group_by(US_L4CODE) %>% 
  summarise(area_eco_l4 = sum(Shape_Area, na.rm = TRUE) * 0.0001)  # hectares

eco3 <- eco_l4 %>% 
  group_by(US_L3CODE) %>% 
  summarise(area_eco_l3 = sum(Shape_Area, na.rm = TRUE) * 0.0001)  # hectares

# Diagnostic checks
sum(eco$area_eco_l4)
sum(eco3$area_eco_l3)


# Retain relevant columns for aggregation ---------------------------------

mtbs <- mtbs[, 1:6]


# Aggregate annual burned area by Level IV ecoregion ----------------------

mtbs_l4 <- mtbs %>% 
  group_by(US_L4CODE, year) %>% 
  summarise(area_burned_l4 = sum(area_burned_ha, na.rm = TRUE))

mtbs_l4 <- mtbs_l4 %>% 
  ungroup() %>% 
  left_join(eco)

# Fill missing years for each Level IV ecoregion and compute proportion burned
mtbs_l4 <- mtbs_l4 %>% 
  group_by(US_L4CODE) %>%
  complete(year = 1984:2024) %>% 
  arrange(US_L4CODE, year) %>% 
  mutate(prop_area_burned_eco_l4 = area_burned_l4 / area_eco_l4) %>%
  ungroup()

# Replace missing annual burned area and proportions with zero
mtbs_l4 <- mtbs_l4 %>% 
  mutate(
    area_burned_l4 = ifelse(is.na(area_burned_l4), 0, area_burned_l4),
    prop_area_burned_eco_l4 = ifelse(is.na(prop_area_burned_eco_l4), 0, prop_area_burned_eco_l4)
  )

# Write Level IV output
write.csv(mtbs_l4, "Data/Processed/mtbs_l4.csv", row.names = FALSE)
# Output units: hectares for area_burned_l4

# Diagnostic check
test1 <- mtbs_l4 %>% 
  summarise(area_burned_l4 = sum(area_burned_l4, na.rm = TRUE))


# Aggregate annual burned area by Level III ecoregion ---------------------

mtbs_l3 <- mtbs %>% 
  group_by(US_L3CODE, year) %>% 
  summarise(area_burned_l3 = sum(area_burned_ha, na.rm = TRUE))

mtbs_l3 <- mtbs_l3 %>% 
  mutate(US_L3CODE = as.character(US_L3CODE)) %>% 
  ungroup() %>% 
  left_join(eco3)

# Fill missing years for each Level III ecoregion and compute proportion burned
mtbs_l3 <- mtbs_l3 %>% 
  group_by(US_L3CODE) %>%
  complete(year = 1984:2024) %>% 
  arrange(US_L3CODE, year) %>% 
  mutate(prop_area_burned_eco_l3 = area_burned_l3 / area_eco_l3) %>%
  ungroup()

# Replace missing annual burned area and proportions with zero
mtbs_l3 <- mtbs_l3 %>% 
  mutate(
    area_burned_l3 = ifelse(is.na(area_burned_l3), 0, area_burned_l3),
    prop_area_burned_eco_l3 = ifelse(is.na(prop_area_burned_eco_l3), 0, prop_area_burned_eco_l3)
  )

# Drop geometry before export
mtbs_l3 <- st_drop_geometry(mtbs_l3)
mtbs_l3 <- mtbs_l3 %>% 
  select(-geometry)

# Diagnostic check
test2 <- mtbs_l3 %>% 
  summarise(area_burned_l3 = sum(area_burned_l3, na.rm = TRUE))

# Write Level III output
write.csv(mtbs_l3, "Data/Processed/mtbs_l3.csv", row.names = FALSE)
# Output units: hectares for area_burned_l3


# Optional diagnostic check ------------------------------------------------
# filter(mtbs_l4, year %in% 2000) %>% 
#   summarise(sum(area_burned_l4))
