# =============================================================================
# Script: build_exclusion_layers.R
# Purpose:
#   Create a national exclusion raster for the conterminous United States
#   by combining protected lands ownership classes (PAD-US) with major lakes
#   and reservoirs. The output is aligned to a pre-existing 250 m template grid.
#
# Inputs required:
#   1. Data/Raw/Empty_250_US.tif
#      - Empty template raster defining resolution, extent, and CRS.
#      - Used as the reference grid for all rasterization steps.
#
#   2. Data/Raw/cb_2019_us_county_500k
#      - U.S. Census county boundaries shapefile or directory.
#      - Used to define the conterminous U.S. boundary and crop the template.
#
#   3. Data/Raw/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb
#      - PAD-US geodatabase.
#      - Layer used:
#        "PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement"
#      - Used to rasterize protected lands by ownership type.
#
#   4. Data/Raw/MajorLakesAndReservoirs
#      - Vector layer of major lakes and reservoirs.
#      - Used to add water bodies to the exclusion raster.
#
# Outputs produced:
#   1. Data/Processed/protected_all_classes.tif
#      - Raster of protected lands ownership classes.
#
#   2. Data/Processed/exclude.tif
#      - Raster combining protected lands and water exclusions.
#
# Notes:
#   - This script excludes Alaska, Hawaii, Puerto Rico, and other U.S.
#     territories, retaining the lower 48 states plus Washington, D.C.
#   - Water is assigned its own class in the final exclusion raster.
#   - Ownership classes are derived from PAD-US Own_Type.
# =============================================================================

# Load required packages ---------------------------------------------------
# These packages are used for data import, vector/raster processing,
# rasterization, and plotting.
pacman::p_load(tidyverse, raster, sf, fasterize, viridis, terra)

# Load template raster -----------------------------------------------------
# Input:
#   Data/Raw/Empty_250_US.tif
# Purpose:
#   Defines the target raster grid (extent, resolution, projection) used for
#   all subsequent raster outputs.
empty_grid <- raster('Data/Raw/Empty_250_US.tif')

# Load and prepare U.S. county boundaries ---------------------------------
# Input:
#   Data/Raw/cb_2019_us_county_500k
# Purpose:
#   Provides a U.S. land boundary for cropping the template raster and
#   restricting analysis to the conterminous United States + D.C.
us <- st_read('Data/Raw/cb_2019_us_county_500k') %>% 
  st_transform(crs(empty_grid))

# Exclude non-CONUS states and territories:
# 78 = USVI, 72 = PR, 69 = MP, 66 = GU, 60 = AS, 15 = HI, 02 = AK
us <- subset(us, !(STATEFP %in% c('78', '72', '69', '66', '60', '15', '02')))

# Dissolve counties into a single CONUS polygon for plotting/cropping.
us1 <- st_union(us[,10])

# Crop template raster to the CONUS boundary.
empty_grid <- empty_grid %>% 
  crop(us)

# Create output directories ------------------------------------------------
# Purpose:
#   Ensures output folders exist before writing processed files.
if(!dir.exists('Data/Processed')){
  dir.create('Data/Processed')
}

if(!dir.exists('Data/Processed/Land_use')){
  dir.create('Data/Processed/Land_use')
}

# Protected lands ----------------------------------------------------------
# Inspect available layers in the PAD-US geodatabase.
st_layers('Data/Raw/PADUS4_0Geodatabase/PAD_US4_0.gdb')

# Input:
#   Data/Raw/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb
# Layer:
#   PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement
# Purpose:
#   Load PAD-US protected areas and retain selected ownership classes for
#   rasterization to the template grid.
protected <- st_read(
  'Data/Raw/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb',
  layer = 'PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement'
) %>% 
  st_transform(crs(empty_grid))

# Reference:
# U.S. Geological Survey (USGS) Gap Analysis Project (GAP), 2024,
# Protected Areas Database of the United States (PAD-US) 3.0
# (ver. 2.0, March 2023): https://doi.org/10.5066/P9Q9LQ4B

# Filter to retained jurisdictions and ownership types.
# Retained ownership classes:
#   FED  = federal
#   TRIB = Tribal
#   STAT = state
#   LOC  = local
#   JNT  = joint
protected <- protected %>% 
  filter(
    !State_Nm %in% c('UNKF', 'AS', 'MP', 'GU', 'UM', 'WV', 'AK', 'HI', 'PR') &
      Own_Type %in% c('FED', 'TRIB', 'STAT', 'LOC', 'JNT')
  )

# Convert ownership type to a factor, then to numeric codes for rasterization.
# This allows fasterize() to write integer values to raster.
protected$Own_Type_f <- factor(protected$Own_Type)
protected$Own_Type_num <- as.numeric(protected$Own_Type_f)

# Keep only polygon geometries, since rasterization requires area features.
protected_pol <- protected[st_geometry_type(protected) %in% c('POLYGON', 'MULTIPOLYGON'),] 

# Rasterize protected lands to the template grid using ownership class codes.
protected_r <- protected_pol %>% 
  fasterize(empty_grid, field = 'Own_Type_num')

# Recover the original ownership class labels.
character_levels <- levels(protected_pol$Own_Type_f)

# Convert numeric raster values back to factor labels.
protected_r2 <- protected_r
values(protected_r2) <- factor(
  values(protected_r),
  levels = 1:length(character_levels),
  labels = character_levels
)

# Output:
#   Data/Processed/protected_all_classes.tif
# Description:
#   Raster of protected lands, labeled by ownership class.
writeRaster(protected_r2, 'Data/Processed/protected_all_classes.tif', overwrite = TRUE)

# Quick visual check.
plot(protected_r2)

# Hydrology ---------------------------------------------------------------
# Input:
#   Data/Raw/MajorLakesAndReservoirs
# Purpose:
#   Load major lakes and reservoirs and rasterize them to the template grid.
#   These water bodies are added as exclusions in the final raster.
lakes <- st_read('Data/Raw/MajorLakesAndReservoirs') %>% 
  st_transform(crs(empty_grid))

# Rasterize water bodies to the template grid.
lakes_r <- lakes %>% 
  fasterize(empty_grid)

# Assign a numeric code for water.
# Here, water is coded as 6 so it can be distinguished from PAD-US classes.
values(lakes_r) <- ifelse(!is.na(values(lakes_r)), 6, NA)

# Quick visual check.
plot(us1)
plot(lakes_r, add = TRUE)

# Exclusion raster ---------------------------------------------------------
# Purpose:
#   Combine water and protected lands into a single exclusion raster.
#   Where water is present, it takes precedence; otherwise use protected land.
exclude <- overlay(
  lakes_r,
  protected_r,
  fun = function(x, y) ifelse(!is.na(x), x, y)
)

# Convert combined numeric raster to labeled factor raster.
exclude2 <- exclude
character_levels2 <- c(character_levels, levels(factor('water')))

values(exclude2) <- factor(
  values(exclude),
  levels = 1:length(character_levels2),
  labels = character_levels2
)

# Quick visual check.
plot(us1)
plot(exclude2, add = TRUE)

# Output:
#   Data/Processed/exclude.tif
# Description:
#   Combined exclusion raster containing protected land ownership classes
#   plus a water class.
writeRaster(exclude2, 'Data/Processed/exclude.tif', overwrite = TRUE)

# Optional: convert raster to data frame for inspection/export
# exdf <- as.data.frame(exclude2, xy = TRUE)
# str(exdf)
# f <- exdf[complete.cases(exdf), ]
