# =============================================================================
# Script: 1_build_exclusion_layers.R
# Description:
#   Generates a national exclusion raster for the conterminous United States by
#   rasterizing PAD-US protected lands ownership classes and major lakes and
#   reservoirs onto a common 250 m template grid. The script aligns all vector
#   inputs to the template raster, removes non-CONUS regions, and produces
#   standardized exclusion layers for downstream spatial analyses.
# Inputs:
#   Data/Raw/Empty_250_US.tif; Data/Raw/cb_2019_us_county_500k;
#   Data/Raw/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb;
#   Data/Raw/MajorLakesAndReservoirs
# Outputs:
#   Data/Processed/protected_all_classes.tif; Data/Processed/exclude.tif
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
