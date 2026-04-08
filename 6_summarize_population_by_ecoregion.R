# =============================================================================
# Script: summarize_population_by_ecoregion.R
#
# Purpose:
#   Summarize ICLUS population counts within Level IV ecoregions by rasterizing
#   polygon population data to a 250 m grid and then aggregating population
#   totals within each ecoregion.
#
#   The script converts polygon-level population totals to per-cell values,
#   rasterizes each population field to a common 250 m template, and computes
#   zonal sums for each Level IV ecoregion.
#
# Inputs required:
#   1. Data/Raw/us_eco_l4_state_boundaries
#      - Vector layer of U.S. Level IV ecoregion boundaries.
#      - Used as the zonal units for population summaries.
#
#   2. Data/Raw/ICLUS_v2_1_1_population.gdb
#      - ICLUS v2.1.1 population geodatabase.
#      - Must contain polygon geometries and population attributes, including
#        fields from TOTALPOP90 through SSP52100.
#
# Outputs produced:
#   1. Data/Processed/Population_summaries.csv
#      - Table of population sums by Level IV ecoregion for all selected
#        population variables.
#
# Main processing steps:
#   1. Read and filter Level IV ecoregions.
#   2. Read ICLUS population polygons and transform to the same CRS.
#   3. Convert polygon population totals to 250 m cell-equivalent values.
#   4. Rasterize each population field to a common 250 m template.
#   5. Sum rasterized population values within each ecoregion.
#   6. Write the resulting ecoregion-by-variable summary table to disk.
# =============================================================================


# Load required packages ---------------------------------------------------

# tidyverse: data manipulation
# sf: vector data handling
# foreach / doParallel: looping infrastructure
# raster: raster creation and stacking
# fasterize: fast rasterization of sf polygons
# exactextractr: exact zonal extraction from rasters
pacman::p_load(tidyverse, sf, foreach, doParallel, raster, fasterize, exactextractr)


# Read and prepare Level IV ecoregions ------------------------------------

# Input:
#   Data/Raw/us_eco_l4_state_boundaries
# Purpose:
#   Load Level IV ecoregions and create a unique ID for each polygon.
#   Very small polygons are removed to avoid summaries for regions smaller than
#   four 250 m pixels.
eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>% 
  mutate(ID = paste(L1_KEY, US_L4CODE, Shape_Area, sep = '_')) %>% 
  filter(Shape_Area >= 250 * 250 * 4)  # retain polygons >= 4 pixels


# Read and prepare ICLUS population polygons ------------------------------

# Input:
#   Data/Raw/ICLUS_v2_1_1_population.gdb
# Purpose:
#   Load ICLUS population polygons, transform them to the ecoregion CRS,
#   and calculate polygon area for later conversion of totals to per-cell values.
pop <- st_read('Data/Raw/ICLUS_v2_1_1_population.gdb') %>% 
  st_transform(st_crs(eco_shp)) %>%
  mutate(area = as.numeric(st_area(.)))


# Create a 250 m raster template ------------------------------------------

# Purpose:
#   Define the target raster grid for rasterization. The grid uses the full
#   extent of the filtered ecoregions and a 250 m cell size.
r_template <- raster(extent(eco_shp), resolution = 250)
crs(r_template) <- st_crs(eco_shp)$proj4string


# Separate geometry and attributes ----------------------------------------

# Purpose:
#   Store geometry separately so population attributes can be modified and
#   then reattached to the original polygon geometry.
geom <- st_geometry(pop)


# Convert polygon totals to per-cell-equivalent values --------------------

# Purpose:
#   Select the population fields of interest and convert each polygon total
#   into a 250 m cell-equivalent value by scaling with:
#
#       value * 250 * 250 / polygon_area
#
#   This step distributes polygon totals to the raster grid before zonal
#   aggregation.
pop_mod <- pop %>% 
  st_drop_geometry() %>% 
  dplyr::select("TOTALPOP90":"SSP52100") %>%
  mutate(across(everything(), ~ . * 250 * 250 / pop$area))


# Reattach geometry to the modified attributes ----------------------------

# Purpose:
#   Reconstruct an sf object for rasterization.
pop_mod <- st_sf(pop_mod, geometry = geom)


# Identify fields to rasterize --------------------------------------------

# Purpose:
#   Create a vector of all population fields, excluding the geometry column.
field <- names(pop_mod)[1:(length(names(pop_mod)) - 1)]


# Rasterize each population field -----------------------------------------

# Purpose:
#   Rasterize each ICLUS population field onto the 250 m template.
#   The 'max' function is used if multiple polygons overlap a cell.
stack_pop <- foreach(i = 1:length(field)) %do% {
  fasterize(pop_mod, r_template, field = field[i], fun = 'max')
}

# Combine individual rasters into a RasterStack and restore field names.
stack_pop <- raster::stack(stack_pop)
names(stack_pop) <- field


# Summarize population within each ecoregion ------------------------------

# Purpose:
#   For each ecoregion, use exact extraction to sum each rasterized population
#   field within the polygon boundary.
pop_mean <- foreach(i = 1:nrow(eco_shp), .combine = rbind) %do% {
  cbind(
    ID = eco_shp$ID[i],
    exact_extract(stack_pop, eco_shp[i,], c('sum'))
  )
}


# Check results -----------------------------------------------------------

# Purpose:
#   Compare total input polygon populations to total ecoregion-summed raster
#   populations as a coarse consistency check.
colSums(st_drop_geometry(pop[,5:61])) / colSums(pop_mean[,-1])


# Write output ------------------------------------------------------------

# Output:
#   Data/Processed/Population_summaries.csv
# Description:
#   Table of summed population values for each Level IV ecoregion across all
#   selected ICLUS population fields.
write.csv(pop_mean, 'Data/Processed/Population_summaries.csv')
