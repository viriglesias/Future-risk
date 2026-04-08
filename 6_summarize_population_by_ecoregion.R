# =============================================================================
# Script: 6_summarize_population_by_ecoregion.R
# Description:
#   Summarizes ICLUS population totals within Level IV ecoregions of the
#   conterminous United States by rasterizing polygon population data to a
#   common 250 m grid and aggregating population counts within each ecoregion.
#   Polygon population totals are converted to per-cell values prior to
#   rasterization so that zonal sums reproduce ecoregion-level population
#   estimates for downstream analysis.
# Inputs:
#   Data/Raw/us_eco_l4_state_boundaries;
#   Data/Raw/ICLUS_v2_1_1_population.gdb
# Outputs:
#   Data/Processed/Population_summaries.csv
# =============================================================================

# Load required packages ---------------------------------------------------

pacman::p_load(tidyverse, sf, foreach, doParallel, raster, fasterize, exactextractr)


# Read and prepare Level IV ecoregions ------------------------------------
eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>% 
  mutate(ID = paste(L1_KEY, US_L4CODE, Shape_Area, sep = '_')) %>% 
  filter(Shape_Area >= 250 * 250 * 4)  # retain polygons >= 4 pixels


# Read and prepare ICLUS population polygons ------------------------------

pop <- st_read('Data/Raw/ICLUS_v2_1_1_population.gdb') %>% 
  st_transform(st_crs(eco_shp)) %>%
  mutate(area = as.numeric(st_area(.)))


# Create a 250 m raster template ------------------------------------------

r_template <- raster(extent(eco_shp), resolution = 250)
crs(r_template) <- st_crs(eco_shp)$proj4string


# Separate geometry and attributes ----------------------------------------

geom <- st_geometry(pop)


# Convert polygon totals to per-cell-equivalent values --------------------

pop_mod <- pop %>% 
  st_drop_geometry() %>% 
  dplyr::select("TOTALPOP90":"SSP52100") %>%
  mutate(across(everything(), ~ . * 250 * 250 / pop$area))


# Reattach geometry to the modified attributes ----------------------------

pop_mod <- st_sf(pop_mod, geometry = geom)


# Identify fields to rasterize --------------------------------------------

field <- names(pop_mod)[1:(length(names(pop_mod)) - 1)]


# Rasterize each population field -----------------------------------------

stack_pop <- foreach(i = 1:length(field)) %do% {
  fasterize(pop_mod, r_template, field = field[i], fun = 'max')
}

# Combine individual rasters into a RasterStack and restore field names.
stack_pop <- raster::stack(stack_pop)
names(stack_pop) <- field


# Combine individual rasters into a RasterStack and restore field names.
stack_pop <- raster::stack(stack_pop)
names(stack_pop) <- field


# Summarize population within each ecoregion ------------------------------

pop_mean <- foreach(i = 1:nrow(eco_shp), .combine = rbind) %do% {
  cbind(
    ID = eco_shp$ID[i],
    exact_extract(stack_pop, eco_shp[i,], c('sum'))
  )
}

write.csv(pop_mean, 'Data/Processed/Population_summaries.csv')
