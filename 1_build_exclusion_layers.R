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
pacman::p_load(tidyverse, raster, sf, fasterize, viridis, terra)

# Load template raster -----------------------------------------------------
empty_grid <- raster('Data/Raw/Empty_250_US.tif')

# Load and prepare U.S. county boundaries ---------------------------------
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
#Create directories if they dont exist
if(!dir.exists('Data/Processed')){
  dir.create('Data/Processed')
}

if(!dir.exists('Data/Processed/Land_use')){
  dir.create('Data/Processed/Land_use')
}


# Protected lands -----------------------------------------------------------
st_layers('Data/Raw/PADUS4_0Geodatabase/PAD_US4_0.gdb')
protected <- st_read('Data/Raw/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb', layer = 'PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement') %>% 
  st_transform(crs(empty_grid))

protected <- protected %>% 
  filter(!State_Nm %in% c('UNKF', 'AS', 'MP', 'GU', 'UM', 'WV', 'AK', 'HI', 'PR') &
           Own_Type %in% c('FED', 'TRIB', 'STAT', 'LOC', 'JNT'))

# Convert to factor
protected$Own_Type_f <- factor(protected$Own_Type)

# Convert the factor to numeric
protected$Own_Type_num <- as.numeric(protected$Own_Type_f)
protected_pol <- protected[st_geometry_type(protected) %in% c('POLYGON', 'MULTIPOLYGON'),] 

protected_r <- protected_pol %>% 
  fasterize(empty_grid, field = 'Own_Type_num')

# Extract ecoregion names
character_levels <- levels(protected_pol$Own_Type_f)

# Create a new raster to store character values
protected_r2 <- protected_r

# Replace numeric values with corresponding character values
values(protected_r2) <- factor(values(protected_r), levels = 1:length(character_levels), labels = character_levels)
writeRaster(protected_r2, 'Data/Processed/protected_all_classes.tif', overwrite=T)

plot(protected_r2)


# Hydrology ---------------------------------------------------------------
lakes <- st_read('Data/Raw/MajorLakesAndReservoirs') %>% 
  st_transform(crs(empty_grid)) # Location of lAkes & reservoirs from https://public-nps.opendata.arcgis.com/search?collection=Dataset&q=boundaries

lakes_r <- lakes %>% 
  fasterize(empty_grid)

values(lakes_r) <- ifelse(!is.na(values(lakes_r)), 6, NA)
plot(us1)

plot(lakes_r, add=T)


# Exclusion raster ---------------------------------------------------------
exclude <- overlay(lakes_r, protected_r, fun = function(x,y) ifelse(!is.na(x), x, y))
plot(us1)
plot(exclude2, add = T)

exclude2 <- exclude
character_levels2 <- c(character_levels, levels(factor('water')))

# Replace numeric values with corresponding character values
values(exclude2) <- factor(values(exclude), levels = 1:length(character_levels2), labels = character_levels2)

writeRaster(exclude2, 'Data/Processed/exclude.tif', overwrite = T)



