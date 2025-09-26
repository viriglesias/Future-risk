pacman::p_load(tidyverse, raster, sf, fasterize, viridis, terra)
#Import empty grid compatible with Zillow data
empty_grid <- raster('Data/Raw/Empty_250_US.tif')

us <- st_read('Data/Raw/cb_2019_us_county_500k') %>% 
  st_transform(crs(empty_grid))#CA boundaries from https://data.ca.gov/dataset/ca-geographic-boundaries
us <- subset(us, !(STATEFP %in% c('78', '72', '69', '66', '60', '15', '02')))#keep lower 48 + DC

us1 <- st_union(us[,10])

empty_grid <- empty_grid %>% 
  crop(us)#Crop the grid

#Create directories if they dont exist
if(!dir.exists('Data/Processed')){
  dir.create('Data/Processed')
}

if(!dir.exists('Data/Processed/Land_use')){
  dir.create('Data/Processed/Land_use')
}

# Croplands --------------------------------------------------------------- DIDNT DO. WILL USE DEVELOPMENT TYPES INSTEAD
# if(!file.exists('Data/Processed/Land_use/croplands.tif')){
#   croplands <- raster('Data/Raw/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img') 
#   empty_rast <- raster(empty_grid)
#   croplands_r <- raster::resample(croplands, empty_rast, method = 'ngb')
#   croplands_m <- ifel(croplands_r %in% 'Cultivated Crops', 1, NA)
#   croplands <- mask(croplands_r, croplands_m)
#   writeRaster(croplands, 'Data/Processed/Land_use/croplands.tif', overwrite=TRUE)
# }else{
#   croplands <- raster('Data/Processed/Land_use/croplands.tif') 
# }
# 
# plot(us1)
# plot(croplands, add = T)

# Protected lands -----------------------------------------------------------
st_layers('Data/Raw/PADUS4_0Geodatabase/PAD_US4_0.gdb')
protected <- st_read('Data/Raw/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb', layer = 'PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement') %>% 
  st_transform(crs(empty_grid))
# U.S. Geological Survey (USGS) Gap Analysis Project (GAP), 2024, Protected Areas Database of the United States (PAD-US) 3.0 (ver. 2.0, March 2023): U.S. Geological Survey data release, https://doi.org/10.5066/P9Q9LQ4B.
# Summary
# The Protected Areas Database of the United States (PAD-US) version 3.0 dataset was clipped to 56 State and Territorial boundaries to provide data downloads for areas of interest. The Census Bureau is the national steward for legally defined geographic area boundaries for State and Equivalent areas. The national sub-state geography geodatabase is updated annually and contains multiple layers reflecting selected geography, including state name and jurisdiction; this dataset is translated by USGS and used to standardize "State-Name" and improve boundary quality as a common reference for agencies contributing data for PAD-US. 
#includes dod, blm, np, nf, reservations and other federally, state, and locally managed land
# see /Users/viig7608/Desktop/Fire risk/Data/Raw/PADUS_Standard_Tables_1.xlsx for attribute info
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
# Find for the country: lakes & rivers
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
# exdf <- as.data.frame(exclude2, xy=T)
# str(exdf)
# f <- exdf[complete.cases(exdf),]


