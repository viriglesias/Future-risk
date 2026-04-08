# =============================================================================
# Script: summarize_BUPR_by_ownership_and_ecoregion.R
# Purpose:
#   Summarize annual BUPR structure counts by land ownership / exclusion class
#   (federal, state, tribal, local, joint, water, private) within Level IV
#   ecoregions across the conterminous United States.
#
#   The script overlays annual BUPR rasters with the exclusion raster produced
#   in the previous step, then aggregates structure counts by exclusion class
#   within each Level IV ecoregion polygon.
#
# Inputs required:
#   1. Data/Raw/BUPR/BUPR/
#      - Directory containing annual BUPR raster files.
#      - In this script, files 27:43 from the recursive file listing are used.
#      - These rasters are assumed to represent annual structure counts.
#
#   2. Data/Processed/exclude.tif
#      - Exclusion raster produced by the previous script.
#      - Encodes land ownership / exclusion classes:
#          1 = Federal
#          2 = Joint
#          3 = Local
#          4 = State
#          5 = Tribal
#          6 = Water
#         NA = Private
#
#   3. Data/Raw/us_eco_l4_state_boundaries
#      - Vector layer of U.S. Level IV ecoregion boundaries.
#      - Used to spatially summarize BUPR values by ecological region.
#
# Outputs produced:
#   - One CSV file per Level IV ecoregion written to:
#       Data/Processed/structures_ts/
#   - Each CSV contains annual structure totals by ownership / exclusion class
#     for that ecoregion.
#
# Notes:
#   - Only polygons at least as large as 4 raster cells are retained.
#   - The script currently processes only BUPR.
#   - The code assumes annual information is encoded in raster layer names.
# =============================================================================

# Load required packages ---------------------------------------------------
# tidyverse: data manipulation
# raster: raster processing
# sf: vector processing
# foreach / doParallel: looping infrastructure
pacman::p_load(tidyverse, raster, sf, foreach, doParallel)

# Specify land-use type(s) to process --------------------------------------
# Here only BUPR is processed.
lu <- c('BUPR')

# Identify input BUPR raster files -----------------------------------------
# Input:
#   Data/Raw/BUPR/BUPR/
# Purpose:
#   Recursively list annual BUPR raster files and select a subset.
# Note:
#   The [27:43] indexing assumes the desired files are always in those
#   positions; this should ideally be replaced with explicit filename matching.
tifs <- list.files(
  paste0('Data/Raw/BUPR/', lu),
  recursive = TRUE,
  full.names = TRUE
)[27:43]

# Loop over land-use categories --------------------------------------------
foreach(d = 1:length(lu)) %do% {

  # Define output directory based on land-use code.
  # For BUPR, output is written to:
  #   Data/Processed/structures_ts
  directory <- paste0(
    'Data/Processed/',
    ifelse(lu[d] %in% 'A', 'agriculture',
           ifelse(lu[d] %in% 'C', 'commercial',
                  ifelse(lu[d] %in% 'RO', 'residowned',
                         ifelse(lu[d] %in% 'RI', 'residincome',
                                ifelse(lu[d] %in% 'BUPR', 'structures',
                                       'Double-check'))))),
    '_ts'
  )

  # Create output directory if needed.
  if(!dir.exists(directory)){
    dir.create(directory)
  }

  # Load annual BUPR rasters as a stack.
  # Each layer is assumed to correspond to a different year.
  tif_stack <- stack(tifs)

  # Load exclusion raster --------------------------------------------------
  # Input:
  #   Data/Processed/exclude.tif
  # Purpose:
  #   Provides the ownership / exclusion class assigned to each raster cell.
  dlu_r <- raster('Data/Processed/exclude.tif')

  # Load and prepare Level IV ecoregions ----------------------------------
  # Input:
  #   Data/Raw/us_eco_l4_state_boundaries
  # Purpose:
  #   Used to summarize BUPR raster values within each ecoregion.
  eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>% 
    st_transform(crs(dlu_r)) %>%
    mutate(ID = paste(L1_KEY, US_L4CODE, Shape_Area, sep = '_')) %>% 
    filter(Shape_Area >= 250 * 250 * 4) # retain polygons >= 4 raster cells

  # Define helper function -------------------------------------------------
  # Purpose:
  #   For one annual BUPR raster and one ecoregion polygon, crop/mask both the
  #   annual raster and the exclusion raster, combine them, assign ownership
  #   classes, and aggregate structure counts by class.
  read_raster <- function(rast, shp, df){

    # Crop and mask the exclusion raster to the ecoregion polygon.
    df <- df %>% 
      crop(shp) %>% 
      mask(shp)
    df <- as.data.frame(df)

    # Crop and mask the annual BUPR raster to the same ecoregion polygon.
    r <- rast %>% 
      crop(shp) %>% 
      mask(shp)
    r <- as.data.frame(r)

    # Combine annual structure values with exclusion-class values.
    r <- cbind(r, df)

    # Extract year from raster layer name.
    # Assumes the first part of the layer name contains the year.
    yr <- gsub('X', '', str_split_fixed(names(r)[1], '_', 2)[1,1])

    # Rename first column to reflect BUPR structure counts.
    names(r)[1] <- 'structures'

    # Remove cells with missing structure values.
    r <- r[!is.na(r$structures),]

    # Recode exclusion raster values into descriptive ownership classes.
    r <- r %>% 
      mutate(
        dlu = ifelse(exclude %in% 1, 'Federal',
              ifelse(exclude %in% 2, 'Joint',
              ifelse(exclude %in% 3, 'Local',
              ifelse(exclude %in% 4, 'State',
              ifelse(exclude %in% 5, 'Tribal',
              ifelse(exclude %in% 6, 'Water',
              ifelse(is.na(exclude), 'Private', 'Double-check')))))))
      )

    # Aggregate total structures by ownership / exclusion class.
    r_sum <- r %>% 
      group_by(dlu) %>% 
      summarise(structures = sum(structures))

    # Add ecoregion identifiers and year.
    eco_l4_id <- shp$ID
    eco_l1 <- shp$L1_KEY

    r_sum <- r_sum %>% 
      mutate(
        year = as.numeric(yr),
        eco_l4_id = eco_l4_id,
        eco_l1 = eco_l1
      )

    return(r_sum)
  }

  # Process each ecoregion -------------------------------------------------
  # For each Level IV ecoregion polygon, apply the helper function to every
  # annual raster layer, combine the yearly summaries, and write one CSV.
  foreach(j = 1:nrow(eco_shp)) %do% {

    stack_processed <- lapply(1:nlayers(tif_stack), function(i) {
      read_raster(tif_stack[[i]], eco_shp[j,], dlu_r)
    })

    stack_df <- bind_rows(stack_processed)

    # Output:
    #   One CSV per ecoregion, named using Level I and Level IV identifiers.
    write.csv(
      stack_df,
      paste0(directory, '/', stack_df[1,5], '_', stack_df[1,4], '.csv'),
      row.names = FALSE
    )
  }
}

# Clean up workspace -------------------------------------------------------
rm(list = ls())
gc()


