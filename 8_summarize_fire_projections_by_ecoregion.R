# =============================================================================
# Script: summarize_fire_projections_by_ecoregion.R
#
# Purpose:
#   Read projected fire-area datasets for a selected climate model, extract or
#   load annual burned-area summaries by Level IV ecoregion, convert burned
#   area from acres to hectares, calculate the proportion of each ecoregion
#   projected to burn per year, and write the summarized outputs to disk.
#
# Inputs required:
#   1. Data/Raw/Fire_projections/<model>/
#      - Directory containing zipped fire-projection files for one climate
#        model.
#      - The code assumes these zip files contain either:
#          a. RDS files, or
#          b. vector spatial files readable by sf::st_read()
#      - Each file must contain at least:
#          US_L4CODE
#          Year
#          En_Area   (burned area in acres)
#
#   2. Data/Raw/us_eco_l4_state_boundaries
#      - Vector layer of U.S. Level IV ecoregion boundaries.
#      - Used to calculate total ecoregion area in hectares.
#
# Outputs produced:
#   1. Data/Processed/Fire_projections/Fire_projections_mtbs_<model>.csv
#      - Annual projected burned area (ha) by Level IV ecoregion.
#
#   2. Data/Processed/Fire_projections/Fire_projections_<model>_l4.csv
#      - Annual projected burned area and proportion burned by Level IV
#        ecoregion.
#
# Notes:
#   - Burned area is converted from acres to hectares using:
#         1 acre = 0.404686 ha
#   - The script first checks whether a processed file already exists.
#     If it does, it reads that file; otherwise it unzips and processes the raw
#     fire-projection files.
# =============================================================================


# Load required packages ---------------------------------------------------

# tidyverse: data manipulation
# sf: vector data handling
# foreach: iterative processing
pacman::p_load(tidyverse, sf, foreach)


# Select climate model -----------------------------------------------------

# Available examples noted in the original code:
#   'CNRM', 'MRI', 'HadGEM2-ES'
model <- 'HadGEM2-ES'


# Load existing processed fire projections if available --------------------

if(file.exists(paste0('Data/Processed/Fire_projections/Fire_projections_', model, '.csv'))){

  fire <- read.csv(
    paste0('Data/Processed/Fire_projections/Fire_projections_', model, '.csv')
  )

} else {

  # Create output directory for extracted raw files if needed --------------
  if(!dir.exists(paste0('Data/Raw/Fire_projections/', model, '_unz'))){
    dir.create(paste0('Data/Raw/Fire_projections/', model, '_unz'))
  }

  # Identify zipped raw projection files -----------------------------------
  zip_files <- list.files(
    paste0('Data/Raw/Fire_projections/', model, '/'),
    pattern = "\\.zip$",
    full.names = TRUE
  )

  # Extract all zip files --------------------------------------------------
  for(zip_file in zip_files){

    # Base filename is computed but not used later in the original script
    zip_base_name <- tools::file_path_sans_ext(basename(zip_file))

    unzip(
      zip_file,
      exdir = paste0('Data/Raw/Fire_projections/', model, '_unz')
    )
  }

  # List extracted files ---------------------------------------------------
  files <- list.files(
    paste0('Data/Raw/Fire_projections/', model, '_unz'),
    full.names = TRUE
  )

  # Read and summarize extracted files ------------------------------------
  # Purpose:
  #   For each extracted file:
  #     1. read the data
  #     2. drop geometry
  #     3. group by ecoregion and year
  #     4. calculate projected burned area in hectares
  #
  # Note:
  #   The original loop uses 1:(length(files)-1), which skips the last file.
  #   That behavior is preserved here because it appears in your source code.
  fire <- foreach(i = 1:(length(files) - 1), .combine = rbind) %do% {

    if(grepl('rds', files[[i]])){
      f <- readRDS(files[[i]]) %>%
        st_drop_geometry()
    } else {
      f <- st_read(files[[i]]) %>%
        st_drop_geometry()
    }

    f <- f %>%
      group_by(US_L4CODE, Year) %>%
      summarize(area_burned = 0.404686 * mean(En_Area))  # acres to ha
  }

  fire <- ungroup(fire)

  # Save intermediate processed projections --------------------------------
  write.csv(
    fire,
    paste0('Data/Processed/Fire_projections/Fire_projections_mtbs_', model, '.csv'),
    row.names = FALSE
  )
}


# Read ecoregion boundaries -----------------------------------------------

# Input:
#   Data/Raw/us_eco_l4_state_boundaries
# Purpose:
#   Calculate total area of each Level IV ecoregion in hectares so projected
#   burned area can be converted to proportion burned.
eco_st <- st_read('Data/Raw/us_eco_l4_state_boundaries')

eco <- st_drop_geometry(eco_st)

eco_l4 <- eco %>%
  group_by(US_L4CODE) %>%
  summarise(area_eco_l4 = sum(Shape_Area, na.rm = TRUE) * 0.0001) %>%  # to ha
  ungroup()


# Join ecoregion area and calculate proportion burned ---------------------

fire <- fire %>%
  left_join(eco_l4)

fire <- fire %>%
  mutate(prop_area_burned_l4 = area_burned / area_eco_l4)


# Write final output -------------------------------------------------------

write.csv(
  fire,
  paste0('Data/Processed/Fire_projections/Fire_projections_', model, '_l4.csv'),
  row.names = FALSE
)
