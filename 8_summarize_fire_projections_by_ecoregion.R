# =============================================================================
# Script: summarize_fire_projections_by_ecoregion.R
# Description:
#   Summarizes projected annual burned area within Level IV ecoregions for a
#   selected climate model by reading processed fire projections or extracting
#   raw projection files, converting burned area from acres to hectares, and
#   calculating the proportion of each ecoregion projected to burn per year.
#   The workflow writes annual burned-area summaries and ecoregion-normalized
#   fire projections for downstream comparison with historical MTBS records.
# Inputs:
#   Data/Raw/Fire_projections/<model>/;
#   Data/Raw/us_eco_l4_state_boundaries
# Outputs:
#   Data/Processed/Fire_projections/Fire_projections_mtbs_<model>.csv;
#   Data/Processed/Fire_projections/Fire_projections_<model>_l4.csv
# =============================================================================

# Load required packages ---------------------------------------------------
pacman::p_load(tidyverse, sf, foreach)


# Select climate model ( 'CNRM', 'MRI', 'HadGEM2-ES')-----------------------------------------------------

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
