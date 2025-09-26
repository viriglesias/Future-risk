pacman::p_load(tidyverse, sf, foreach)
#Models='CNRM', 'MRI', 'HadGEM2-ES'
model <- 'HadGEM2-ES'
if(file.exists(paste0('Data/Processed/Fire_projections/Fire_projections_', model, '.csv'))){
  fire <- read.csv(paste0('Data/Processed/Fire_projections/Fire_projections_', model, '.csv'))
}else{
  # Create the output directory if it does not exist
  if (!dir.exists(paste0('Data/Raw/Fire_projections/', model, '_unz'))) {
    dir.create(paste0('Data/Raw/Fire_projections/', model, '_unz'))
  }
  
  # Get a list of all .zip files in the input directory
  zip_files <- list.files(paste0('Data/Raw/Fire_projections/', model, '/'), pattern = "\\.zip$", full.names = TRUE)
  
  # Loop through each zip file and extract it
  for (zip_file in zip_files) {
    # Extract the base name of the zip file (without extension)
    zip_base_name <- tools::file_path_sans_ext(basename(zip_file))
    unzip(zip_file, exdir = paste0('Data/Raw/Fire_projections/', model, '_unz'))
  }
  
  files <- list.files(paste0('Data/Raw/Fire_projections/', model, '_unz'), full.names = T)
  
  fire <- foreach(i = 1:(length(files)-1), .combine = rbind) %do% {
    if(grepl('rds', files[[i]])){
      f <- readRDS(files[[i]]) %>% 
        st_drop_geometry()
    }else{
      f <- st_read(files[[i]]) %>% 
        st_drop_geometry()
      } 
    f <- f %>% 
      group_by(US_L4CODE, Year) %>% 
      summarize(area_burned = 0.404686*mean(En_Area))#acres to ha
  }
  fire <- ungroup(fire)
  write.csv(fire, paste0('Data/Processed/Fire_projections/Fire_projections_mtbs_', model, '.csv'), row.names = F)  
}

eco_st <-st_read('Data/Raw/us_eco_l4_state_boundaries')
eco <- st_drop_geometry(eco_st)
eco_l4 <- eco %>% 
  group_by(US_L4CODE) %>% 
  summarise(area_eco_l4 = sum(Shape_Area, na.rm = T)*0.0001) %>% #to ha
  ungroup()
fire <- fire %>% 
  left_join(eco_l4)
fire <- fire %>% 
  mutate(prop_area_burned_l4 = area_burned/area_eco_l4)

write.csv(fire, paste0('Data/Processed/Fire_projections/Fire_projections_', model, '_l4.csv'), row.names = F)

