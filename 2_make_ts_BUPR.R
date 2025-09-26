#Exclude federal, state, and tribal lands from BUPR data
pacman::p_load(tidyverse, raster, sf, foreach, doParallel)

lu <- c('BUPR')

tifs <- list.files( paste0('Data/Raw/BUPR/', lu),
                    recursive = TRUE,
                    full.names = TRUE)[27:43]


foreach(d = 1:length(lu))%do%{
  directory <- paste0('Data/Processed/', 
                      ifelse(lu[d] %in% 'A', 'agriculture', 
                             ifelse(lu[d] %in% 'C', 'commercial',
                                    ifelse(lu[d] %in% 'RO','residowned',
                                           ifelse(lu[d] %in% 'RI', 'residincome',
                                                  ifelse(lu[d] %in% 'BUPR', 'structures',
                                                         'Double-check'))))), '_ts')
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  
  tif_stack <- stack(tifs)
  
  dlu_r <- raster('Data/Processed/exclude.tif')
  
  eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>% 
    st_transform(crs(dlu_r)) %>%
    mutate(ID = paste(L1_KEY, US_L4CODE, Shape_Area, sep = '_')) %>% 
    filter(Shape_Area>= 250*250*4)#Only keep polygons equal to or larger than 4 pixels
  
  read_raster <- function(rast, shp, df){
    df <- df %>% 
      crop(shp) %>% 
      mask(shp)
    df <- as.data.frame(df)
    # r <- raster(rast) 
    r <- rast %>% 
      crop(shp) %>% 
      mask(shp)#Crop the grid
    r <- as.data.frame(r)
    r <- cbind(r, df)
    yr <- gsub('X', '', str_split_fixed(names(r)[1], '_', 2)[1,1])
    names(r)[1] <- 'structures'
    r <- r[!is.na(r$structures),]
    r <- r %>% 
      mutate(dlu = ifelse(exclude %in% 1, 'Federal', 
                          ifelse(exclude %in% 2, 'Joint',
                                 ifelse(exclude %in% 3, 'Local',
                                        ifelse(exclude %in% 4, 'State',
                                               ifelse(exclude %in% 5, 'Tribal',
                                                      ifelse(exclude %in% 6, 'Water',
                                                             ifelse(is.na(exclude), 'Private', 'Double-check'))))))))
    r_sum <- r %>% 
      group_by(dlu) %>% 
      summarise(structures = sum(structures))
    eco_l4_id <- shp$ID
    eco_l1 <- shp$L1_KEY
    r_sum <- r_sum %>% 
      mutate(year = as.numeric(yr),
             eco_l4_id = eco_l4_id,
             eco_l1 = eco_l1)
    return(r_sum)
  }
  # Apply the function to each layer
  
  foreach(j = 1:nrow(eco_shp))%do%{
    stack_processed <- lapply(1:nlayers(tif_stack), function(i) {
      read_raster(tif_stack[[i]], eco_shp[j,], dlu_r)
    })
    stack_df <- bind_rows(stack_processed)
    write.csv(stack_df, paste0(directory, '/', stack_df[1,5], '_', stack_df[1,4], '.csv'), row.names = F)
  }
}

rm(list=ls())
gc()


