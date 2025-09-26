pacman::p_load(tidyverse, sf, foreach, doParallel, raster, fasterize, exactextractr)

eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>% 
  mutate(ID = paste(L1_KEY, US_L4CODE, Shape_Area, sep = '_')) %>% 
  filter(Shape_Area>= 250*250*4)#Only keep polygons equal to or larger than 4 pixels

pop <- st_read('Data/Raw/ICLUS_v2_1_1_population.gdb') %>% 
  st_transform(st_crs(eco_shp)) %>%
  mutate(area = as.numeric(st_area(.)))

r_template <- raster(extent(eco_shp), resolution = 250)
crs(r_template) <- st_crs(eco_shp)$proj4string


# Separate geometry and attributes
geom <- st_geometry(pop)

pop_mod <- pop %>% 
  st_drop_geometry() %>% 
  dplyr::select("TOTALPOP90": "SSP52100") %>%
  mutate(across(everything(), ~ .*250*250/pop$area)) 

# Convert back to sf object
pop_mod <- st_sf(pop_mod, geometry = geom)

field <- names(pop_mod)[1:(length(names(pop_mod))-1)]

stack_pop <- foreach(i=1:length(field))%do%{
  fasterize(pop_mod, r_template, field = field[i], fun = 'max')
}

stack_pop <- raster::stack(stack_pop)
names(stack_pop) <- field

pop_mean <- foreach(i = 1:nrow(eco_shp), .combine = rbind)%do%{
  cbind(ID = eco_shp$ID[i], exact_extract(stack_pop, eco_shp[i,], c('sum')))
}

#Check results
colSums(st_drop_geometry(pop[,5:61]))/colSums(pop_mean[,-1])
write.csv(pop_mean, 'Data/Processed/Population_summaries.csv')

