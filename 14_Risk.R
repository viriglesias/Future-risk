pacman::p_load(tidyverse, sf)

fire <- read.csv('Data/Processed/Fire_projections/fire_full.csv')
fire <- fire %>% 
  select(US_L4CODE, Year, type, prop_area_burned_mod)

houses <- read.csv('Data/Processed/structures/Model/structures_complete.csv') 
houses <- houses %>% 
  filter(year>1984) %>% 
  select(id, eco_l4, year, model_out)

combo <- right_join(fire, houses, by = c('US_L4CODE' = 'eco_l4', 'Year' = 'year'))
names(combo)[c(4, 6)] <- c('prop_area_burned', 'structures')
damage_l <- read.csv('/Users/viig7608/Desktop/Fire risk/Data/Processed/prob_burning_adj.csv')
eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>%
  mutate(id = paste(US_L4CODE, Shape_Area, sep = '_'))

cw <- eco_shp %>% 
  st_drop_geometry() %>% 
  mutate(region = str_extract_all(L1_KEY, "\\d+"))
cw <- cw %>% 
  mutate(region = ifelse(region %in% c(6, 7, 10, 11, 12, 13), 'West', 
                         ifelse(region %in% c(8, 15, 5), 'East', 
                                ifelse(region %in% c(9), 'Central', 'Double-check')))) 
cw <- cw %>% 
  select(id, US_L3CODE, L1_KEY, region, Shape_Area)

combo <- left_join(cw, combo)

combo <- combo %>% 
  mutate(prop_area_burned = ifelse(is.na(prop_area_burned), 0, prop_area_burned),
         structures = ifelse(is.na(structures), 0, structures))
combo <- combo %>% 
  distinct()

combo <- combo %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE)) %>% 
  left_join(damage_l[,c(2,5)] )
combo <- combo %>% 
  filter(!is.na(US_L4CODE) & !is.na(damaged_ratio_adj_median)) %>% 
  mutate(risk = prop_area_burned*damaged_ratio_adj_median,
         str_destroyed = prop_area_burned*structures*damaged_ratio_adj_median)
combo <- combo %>% 
  mutate(type = ifelse(is.na(type) & Year<2024, 'MTBS', type))
mv <- filter(combo, is.na(type))#59 unique id's
mv <- filter(mv,  structures>0 & damaged_ratio_adj_median>0)#18 unique id's
excl <- unique(mv$id)
combo <- filter(combo, !(id %in% excl))
unique(combo$type)
combo <- filter(combo, !is.na(type))
write.csv(combo, '/Users/viig7608/Desktop/Fire risk/Data/Processed/risk.csv', row.names = F)
