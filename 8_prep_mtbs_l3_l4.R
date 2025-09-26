pacman::p_load(tidyverse, sf)

eco_l4 <-st_read('Data/Raw/us_eco_l4_state_boundaries')
mtbs_sf <- st_read('Data/Raw/mtbs_perimeter_data 2') %>% 
  st_transform(st_crs(eco_l4))
mtbs_sf <- mtbs_sf %>% 
  mutate(area_burned_ha = as.numeric(st_area(.)*0.0001))
mtbs_sf <- mtbs_sf %>% 
  filter(Incid_Type %in% c('Wildfire', 'Unkown'))
mtbs_sf <- mtbs_sf %>% 
  mutate(year = year(Ig_Date))
mtbs <- mtbs_sf %>% 
  st_drop_geometry() %>% 
  select(Event_ID, year, area_burned_ha)
mtbs_pt <- st_read('Data/Raw/mtbs_fod_pts_data') %>% 
  st_transform(st_crs(eco_l4)) %>% 
  filter(Incid_Type %in% c('Wildfire', 'Unkown')) %>% 
  select(Event_ID, Ig_Date)
mtbs_eco <- st_join(mtbs_pt, eco_l4, join = st_within)
mtbs_eco <- mtbs_eco %>% 
  filter(!is.na(L1_KEY))

mtbs <- right_join(mtbs, mtbs_eco)
mtbs <- st_drop_geometry(mtbs) 

mtbs <- mtbs[,c(1:5, 7)]
write.csv(mtbs, "Data/Processed/mtbs_eco.csv", row.names = F)#data are in ha. This contains events

mtbs <- read.csv("Data/Processed/mtbs_eco.csv")
eco <- st_drop_geometry(eco_l4)
eco <- eco %>% 
  group_by(US_L4CODE) %>% 
  summarise(area_eco_l4 = sum(Shape_Area, na.rm = T)*0.0001)#to ha
eco3 <- eco_l4 %>% 
  group_by(US_L3CODE) %>% 
  summarise(area_eco_l3 = sum(Shape_Area, na.rm = T)*0.0001)#to ha
sum(eco$area_eco_l4)
sum(eco3$area_eco_l3)


mtbs <- mtbs[,1:6]

#Aggregate @ eco l4
mtbs_l4 <- mtbs %>% 
  group_by(US_L4CODE, year) %>% 
  summarise(area_burned_l4 = sum(area_burned_ha, na.rm = T))
mtbs_l4 <- mtbs_l4 %>% 
  ungroup() %>% 
  left_join(eco)

mtbs_l4 <- mtbs_l4 %>% 
  group_by(US_L4CODE) %>%
  complete(year = 1984:2024) %>%  # Fill in all years for each region
  arrange(US_L4CODE, year) %>%  # Order data for interpolation
  mutate(prop_area_burned_eco_l4 = area_burned_l4/area_eco_l4) %>%
  ungroup() 

mtbs_l4 <- mtbs_l4 %>% 
  mutate(area_burned_l4 = ifelse(is.na(area_burned_l4), 0, area_burned_l4),
         prop_area_burned_eco_l4 = ifelse(is.na(prop_area_burned_eco_l4), 0, prop_area_burned_eco_l4))
write.csv(mtbs_l4, "Data/Processed/mtbs_l4.csv", row.names = F)#data are in ha for eco l4
test1 <- mtbs_l4 %>% 
  summarise(area_burned_l4 = sum(area_burned_l4, na.rm = T))

#Eco l3
mtbs_l3 <- mtbs %>% 
  group_by(US_L3CODE, year) %>% 
  summarise(area_burned_l3 = sum(area_burned_ha, na.rm = T))
mtbs_l3 <- mtbs_l3 %>% 
  mutate(US_L3CODE = as.character(US_L3CODE)) %>% 
  ungroup() %>% 
  left_join(eco3)

mtbs_l3 <- mtbs_l3 %>% 
  group_by(US_L3CODE) %>%
  complete(year = 1984:2024) %>%  # Fill in all years for each region
  arrange(US_L3CODE, year) %>%  # Order data for interpolation
  mutate(prop_area_burned_eco_l3 = area_burned_l3/area_eco_l3) %>%
  ungroup() 

mtbs_l3 <- mtbs_l3 %>% 
  mutate(area_burned_l3 = ifelse(is.na(area_burned_l3), 0, area_burned_l3),
         prop_area_burned_eco_l3 = ifelse(is.na(prop_area_burned_eco_l3), 0, prop_area_burned_eco_l3))
mtbs_l3 <- st_drop_geometry(mtbs_l3)
mtbs_l3 <- mtbs_l3 %>% 
  select(-geometry)
test2 <- mtbs_l3 %>% 
  summarise(area_burned_l3 = sum(area_burned_l3, na.rm = T))
write.csv(mtbs_l3, "Data/Processed/mtbs_l3.csv", row.names = F)#data are in ha for eco l3

# filter(mtbs_l4, year %in% 2000) %>% 
#   summarise(sum(area_burned_l4))
