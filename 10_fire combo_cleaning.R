pacman::p_load(tidyverse, sf, foreach)
mtbs <- read.csv("Data/Processed/mtbs_l4.csv")
names(mtbs) <- c('US_L4CODE', 'Year', 'area_burned', 'area_eco_l4', 'prop_area_burned_l4')
mtbs <- mtbs %>% 
  filter(Year>1984 & Year<2024) %>% 
  group_by(US_L4CODE) %>%
  complete(Year = 1985:2023) %>% 
  mutate(type = 'MTBS')

mtbs_sum <- mtbs %>% 
  group_by(US_L4CODE) %>% 
  summarise(mx = max(area_burned))

# 'CNRM'
cnrm <- read.csv('Data/Processed/Fire_projections/Fire_projections_CNRM_l4.csv')
cnrm <- cnrm %>% 
  mutate(type = 'CNRM')

combo <- rbind(mtbs, cnrm)
combo <- left_join(combo, mtbs_sum) %>% 
  mutate(mx = ifelse(is.na(mx), 0, mx))

combo <- combo %>% 
  mutate(area_burned_mod = ifelse(mx>0 & area_burned>3*mx, 3*mx, area_burned))

combo <- combo %>% 
  mutate(area_burned_mod = ifelse(mx %in% 0 & area_burned_mod> 2500, 2500, area_burned_mod))
# test1 <- filter(combo, mx %in% 0)
# summary(test1$area_burned_mod)
# t1 <- filter(test1, area_burned_mod>2000)

# ss <- filter(t2,  `n()`>9)
# combo <- anti_join(combo, ss)

write.csv(combo, 'Data/Processed/Fire_projections/Fire_combo_CNRM_l4.csv', row.names = F)

#MRI
mri <- read.csv('Data/Processed/Fire_projections/Fire_projections_MRI_l4.csv')
mri <- mri %>% 
  mutate(type = 'mri')

combo <- rbind(mtbs, mri)
combo <- left_join(combo, mtbs_sum) %>% 
  mutate(mx = ifelse(is.na(mx), 0, mx))

combo <- combo %>% 
  mutate(area_burned_mod = ifelse(mx>0 & area_burned>3*mx, 3*mx, area_burned))

combo <- combo %>% 
  mutate(area_burned_mod = ifelse(mx %in% 0 & area_burned_mod> 200, 200, area_burned_mod))


write.csv(combo, 'Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv', row.names = F)

# HadGEM2-ES 

HadGEM2 <- read.csv('Data/Processed/Fire_projections/Fire_projections_HadGEM2-ES_l4.csv')
HadGEM2 <- HadGEM2 %>% 
  mutate(type = 'HadGEM2-ES')

combo <- rbind(mtbs, HadGEM2)
combo <- left_join(combo, mtbs_sum) %>% 
  mutate(mx = ifelse(is.na(mx), 0, mx))

combo <- combo %>% 
  mutate(area_burned_mod = ifelse(mx>0 & area_burned>3*mx, 3*mx, area_burned))

combo <- combo %>% 
  mutate(area_burned_mod = ifelse(mx %in% 0 & area_burned_mod> 150, 150, area_burned_mod))
# test1 <- filter(combo, mx %in% 0)
# summary(test1$area_burned_mod)
# t1 <- filter(test1, area_burned_mod>2000)


write.csv(combo, 'Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv')

