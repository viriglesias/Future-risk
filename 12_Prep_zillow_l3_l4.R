pacman::p_load(tidyverse, foreach)

# Option A
get_e3 <- function(x){
  eco3 <- str_split_fixed(x, '_', 3)[1,2] %>% 
    gsub("[^0-9]", "", .)
  return(eco3)
}

zillow_files <- list.files('Data/Processed/structures_ts/',
                           recursive = T,
                           full.names = T)

zillow <- foreach(i = 1:length(zillow_files), .combine = rbind) %do% {
  zill <- read.csv(zillow_files[[i]])
  zill <- filter(zill, !is.na(structures))
  zill <- zill %>% 
    filter(year > 1983 & dlu %in% 'Private') %>% 
    group_by(year, eco_l4_id) %>% 
    summarize(structures = sum(structures, na.rm = T)) %>% 
    mutate(US_L3CODE = tryCatch(get_e3(eco_l4_id), error = function(e) NA_character_))
}

#Option A: L3
zillow_sum_l3 <- zillow %>% 
  group_by(year, US_L3CODE) %>% 
  summarize(structures = sum(structures))

zillow_int_l3 <- zillow_sum_l3 %>%
  group_by(US_L3CODE) %>%
  complete(year = 1984:2023) %>%  # Fill in all years for each region
  arrange(US_L3CODE, year) %>%  # Order data for interpolation
  group_by(US_L3CODE) %>%
  mutate(structures = approx(year, structures, xout = year, rule = 2)$y) %>%
  ungroup() 
names(zillow_int_l3)[2] <- 'START_YEAR'  
write.csv(zillow_int_l3, "Data/Processed/zillow_l3_1.csv", row.names = F)

#Option A: L4
zillow_sum_l4 <- zillow %>% 
  ungroup() %>% 
  mutate(US_L4CODE = sapply(eco_l4_id, function(x){str_split_fixed(x, '_', 3)[1,2]}))
  
zillow_sum_l4 <- zillow_sum_l4 %>%   
  group_by(year, US_L4CODE) %>% 
  summarize(structures = sum(structures))

zillow_int_l4 <- zillow_sum_l4 %>%
  group_by(US_L4CODE) %>%
  complete(year = 1984:2023) %>%  # Fill in all years for each region
  arrange(US_L4CODE, year) %>%  # Order data for interpolation
  group_by(US_L4CODE) %>%
  mutate(structures = approx(year, structures, xout = year, rule = 2)$y) %>%
  ungroup() 
names(zillow_int_l4)[2] <- 'START_YEAR'  
write.csv(zillow_int_l4, "Data/Processed/zillow_l4_1.csv", row.names = F)

#Option B
zill <- read.csv('Data/Processed/Summaries/structures.csv')
zill <- filter(zill, !is.na(struct_training) | !is.na(struct_valid))
zill <- filter(zill, year>1983)
zill <- zill %>% 
  mutate(US_L4CODE = sapply(eco_l4, function(x) str_split_fixed(x, '_', 2)[1,1]),
         US_L3CODE = sapply(US_L4CODE, function(x){gsub("[^0-9]", "", x)}),
         structures = ifelse(is.na(struct_training), struct_valid, struct_training),
         area_occ = ifelse(is.na(exps_training), exps_valid, exps_training),
         str_density = 10000*structures/area_occ) #to str per ha
zill$str_density[is.na( zill$str_density)] <- 0 #Fix 0/0

#Option B: L3
zill_l3 <- zill %>% 
  group_by(year, US_L3CODE) %>% 
  summarise(structures = sum(structures),
            area_occ = sum(area_occ),
            str_density = median(str_density))
zillow_int_l3 <- zill_l3 %>%
  group_by(US_L3CODE) %>%
  complete(year = 1984:2023) %>%  # Fill in all years for each region
  arrange(US_L3CODE, year) %>%  # Order data for interpolation
  group_by(US_L3CODE) %>%
  mutate(structures = approx(year, structures, xout = year, rule = 2)$y,
         area_occ = approx(year, area_occ, xout = year, rule = 2)$y,
         str_density = approx(year, str_density, xout = year, rule = 2)$y) %>%
  ungroup()
names(zillow_int_l3)[2] <- 'START_YEAR'
write.csv(zillow_int_l3, "Data/Processed/zillow_l3_2.csv", row.names = F)

#Option B: L4
zill_l4 <- zill %>% 
  group_by(year, US_L4CODE) %>% 
  summarise(structures = sum(structures),
            area_occ = sum(area_occ),
            str_density = median(str_density))
zillow_int_l4 <- zill_l4 %>%
  group_by(US_L4CODE) %>%
  complete(year = 1984:2023) %>%  # Fill in all years for each region
  arrange(US_L4CODE, year) %>%  # Order data for interpolation
  group_by(US_L4CODE) %>%
  mutate(structures = approx(year, structures, xout = year, rule = 2)$y,
         area_occ = approx(year, area_occ, xout = year, rule = 2)$y,
         str_density = approx(year, str_density, xout = year, rule = 2)$y) %>%
  ungroup()
names(zillow_int_l4)[2] <- 'START_YEAR'
write.csv(zillow_int_l4, "Data/Processed/zillow_l4_2.csv", row.names = F)


#Test
l31 <- read.csv("Data/Processed/zillow_l3_1.csv")
l32 <- read.csv("Data/Processed/zillow_l3_2.csv")
l3 <- full_join(l31, l32, by = c('US_L3CODE', 'START_YEAR'))
l3_diff <- l3 %>% 
  mutate(l3_diff = structures.x-structures.y)
range(l3_diff$l3_diff)
if(max(l3_diff$l3_diff)>0){print('Double-check')}


l41 <- read.csv("Data/Processed/zillow_l4_1.csv")
l42 <- read.csv("Data/Processed/zillow_l4_2.csv")
l4 <- full_join(l41, l42, by = c('US_L4CODE', 'START_YEAR'))
l4_diff <- l4 %>% 
  mutate(l4_diff = structures.x-structures.y)
range(l4_diff$l4_diff)
if(max(l4_diff$l4_diff)>0){print('Double-check')}

l3_b <- l4 %>% 
  mutate(US_L3CODE = as.numeric(sapply(US_L4CODE, function(x){gsub("[^0-9]", "", x)})))
  
l3_b <- l3_b %>% 
  group_by(US_L3CODE, START_YEAR) %>% 
  summarize(struc = sum(structures.x))

l3 <- full_join(l3, l3_b)
l3_diff <- l3 %>% 
  mutate(l3diff = structures.x-struc)
a <- filter(l3_diff, l3diff !=0)

