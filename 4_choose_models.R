pacman::p_load(tidyverse, foreach, stringr)

summaries <- list.files('Data/Processed/structures/Model/summaries', 
                           recursive = T, 
                           full.names = T)
summaries <- summaries[!grepl('land', summaries)]

select_model <- function(summaries){
  sums <- read.csv(summaries)
  preds <- read.csv(paste0('Data/Processed/structures/Model/predictions/', basename(summaries)))
  df <- data.frame(year = preds$year,
                 struct_model = preds$struct_model,
                 struct_valid = preds$struct_valid,
                 model_out = if(sums$aic_nb<sums$aic_poiss) {preds$fit_nb.x}else{preds$fit_poiss},
                 struct_se = if(sums$aic_nb<sums$aic_poiss) {preds$se_nb.x}else{preds$se_poiss},
                 id = gsub('.csv', '', basename(summaries)))
  return(df)
}



struct <- foreach(i = 1:length(summaries), .combine = rbind) %do% {
  ss <- select_model(summaries = summaries[i])
}

#Test

t3 <- pivot_wider(struct[,c(1, 4, 6)], names_from = year, values_from = model_out)

t3 <- t3 %>% 
  mutate(fifty = `2050`/`2020`,
         sixty = `2060`/`2020`) %>% 
  dplyr::select(id, fifty, sixty)

t4 <- t3 %>% 
  filter(fifty>3)
t5 <- inner_join(struct, t4) 
unique(t5$id)

#Change model for outliers

select_model_2 <- function(summaries){
  sums <- read.csv(summaries)
  preds <- read.csv(paste0('Data/Processed/structures/Model/predictions/', basename(summaries)))
  df <- data.frame(year = preds$year,
                   struct_model = preds$struct_model,
                   struct_valid = preds$struct_valid,
                   model_out = if(sums$aic_nb>sums$aic_poiss) {preds$fit_nb.x}else{preds$fit_poiss},
                   struct_se = if(sums$aic_nb>sums$aic_poiss) {preds$se_nb.x}else{preds$se_poiss},
                   id = gsub('.csv', '', basename(summaries)))
  return(df)
}

summaries_2 <- summaries[str_detect(summaries, paste(t5$id, collapse = "|"))]

struct_2 <- foreach(i = 1:length(summaries_2), .combine = rbind) %do% {
  ss <- select_model_2(summaries = summaries_2[i])
}

struct_1 <- struct %>% 
  filter(!id %in% unique(t5$id))

struct <- rbind(struct_1, struct_2)

t3 <- pivot_wider(struct[,c(1, 4, 6)], names_from = year, values_from = model_out)

t3 <- t3 %>% 
  mutate(fifty = `2050`/`2020`,
         sixty = `2060`/`2020`) %>% 
  dplyr::select(id, fifty, sixty)

t4 <- t3 %>% 
  filter(fifty>3)

struct_3 <- struct %>% 
  filter(!id %in% t4$id)
length(unique(struct_3$id))


#Complete info
eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>%
  mutate(eco_l4 = paste(US_L4CODE, Shape_Area, sep = '_'))
regions <- eco_shp %>% 
  mutate(region = str_extract_all(L1_KEY, "\\d+"))
regions <- regions %>% 
  mutate(region = ifelse(region %in% c(6, 7, 10, 11, 12, 13), 'West', 
                         ifelse(region %in% c(8, 15, 5), 'East', 
                                ifelse(region %in% c(9), 'Central', 'Double-check')))) 
regions <- regions %>% 
  st_drop_geometry() %>% 
  dplyr::select(US_L4CODE, L1_KEY, region)
names(regions)[1:2] <- c('eco_l4', 'eco_l1')

regions <- regions %>% 
  distinct()
struct_3 <- struct_3 %>% 
  mutate(eco_l4 = sub("_.*", "", id))
struct_3 <- left_join(struct_3, regions)
struct_3 <- struct_3 %>% 
  mutate(upper_str = model_out + struct_se*1.96,
         lower_str= model_out - struct_se*1.96)
struct_3 <- struct_3 %>% 
  mutate(lower_str = ifelse(lower_str<0, 0, lower_str))

write.csv(struct_3, 'Data/Processed/structures/Model/structures_complete.csv', row.names = F)

s1 <- struct_3 %>% 
  group_by(year) %>% 
  summarize(struct = sum(model_out))
filter(s1, year %in% c(2020, 2030, 2060))
head(s1)
