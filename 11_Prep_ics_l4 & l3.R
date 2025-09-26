pacman::p_load(tidyverse, sf)
ics <- read.csv('/Users/viig7608/Desktop/Fire risk/Data/Raw/ics209-plus-wf_incidents_1999to2023.csv')[,-1]
ics <- ics %>% 
  # Remove some known "problem fires"
  filter(INCIDENT_ID != "2017_7211255_REDWOOD VALLEY INCIDENT",
         INCIDENT_ID != "2017_7293073_REDWOOD VALLEY INCIDENT") %>%
  # Filter incidents with 0 acres
  filter(FINAL_ACRES!=0) %>% 
  filter( START_YEAR > '1999')
ics <- ics %>% 
  filter(!POO_STATE %in% c('AK', 'HI', 'PR'))
ics <- ics %>% 
  filter(!is.na(POO_LATITUDE) & !is.na(POO_LONGITUDE))
ics <- filter(ics, STR_DESTROYED_TOTAL>0 |STR_DAMAGED_TOTAL>0)
ics_sf <- st_as_sf(ics, coords = c("POO_LONGITUDE", "POO_LATITUDE"), crs = 4326)  # Set CRS to WGS 84
eco_l4 <-st_read('Data/Raw/us_eco_l4_state_boundaries')
ics_sf <- st_transform(ics_sf, crs = st_crs(eco_l4))
ics_eco <- st_join(ics_sf, eco_l4, join = st_within)
ics_eco_df <- st_drop_geometry(ics_eco)

#Find problems
pr <- filter(ics_eco_df, is.na(US_L4CODE))
pr <- pr %>% 
  arrange(POO_STATE) %>% 
  select(POO_STATE, INCIDENT_NAME, START_YEAR, POO_COUNTY, POO_SHORT_LOCATION_DESC, STR_DAMAGED_TOTAL, STR_DESTROYED_TOTAL)
pr <- pr %>% 
  filter(!INCIDENT_NAME %in% c('STUART CREEK #2', 'POINT MACKENZIE', 'MOORE CREEK'))#in AK
ics_eco_df <- ics_eco_df %>% 
  filter(!INCIDENT_NAME %in% c('STUART CREEK #2', 'POINT MACKENZIE', 'MOORE CREEK'))#in AK
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '23c']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'PUMPKIN'] <- 23
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'PUMPKIN'] <- '23c'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '6ae']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'MANTER'] <- 6
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'MANTER'] <- '6ae'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '6o']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'TAM'] <- 6
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'TAM'] <- '6o'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '75k']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'Reserve'] <- 75
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'Reserve'] <- '75k'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '75d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'PERSIMMONS'] <- 75
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'PERSIMMONS'] <- '75d'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '50d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'Horne'] <- 50
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'Horne'] <- '50d'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '50b']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'ST. CROIX FIRE'] <- 50
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'ST. CROIX FIRE'] <- '50b'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '50o']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'VAN BUREN'] <- 50
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'VAN BUREN'] <- '50o'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '51h']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'River Fire'] <- 51
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'River Fire'] <- '51h'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '65q']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'cr 4136'] <- 65
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'cr 4136'] <- '65q'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '65d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'Palmer Road'] <- 65
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'Palmer Road'] <- '65d'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '43n']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'PEASE FIRE'] <- 43
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'PEASE FIRE'] <- '43n'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '63g']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'MOON TILLETT FIRE'] <- 63
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'MOON TILLETT FIRE'] <- '63g'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '21d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'CERRO GRANDE'] <- 21
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'CERRO GRANDE'] <- '21d'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '26d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'BELL'] <- 26
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'BELL'] <- '26d'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '21d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'VIVEASH'] <- 21
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'VIVEASH'] <- '21d'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '13m']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'NORTHEAST ELKO COUNTY ZONE'] <- 13#double-check
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'NORTHEAST ELKO COUNTY ZONE'] <- '13m'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '27i']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'BAILER'] <- 27
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'BAILER'] <- '27i'
ics_eco_df$US_L3NAME[ics_eco_df$US_L3CODE ==  '34']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'GRANJENO'] <- 34
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'GRANJENO'] <- '34f'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '27i']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'BAILER'] <- 27
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'BAILER'] <- '27i'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '10m']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'GOODNOE FIRE'] <- 10
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'GOODNOE FIRE'] <- '10m'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '10e']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'COMMAND 24'] <- 10
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'COMMAND 24'] <- '10e'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '50a']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'SUPERIOR'] <- 50
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'SUPERIOR'] <- '50a'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '51h']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'CARLOS EDGE'] <- 51
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'CARLOS EDGE'] <- '51h'
ics_eco_df$US_L4NAME[ics_eco_df$US_L4CODE ==  '65d']
ics_eco_df$US_L3CODE[ics_eco_df$INCIDENT_NAME %in% 'Attala'] <- 65
ics_eco_df$US_L4CODE[ics_eco_df$INCIDENT_NAME %in% 'Attala'] <- '65d'
write.csv(ics_eco_df, "Data/Processed/ics_l3-_l4_full.csv", row.names = F)

compl_T <- filter(ics_eco_df, COMPLEX %in% 'True') %>%
  filter(!is.na(INCIDENT_NAME))
compl_F <- filter(ics_eco_df, !(COMPLEX %in% 'True')) %>%
  filter(!is.na(INCIDENT_NAME))
# combo <- left_join(compl_T[,c(1:3, 5, 9, 22, 26, 29, 34, 50, 60, 24)], compl_F[,c(1:3, 5, 9, 22, 26, 29, 34, 50, 60, 24)], by = c('START_YEAR', 'US_L4CODE'))
# combo <- filter(combo, !is.na(STR_DESTROYED_TOTAL.y) & !is.na(STR_DAMAGED_TOTAL.y))
# combo <- filter(combo, STR_DESTROYED_TOTAL.y>0 | STR_DAMAGED_TOTAL.y>0)
# combo <- combo %>% 
#   filter(FINAL_ACRES.x>FINAL_ACRES.y)
# combo <- combo %>% 
#   filter(STR_DESTROYED_TOTAL.x>=STR_DESTROYED_TOTAL.y)
# combo <- combo %>% 
#   filter(STR_DAMAGED_TOTAL.x>=STR_DAMAGED_TOTAL.y)
# combo <- combo %>% 
#   filter(as.Date(WF_CESSATION_DATE.y)<=as.Date(WF_CESSATION_DATE.x))

compl_F$Match <- sapply(compl_F$INCIDENT_NAME, function(desc) {
  # Check for matches in df2$Name using str_detect
  matches <- compl_T$INCIDENT_NAME[str_detect(desc, fixed(compl_F$INCIDENT_NAME, ignore_case = TRUE))] # partial match, case-insensitive
  if (length(matches) > 0) {
    return(matches[1])  # Return first match found
  } else {
    return(NA)
  }
})

merged_df <- compl_F %>%
  filter(!is.na(Match)) %>%  # Filter out rows where no match is found
  left_join(compl_T, by = c("Match" = "INCIDENT_NAME"))
mer <- merged_df[,c(3, 5, 8, 22, 24, 61, 79, 85, 99, 101, 127, 138)]
mer <- mer %>% 
  filter(POO_STATE.x == POO_STATE.y)
mer <- mer %>% 
  filter(START_YEAR.x == START_YEAR.y)#No repetition

#Ecol3
test1 <- ics_eco_df %>% 
  mutate(damaged_dest = STR_DAMAGED_TOTAL + STR_DESTROYED_TOTAL) %>%
  summarise(damaged_dest = sum(damaged_dest, na.rm = T),
            STR_DAMAGED_TOTAL = sum(STR_DAMAGED_TOTAL, na.rm = T),
            STR_DESTROYED_TOTAL = sum(STR_DESTROYED_TOTAL, na.rm = T))
ics_eco_l3 <- ics_eco_df %>% 
  group_by(US_L3CODE, START_YEAR) %>% 
  summarise(STR_DAMAGED_TOTAL = sum(STR_DAMAGED_TOTAL, na.rm = T),
            STR_DESTROYED_TOTAL = sum(STR_DESTROYED_TOTAL, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(damaged_dest = STR_DAMAGED_TOTAL + STR_DESTROYED_TOTAL) 
test2 <- ics_eco_l3 %>% 
  summarise(damaged_dest = sum(damaged_dest, na.rm = T),
            STR_DAMAGED_TOTAL = sum(STR_DAMAGED_TOTAL, na.rm = T),
            STR_DESTROYED_TOTAL = sum(STR_DESTROYED_TOTAL, na.rm = T))
ics_eco_l3 <- ics_eco_l3 %>% 
  group_by(US_L3CODE) %>%
  complete(START_YEAR = 2000:2023) %>%  # Fill in all years for each region
  arrange(US_L3CODE, START_YEAR) %>%  # Order data for interpolation
  mutate(damaged_dest = ifelse(is.na(damaged_dest), 0, damaged_dest)) %>%
  ungroup() 
write.csv(ics_eco_l3, "Data/Processed/ics_l3_1.csv", row.names = F)

#Ecol4
ics_eco_l4 <- ics_eco_df %>% 
  group_by(US_L4CODE, START_YEAR) %>% 
  summarise(STR_DAMAGED_TOTAL = sum(STR_DAMAGED_TOTAL, na.rm = T),
            STR_DESTROYED_TOTAL = sum(STR_DESTROYED_TOTAL, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(damaged_dest = STR_DAMAGED_TOTAL + STR_DESTROYED_TOTAL) 
test2 <- ics_eco_l4 %>% 
  summarise(damaged_dest = sum(damaged_dest, na.rm = T),
            STR_DAMAGED_TOTAL = sum(STR_DAMAGED_TOTAL, na.rm = T),
            STR_DESTROYED_TOTAL = sum(STR_DESTROYED_TOTAL, na.rm = T))
ics_eco_l4 <- ics_eco_l4 %>% 
  group_by(US_L4CODE) %>%
  complete(START_YEAR = 2000:2023) %>%  # Fill in all years for each region
  arrange(US_L4CODE, START_YEAR) %>%  # Order data for interpolation
  mutate(damaged_dest = ifelse(is.na(damaged_dest), 0, damaged_dest)) %>%
  ungroup() 
write.csv(ics_eco_l4, "Data/Processed/ics_l4_1.csv", row.names = F)


