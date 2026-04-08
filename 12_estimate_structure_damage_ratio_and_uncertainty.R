# =============================================================================
# Script: 12_estimate_structure_damage_ratio_and_uncertainty.R
# Description:
#   Combines projected burned-area data, modeled structure counts, and
#   ICS-209 structure-loss records to estimate the ratio of structures
#   damaged or destroyed per unit of burned area within Level III ecoregions.
#   The script aggregates fire, housing, and damage datasets, computes
#   damage ratios normalized by burned area and exposure, and applies
#   bootstrap resampling to quantify uncertainty in the damage ratio and
#   resulting structure-loss estimates.
# Inputs:
#   Data/Processed/Fire_projections/fire_full.csv;
#   Data/Processed/structures/Model/structures_complete.csv;
#   Data/Processed/ics_l3_1.csv;
#   Data/Raw/us_eco_l4_state_boundaries
# Outputs:
#   Data/Processed/damage_ratio.csv;
#   Data/Processed/damage_ratio_uncert.csv;
#   Data/Processed/sigma_loss.csv
# =============================================================================

#Load required packages-------------------------------------------------
pacman::p_load(tidyverse, purrr, sf)

fire <- read.csv('Data/Processed/Fire_projections/fire_full.csv')
fire <- fire %>% 
  select(US_L4CODE, Year, type, area_burned, prop_area_burned_l4) %>% 
  distinct()
fire <- fire %>% 
  filter(prop_area_burned_l4<2) %>% 
  mutate(area_burned = ifelse(area_burned>1e+10, 1e+10, area_burned))

fire <- fire %>% 
  filter(Year>2000 & Year<2024) %>% 
  select(US_L4CODE, Year, area_burned) %>% 
  distinct()

eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') 
cw <- eco_shp %>% 
  st_drop_geometry() %>% 
  mutate(region = str_extract_all(L1_KEY, "\\d+")) %>% 
  select(US_L4CODE, US_L3CODE, region, Shape_Area)
area_l3 <- cw %>% 
  group_by(US_L3CODE) %>% 
  summarise(area_l3 = sum(Shape_Area, na.rm = T)/10000)
cw <- cw[,1:3] %>% 
  distinct() %>% 
  mutate(region = ifelse(region %in% c(6, 7, 10, 11, 12, 13), 'West', 
                         ifelse(region %in% c(8, 15, 5), 'East', 
                                ifelse(region %in% c(9), 'Central', 'Double-check')))) 

fire <- fire %>% 
  right_join(cw[,1:3])

fire <- fire %>% 
  mutate(area_burned = ifelse(US_L3CODE %in% '58' & area_burned>10, 10, area_burned))
fire_l <- fire
fire <- fire %>% 
  ungroup() %>% 
  group_by(US_L3CODE) %>% 
  summarise(area_burned = sum(area_burned, na.rm = T))

fire <- fire %>% 
  left_join(area_l3) %>% 
  mutate(prop_area_burned = area_burned/area_l3)

fire_l <- fire_l %>% 
  group_by(region, Year, US_L3CODE) %>% 
  summarise(area_burned = sum(area_burned, na.rm = T)) 

fire_l <- fire_l %>% 
  left_join(area_l3) %>% 
  mutate(prop_area_burned = area_burned/area_l3)

houses <- read.csv('Data/Processed/structures/Model/structures_complete.csv') 
houses <- houses %>% 
  filter(year>2000 & year<2024) %>% 
  select(id, eco_l4, year, model_out)
houses <- houses %>% 
  left_join(cw[,1:2], by = c( 'eco_l4'='US_L4CODE'))

houses <- houses %>% 
  group_by(US_L3CODE, year) %>% 
  summarise(structures = sum(model_out, na.rm = T)) %>% 
  ungroup()
houses_l <- houses

houses <- houses %>% 
  group_by(US_L3CODE) %>% 
  summarise(structures = mean(structures, na.rm = T))

houses_l <- houses_l %>% 
  group_by(year, US_L3CODE) %>% 
  summarise(structures = mean(structures, na.rm = T))

combo <- left_join(fire, houses, by = c('US_L3CODE'))
fire_l <- fire_l %>% 
  filter(!is.na(Year) & !(is.na(US_L3CODE)))
houses_l <- houses_l %>% 
  filter(!is.na(year) & !(is.na(US_L3CODE)))
combo_l <- left_join(ungroup(fire_l), ungroup(houses_l), by = c('US_L3CODE', 'Year'='year')) 
ics_l3 <- read.csv("/Users/viig7608/Desktop/Fire risk/Data/Processed/ics_l3_1.csv") %>% 
  filter(START_YEAR>2000 & START_YEAR<2024)#84 regions and it's correct

ics_l3_l <- ics_l3 %>% 
  group_by(START_YEAR, US_L3CODE) %>% 
  summarise(damaged_dest = sum(damaged_dest, na.rm = T)) %>% 
  ungroup()
ics_l3_l <- ics_l3_l %>% 
  filter(!is.na(US_L3CODE) & !is.na(START_YEAR))
ics_l3 <- ics_l3 %>% 
  group_by(US_L3CODE) %>% 
  summarise(damaged_dest = sum(damaged_dest, na.rm = T))
names(ics_l3_l)[1] <- 'Year'
combo <- combo %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE)) %>% 
  left_join(ics_l3)
combo_l <- combo_l %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE)) %>% 
  left_join(ics_l3_l)

combo <- combo %>% 
  mutate(damaged_dest_ratio = damaged_dest/structures,
         damaged_dest_ratio_adj = damaged_dest_ratio / prop_area_burned)
combo_l <- combo_l %>% 
  mutate(damaged_dest_ratio = damaged_dest/structures,
         damaged_dest_ratio_adj = damaged_dest_ratio / prop_area_burned)
# combo_sum <- combo_sum %>% 
#   mutate(str2 = prop_area_burned_eco_l3* damaged_dest_ratio_adj*total_structures)
write.csv(combo, '/Users/viig7608/Desktop/Fire risk/Data/Processed/damage_ratio.csv')

#Compute uncertainty
set.seed(123)
B <- 2000   # bootstrap replicates

boot_pd <- function(df) {
  num <- sum(df$damaged_dest, na.rm = TRUE)
  den <- sum(df$prop_area_burned * df$structures, na.rm = TRUE)
    if (is.na(den) || den <= 0) return(NA_real_)
  
  num / den
}


# 2) Point estimate per region (non-bootstrap)

pd_point <- combo_l %>%
  group_by(US_L3CODE) %>%
  summarise(
    pd_hat = boot_pd(cur_data()),
    den_sum = sum(prop_area_burned * structures, na.rm = TRUE),
    num_sum = sum(damaged_dest, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 3) Bootstrap pd per region (resample rows within region)

pd_boot <- combo_l %>%
  group_by(US_L3CODE) %>%
  group_modify(~{
    dat <- .x
    n <- nrow(dat)
    
    # If a region has 0 rows, return empty 
    if (n == 0) return(tibble(boot = integer(), pd = numeric()))
    
    tibble(
      boot = seq_len(B),
      pd = map_dbl(seq_len(B), function(i) {
        idx <- sample.int(n, size = n, replace = TRUE)
        
        tryCatch(
          boot_pd(dat[idx, , drop = FALSE]),
          error = function(e) NA_real_
        )
      })
    )
  }) %>%
  ungroup()

# Propagate pd uncertainty to destroyed structures

loss_boot <- combo_l %>%
  select(US_L3CODE, Year, prop_area_burned, structures) %>%
  inner_join(pd_boot, by = "US_L3CODE") %>%
  mutate(
    str_destroyed_boot = prop_area_burned * structures * pd
  )
# Summarize pd uncertainty per region

pd_summary <- pd_boot %>%
  group_by(US_L3CODE) %>%
  summarise(
    pd_med = median(pd, na.rm = TRUE),
    pd_mean = mean(pd, na.rm = TRUE),
    pd_sd = sd(pd, na.rm = TRUE),
    pd_lo = quantile(pd, 0.025, na.rm = TRUE, names = FALSE),
    pd_hi = quantile(pd, 0.975, na.rm = TRUE, names = FALSE),
    sigma_log_pd = sd(log(pd[pd > 0]), na.rm = TRUE),
    gsd_pd = exp(sigma_log_pd),
    n_eff = sum(!is.na(pd)),
    frac_na = mean(is.na(pd)),
    .groups = "drop"
  ) %>%
  left_join(pd_point, by = "US_L3CODE") %>%
  relocate(US_L3CODE, n, num_sum, den_sum, pd_hat, pd_med, pd_lo, pd_hi, pd_mean, pd_sd, n_eff, frac_na)

write.csv(pd_summary, '/Users/viig7608/Desktop/Fire risk/Data/Processed/damage_ratio_uncert.csv')

write.csv(loss_boot, '/Users/viig7608/Desktop/Fire risk/Data/Processed/sigma_loss.csv')



