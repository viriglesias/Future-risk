pacman::p_load(tidyverse, sf, foreach, scico, scales)

if(file.exists('/Users/viig7608/Desktop/Fire risk/Data/Processed/prob_burning_adj.csv')){
  combo <- read.csv('/Users/viig7608/Desktop/Fire risk/Data/Processed/prob_burning_adj.csv')
  } else {
ics <- read.csv("Data/Processed/ics_l3_1.csv") %>% 
  filter(START_YEAR>2000 & START_YEAR<2024)
zillow <- read.csv("Data/Processed/zillow_l3_1.csv") %>% 
  filter(START_YEAR>2000 & START_YEAR<2024)
combo <- ics %>% 
  right_join(zillow)
combo <- combo %>% 
  mutate(damaged_dest_ratio = damaged_dest/structures)
mtbs <- read.csv("Data/Processed/mtbs_l3.csv") %>% 
  filter(year>2000 & year<2024)

combo <- left_join(mtbs, combo, by = c('US_L3CODE', 'year' = 'START_YEAR'))
combo <- filter(combo, !is.na(US_L3CODE))

combo <- combo %>% 
  mutate(damaged_ratio_adj = damaged_dest_ratio/prop_area_burned_eco_l3) %>% 
  filter(!is.na(damaged_ratio_adj) & is.finite(damaged_ratio_adj))


combo <- combo %>% 
  group_by(US_L3CODE) %>% 
  summarise(area_burned_ratio_median = median(prop_area_burned_eco_l3),
            damaged_ratio_median = median(damaged_dest_ratio),
            damaged_ratio_adj_median = median(damaged_ratio_adj))
write.csv(combo, '/Users/viig7608/Desktop/Fire risk/Data/Processed/prob_burning_adj.csv')
}
#Figs
eco <- st_read('/Users/viig7608/Desktop/Fire risk/Data/Raw/us_eco_l3')
eco <- eco %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE))
combo_l3_sum <-  combo %>% 
  left_join(eco)

if (!inherits(combo_l3_sum, "sf")) {
  combo_l3_sum <- st_as_sf(combo_l3_sum, sf_column_name = "geometry")
}

us <- st_read('Data/Raw/cb_2019_us_county_500k') %>% 
  st_transform(st_crs(eco)) %>% 
  subset(!(STATEFP %in% c('72', '02', '60', '15', '66', '69', '78'))) 

regions <- eco %>% 
  mutate(region = str_extract_all(L1_KEY, "\\d+"))

regions <- regions %>% 
  mutate(region = ifelse(region %in% c(6, 7, 10, 11, 12, 13), 'West', 
                         ifelse(region %in% c(8, 15, 5), 'East', 
                                ifelse(region %in% c(9), 'Central', 'Double-check')))) 
regions <- regions %>% 
  group_by(region) %>% 
  summarise(geometry = st_union(geometry))

disp <- 'wide'
if(disp %in% 'long'){
  pp = 885132
  nc = 1
  wd = 6.5/1.8
  ht = 7
}else{
  pp = 1285132
  nc = 3
  wd = 6.75
  ht = 2.5
}

combo_l3_sum <- combo_l3_sum %>% 
  mutate(proportion_burned_mod = ifelse(area_burned_ratio_median > 0.003, 0.003, area_burned_ratio_median))
burned_ratio_g <- ggplot(combo_l3_sum) +
  geom_sf(aes(fill = proportion_burned_mod),
          col = NA) +
  scale_fill_scico(palette = 'lipari', direction = -1, na.value = 'red') +
  labs(tag =' (B)') +
  geom_sf(data = regions, fill = NA, lwd = .1) +
  coord_sf() +
  theme_void() +
  theme(plot.tag = element_text(size = 11, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 11),
        legend.position = "none") +
  scale_y_continuous(limits = c(272048.5-(1505132), 3172577))


data1 <- density(combo_l3_sum$area_burned_ratio_median, from = min(combo_l3_sum$area_burned_ratio_median), to = max(combo_l3_sum$area_burned_ratio_median), adj = 10)
# plot(data1)
data12 <- as.data.frame(approx(x = data1$x, y = data1$y, n = 10000))
data12 <- data12 %>% 
  mutate(fl = sapply(x, function(x) ifelse(x>0.003, 0.003, x)))
data12 <- data12 %>% 
  mutate(x = x*10^(3))
extra1 <- (range(data12$y)[2]-range(data12$y)[1])*.1

leg1 <- ggplot()+
  geom_segment(data = data12,
               aes(x = x, y = y + extra1, xend = x, yend = 0, colour = fl), linewidth = 0.1) +
  scale_color_scico(palette = 'lipari', direction = -1) +
  geom_area(data = data12, aes(x = x, y = y+ extra1),
            fill = NA, color = "black", lwd = .1) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = data12$y[1]+ extra1),lwd = .1) +
  geom_segment(aes(x = 0, y = 0, xend = max(data12$x), yend = 0), lwd = .1) +
  # geom_segment(aes(x = 0, y = extra1, yend = extra1, xend = max(data12$x)), lwd = .1) +
  theme_void() +
  theme(legend.position = "none",
        axis.title = element_text(size = 8, hjust = 0.5),
        axis.text = element_text(size = 8, margin = margin(t = 2, b=2)),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis tick labels
        axis.ticks.y = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     labels = c('0', '3', '6', '9', '12')) +
  xlab(expression("Burned area ratio ("~ 10^-3 ~ ")"))


f1 <-burned_ratio_g +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = NA, color = "black", linewidth = .5) +
  annotation_custom(ggplotGrob(leg1), 
                    xmin = -2356069.5, xmax = 2258224.8, 
                    ymin = 272048.5-(1505132), ymax = 272048.5+1.5*(725132/12)) +
  theme(plot.margin = margin(5, 1, 5, 0))

  
combo_l3_sum <- combo_l3_sum %>% 
  mutate(proportion_dam_or_dest_mod = ifelse(damaged_ratio_median > .00003, .00003, damaged_ratio_median))
damage_ratio <- ggplot(combo_l3_sum) +
  geom_sf(aes(fill = proportion_dam_or_dest_mod),
          col = NA) + 
  scale_fill_scico(palette = 'lipari', direction = -1, na.value = 'red') +
  labs(tag =' (A)') +
  geom_sf(data = regions, fill = NA, lwd = .1) +
  coord_sf() +
  theme_void() +
  theme(plot.tag = element_text(size = 11, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 11),
        legend.position = "none") +
  scale_y_continuous(limits = c(272048.5-(1505132), 3172577))

data2 <- density(combo_l3_sum$damaged_ratio_median, from = min(combo_l3_sum$damaged_ratio_median), to =  0.0001, adj = 10)
# plot(data2)
data22 <- as.data.frame(approx(x = data2$x, y = data2$y, n = 10000))
data22 <- data22 %>% 
  mutate(fl = sapply(x, function(x) ifelse(x>.00003, .00003, x))) 
data22 <- data22 %>% 
  mutate(x = x*10^5)
extra2 <- (range(data22$y)[2]-range(data22$y)[1])*.1

leg2 <- ggplot()+
  geom_segment(data = data22,
               aes(x = x, y = y+extra2, xend = x, yend = 0, colour = fl), linewidth = 0.1) +
  scale_color_scico(palette = 'lipari', direction = -1, na.value = 'red') +
  geom_area(data = data22, aes(x = x, y = y+extra2), lwd = .1,
            fill = NA, color = "black") +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = data22$y[1]+extra2), lwd = .1) +
  # geom_segment(aes(x = 0, y = 0, xend = max(data22$x), yend = 0), lwd = .1) +
  theme_void() +
  theme(legend.position = "none",
        axis.title = element_text(size = 8, hjust = 0.5),
        axis.text = element_text(size = 8, margin = margin(t = 2, b=2)),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis tick labels
        axis.ticks.y = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10),
                     labels = c('0', '2.5', '5', '7.5', '10')) +
  xlab(expression("Damage ratio ("~ 10^-5 ~ ")"))


f2 <- damage_ratio + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
             fill = NA, color = "black", linewidth = .5) +
  annotation_custom(ggplotGrob(leg2), 
                    xmin = -2356069.5, xmax = 2258224.8, 
                    ymin = 272048.5-(1505132), ymax = 272048.5+1.5*(725132/12)) +
  theme(plot.margin = margin(5, 1, 5, 0))

combo_l3_sum <- combo_l3_sum %>% 
  mutate(proportion_dam_or_dest_adj_mod = ifelse(damaged_ratio_adj_median > 0.07, 0.07, 
                                                 damaged_ratio_adj_median))

damage_ratio_adj <- ggplot(combo_l3_sum) +
  geom_sf(aes(fill = proportion_dam_or_dest_adj_mod),
          col = NA) + 
  scale_fill_scico(palette = 'lipari', direction = -1, na.value = 'red') +
  labs(tag =' (C)') +
  geom_sf(data = regions, fill = NA, lwd = .1) +
  coord_sf() +
  theme_void() +
  theme(plot.tag = element_text(size = 11, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 11),
        legend.position = "none") +
  scale_y_continuous(limits = c(272048.5-(1505132), 3172577))

data <- density(combo_l3_sum$damaged_ratio_adj_median, from = min(combo_l3_sum$damaged_ratio_adj_median), to = 0.085, adj = 10)
# plot(data)
data2 <- as.data.frame(approx(x = data$x, y = data$y, n = 10000))
data2 <- data2 %>% 
  mutate(fl = sapply(x, function(x) ifelse(x>0.07, 0.07, x)))
# data2 <- data2 %>% 
#   mutate(x = x*10^8)
extra <- (range(data2$y)[2]-range(data2$y)[1])*.1

leg3 <- ggplot()+
  geom_segment(data = data2,
               aes(x = x, y = y + extra, xend = x, yend = 0, colour = fl), linewidth = 0.1) +
  scale_color_scico(palette = 'lipari', direction = -1, na.value = 'red') +
  geom_area(data = data2, aes(x = x, y = y + extra),
            fill = NA, color = "black", lwd = .1) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = data2$y[1] + extra), lwd = .1) +
  geom_segment(aes(x = 0, y = 0, xend = max(data2$x), yend = 0), lwd = .1) +
  theme_void() +
  theme(legend.position = "none",
        axis.title = element_text(size = 8, hjust = 0.5),
        axis.text = element_text(size = 8, margin = margin(t = 2, b=2)),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis tick labels
        axis.ticks.y = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(0, 0.03, 0.06)) +
  xlab(expression("Adjusted damage ratio"))

f3 <- damage_ratio_adj + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = NA, color = "black", linewidth = .5) +
  annotation_custom(ggplotGrob(leg3), 
                    xmin = -2356069.5, xmax = 2258224.8, 
                    ymin = 272048.5-(1505132), ymax = 272048.5+1.5*(725132/12)) +
  theme(plot.margin = margin(5, 1, 5, 0))

fig <- ggpubr::ggarrange(f2, f1, f3,
                         ncol = nc)

ggsave(plot = fig, paste0('Figures/Fig_5_map_mtbs_l3_', disp, '.jpeg'), width = wd, height = ht, dpi = 600)
ggsave(plot = fig, paste0('Figures/Fig_5map_mtbs_l3_', disp, '.pdf'), width = wd, height = ht, dpi = 600)

# range(combo_l3_sum$area_burned_ratio_median)[1] 0.000000e+00 0.01219797
# range(combo_l3_sum$damaged_ratio_median) [1] 0.000000000 0.0007765422
# range(combo_l3_sum$damaged_ratio_adj_median)[1] 0.000000e+00 3.499262e-06
# 
# 
head(combo_l3_sum)


cc <- combo_l3_sum %>% 
  mutate(region = str_extract_all(L1_KEY, "\\d+"))

cc <- cc %>% 
  mutate(region = ifelse(region %in% c(6, 7, 10, 11, 12, 13), 'West', 
                         ifelse(region %in% c(8, 15, 5), 'East', 
                                ifelse(region %in% c(9), 'Central', 'Double-check')))) 


cc %>% 
  st_drop_geometry() %>% 
  group_by(region) %>% 
  summarise(mean_area_burned_ratio_median = weighted.mean(area_burned_ratio_median, Shape_Area, na.rm = TRUE),
            se_area = sd(area_burned_ratio_median)/n(),
            median_area = wtd.quantile(area_burned_ratio_median, Shape_Area, probs = 0.5),
            mean_damaged_ratio_median = weighted.mean(damaged_ratio_median, Shape_Area, na.rm = TRUE),
            se_damaged_ratio_median = sd(damaged_ratio_median)/n(),
            median_damaged_ratio_median = wtd.quantile(damaged_ratio_median, Shape_Area, probs = 0.5),
            mean_damaged_ratio_adj_median = weighted.mean(damaged_ratio_adj_median, Shape_Area, na.rm = TRUE),
            se_damaged_ratio_adj_median = sd(damaged_ratio_adj_median)/n(),
            median_damaged_ratio_adj_median = wtd.quantile(damaged_ratio_adj_median, Shape_Area, probs = 0.5)) %>% 
  t()

cc %>% 
  st_drop_geometry() %>% 
  summarise(mean_area_burned_ratio_median = weighted.mean(area_burned_ratio_median, Shape_Area, na.rm = TRUE),
            se_area = sd(area_burned_ratio_median)/n(),
            median_area = wtd.quantile(area_burned_ratio_median, Shape_Area, probs = 0.5),
            mean_damaged_ratio_median = weighted.mean(damaged_ratio_median, Shape_Area, na.rm = TRUE),
            se_damaged_ratio_median = sd(damaged_ratio_median)/n(),
            median_damaged_ratio_median = wtd.quantile(damaged_ratio_median, Shape_Area, probs = 0.5),
            mean_damaged_ratio_adj_median = weighted.mean(damaged_ratio_adj_median, Shape_Area, na.rm = TRUE),
            se_damaged_ratio_adj_median = sd(damaged_ratio_adj_median)/n(),
            median_damaged_ratio_adj_median = wtd.quantile(damaged_ratio_adj_median, Shape_Area, probs = 0.5)) %>% 
  t()
