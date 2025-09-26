pacman::p_load(tidyverse, ggtext, tools, foreach, scico, scales, ggthemes)
models <- c('CNRM', 'MRI', 'HadGEM2-ES')

had <- read.csv('Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv')[,-1]
cnrm <- read.csv('Data/Processed/Fire_projections/Fire_combo_cnrm_l4.csv') %>% 
  filter(type != 'MTBS')
mri <- read.csv('Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv') %>% 
  filter(type != 'MTBS')

combo <- rbind(had, cnrm)
combo <- rbind(combo, mri)

eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>%
  st_drop_geometry() 

eco_shp <- eco_shp[,c(1, 16, 18)]

combo <- left_join(combo, eco_shp )
# Remove the numeric part and leading space
cleaned_factor <- function(x){
  gsub("^\\d+\\s+", "", x) %>% 
    str_to_title() 
}

calculate_se_sum <- function(x) {
  sqrt(sum((sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))^2))
}

combo <- combo %>% 
  mutate(L1_KEY = cleaned_factor(L1_KEY))

combo_sum <- combo %>% 
 group_by(Year, L1_KEY, type) %>% 
  summarise(total_area_burned = sum(area_burned_mod),
            se_total_area_burned = calculate_se_sum(area_burned_mod)) %>% 
  mutate(upper_area_burned = (total_area_burned + 2*se_total_area_burned)*1.25,
         lower_area_burned = ifelse((total_area_burned - 2*se_total_area_burned)<0, 0, (total_area_burned - 2*se_total_area_burned)*.75),
         type = ifelse(type %in% 'mri', 'MRI', type))

x <- combo_sum %>% 
  mutate(L1_KEY = factor(L1_KEY)) %>% 
  group_by(L1_KEY) %>%
  mutate(label1 = paste0('(', LETTERS[as.numeric(L1_KEY)], ') ', L1_KEY),
         label2 = paste0('(', LETTERS[as.numeric(L1_KEY)], ') '),
         ylabel = max(upper_area_burned/1000000, na.rm = T)*1.05)


pal <- palette.colors(3, palette = 'Dark 2')
names(pal) <- unique(combo_sum$type)[-1]
g_fire <- ggplot() + 
  geom_ribbon(data = filter(combo_sum, type %in% 'MTBS'),
              aes(x = Year, ymin = lower_area_burned/1000000, ymax = upper_area_burned/1000000), alpha = .3, show.legend = F) +
  geom_ribbon(data = filter(combo_sum, !type %in% 'MTBS'),
              aes(x = Year, ymin = lower_area_burned/1000000, ymax = upper_area_burned/1000000, fill = type), alpha = .3, show.legend = F) +
  facet_wrap(~L1_KEY, scale = 'free', ncol = 2) +
  geom_line(data = filter(combo_sum, !type %in% 'MTBS'),
            aes(x = Year, y = total_area_burned/1000000, color = type)) +
  geom_line(data = filter(combo_sum, type %in% 'MTBS'),
            aes(x = Year, y = total_area_burned/1000000)) +
  scale_fill_manual(values = pal, '') +
  scale_color_manual(values = pal, '') +
  geom_line(data = filter(combo_sum, type %in% 'MTBS'),
            aes(x = Year, y = total_area_burned/1000000)) +
  ylab('Area burned (M ha)') +
  xlab('Year') +
  geom_text(data = x,
            hjust = 0,
            aes(label = label1, x = 1984, y = ylabel),
            size = 3.5) +
  geom_text(data = x,
            hjust = 0,
            aes(label = label2, x = 1984, y = ylabel) , fontface = "bold",
            size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size = 10),   # Legend labels text size
        legend.title = element_text(size = 11),
        axis.title.y = element_text(hjust = 0.5, vjust = 0, size = 9),
        axis.title.x = element_text(hjust = 0.5, vjust = 0, size = 9),
        axis.text = element_text(size = 8),
        legend.title.align = 0.5) +
  guides(color = guide_legend(ncol = 3,
                              byrow = T)) 
 
ggsave(plot = g_fire, 'Figures/fire_l1_ts.pdf', width = 6.75, height = 9, dpi = 900)
ggsave(plot = g_fire, 'Figures/fire_l1_ts.jpg', width = 6.75, height = 9, dpi = 900)





