# =============================================================================
# Script: plot_fire_timeseries_by_level1_ecoregion.R
#
# Purpose:
#   Create a faceted time-series figure comparing observed and projected annual
#   area burned across Level I ecoregions. The figure combines historical MTBS
#   burned-area estimates with projected burned area from multiple climate
#   models and saves the result as both PDF and JPEG.
#
# Inputs required:
#   1. Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv
#      - Combined historical/projection burned-area table for HadGEM2-ES.
#
#   2. Data/Processed/Fire_projections/Fire_combo_cnrm_l4.csv
#      - Combined historical/projection burned-area table for CNRM.
#
#   3. Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv
#      - Combined historical/projection burned-area table for MRI.
#
#   4. Data/Raw/us_eco_l4_state_boundaries
#      - Level IV ecoregion boundary layer containing Level I labels used for
#        aggregation and faceting.
#
# Expected columns in Fire_combo_* files:
#   - Year
#   - type
#   - area_burned_mod
#   - US_L4CODE
#
# Outputs produced:
#   1. Figures/fire_l1_ts.pdf
#   2. Figures/fire_l1_ts.jpg
#
# Figure description:
#   - Each facet shows one Level I ecoregion.
#   - Black line/ribbon: MTBS historical burned area
#   - Colored lines/ribbons: projected burned area from climate models
#   - Y-axis units are million hectares (M ha)
#
# Notes:
#   - The script aggregates Level IV data to Level I ecoregions.
#   - Uncertainty bands are derived from a custom standard-error aggregation.
#   - CNRM and MRI inputs exclude rows where type == "MTBS" to avoid duplicate
#     historical series.
# =============================================================================


# Load required packages ---------------------------------------------------

# tidyverse: data manipulation and plotting
# ggtext: text rendering support for ggplot
# tools: file/path utilities
# foreach: iterative workflows (loaded but not directly used here)
# scico: color scales (loaded but not directly used here)
# scales: axis scaling / expansion helpers
# ggthemes: additional ggplot themes (loaded but not directly used here)
pacman::p_load(tidyverse, ggtext, tools, foreach, scico, scales, ggthemes)


# Define climate models ----------------------------------------------------

models <- c('CNRM', 'MRI', 'HadGEM2-ES')


# Read combined burned-area datasets --------------------------------------

# Input:
#   Processed burned-area files by climate model
# Purpose:
#   Load model-specific combined fire datasets, removing duplicate MTBS rows
#   from CNRM and MRI files so that the historical record appears only once.
had <- read.csv('Data/Processed/Fire_projections/Fire_combo_HadGEM2-ES_l4.csv')[,-1]

cnrm <- read.csv('Data/Processed/Fire_projections/Fire_combo_cnrm_l4.csv') %>% 
  filter(type != 'MTBS')

mri <- read.csv('Data/Processed/Fire_projections/Fire_combo_MRI_l4.csv') %>% 
  filter(type != 'MTBS')


# Combine all model datasets ----------------------------------------------

combo <- rbind(had, cnrm)
combo <- rbind(combo, mri)


# Read Level IV ecoregion attributes --------------------------------------

# Input:
#   Data/Raw/us_eco_l4_state_boundaries
# Purpose:
#   Attach Level I ecoregion labels to the combined Level IV fire data.
eco_shp <- st_read('Data/Raw/us_eco_l4_state_boundaries') %>%
  st_drop_geometry()

# Retain only the columns needed for joining.
eco_shp <- eco_shp[, c(1, 16, 18)]

combo <- left_join(combo, eco_shp)


# Helper function: clean Level I labels -----------------------------------

# Purpose:
#   Remove leading numeric codes from Level I ecoregion names and convert them
#   to title case for cleaner facet labels.
cleaned_factor <- function(x){
  gsub("^\\d+\\s+", "", x) %>% 
    str_to_title()
}


# Helper function: aggregate standard error -------------------------------

# Purpose:
#   Compute a combined standard error across Level IV ecoregions within each
#   Level I ecoregion-year group.
calculate_se_sum <- function(x) {
  sqrt(sum((sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))^2))
}


# Clean labels and summarize by Level I ecoregion -------------------------

combo <- combo %>% 
  mutate(L1_KEY = cleaned_factor(L1_KEY))

# Aggregate burned area across Level IV ecoregions within each Level I
# ecoregion, year, and data type.
combo_sum <- combo %>% 
  group_by(Year, L1_KEY, type) %>% 
  summarise(
    total_area_burned = sum(area_burned_mod),
    se_total_area_burned = calculate_se_sum(area_burned_mod)
  ) %>% 
  mutate(
    upper_area_burned = (total_area_burned + 2 * se_total_area_burned) * 1.25,
    lower_area_burned = ifelse(
      (total_area_burned - 2 * se_total_area_burned) < 0,
      0,
      (total_area_burned - 2 * se_total_area_burned) * .75
    ),
    type = ifelse(type %in% 'mri', 'MRI', type)
  )


# Prepare facet labels -----------------------------------------------------

# Purpose:
#   Create panel labels with alphabetical prefixes and determine a y-position
#   for panel annotation in each facet.
x <- combo_sum %>% 
  mutate(L1_KEY = factor(L1_KEY)) %>% 
  group_by(L1_KEY) %>%
  mutate(
    label1 = paste0('(', LETTERS[as.numeric(L1_KEY)], ') ', L1_KEY),
    label2 = paste0('(', LETTERS[as.numeric(L1_KEY)], ') '),
    ylabel = max(upper_area_burned / 1000000, na.rm = TRUE) * 1.05
  )


# Define plotting palette --------------------------------------------------

pal <- palette.colors(3, palette = 'Dark 2')
names(pal) <- unique(combo_sum$type)[-1]


# Create faceted time-series plot -----------------------------------------

g_fire <- ggplot() + 

  # Historical MTBS uncertainty ribbon
  geom_ribbon(
    data = filter(combo_sum, type %in% 'MTBS'),
    aes(
      x = Year,
      ymin = lower_area_burned / 1000000,
      ymax = upper_area_burned / 1000000
    ),
    alpha = .3,
    show.legend = FALSE
  ) +

  # Projected-model uncertainty ribbons
  geom_ribbon(
    data = filter(combo_sum, !type %in% 'MTBS'),
    aes(
      x = Year,
      ymin = lower_area_burned / 1000000,
      ymax = upper_area_burned / 1000000,
      fill = type
    ),
    alpha = .3,
    show.legend = FALSE
  ) +

  # Facet by Level I ecoregion
  facet_wrap(~L1_KEY, scale = 'free', ncol = 2) +

  # Projected-model lines
  geom_line(
    data = filter(combo_sum, !type %in% 'MTBS'),
    aes(
      x = Year,
      y = total_area_burned / 1000000,
      color = type
    )
  ) +

  # Historical MTBS line
  geom_line(
    data = filter(combo_sum, type %in% 'MTBS'),
    aes(
      x = Year,
      y = total_area_burned / 1000000
    )
  ) +

  scale_fill_manual(values = pal, '') +
  scale_color_manual(values = pal, '') +

  # Historical MTBS line is drawn a second time in the original script
  geom_line(
    data = filter(combo_sum, type %in% 'MTBS'),
    aes(
      x = Year,
      y = total_area_burned / 1000000
    )
  ) +

  ylab('Area burned (M ha)') +
  xlab('Year') +

  # Panel labels: regular text + bold letter prefix
  geom_text(
    data = x,
    hjust = 0,
    aes(label = label1, x = 1984, y = ylabel),
    size = 3.5
  ) +
  geom_text(
    data = x,
    hjust = 0,
    aes(label = label2, x = 1984, y = ylabel),
    fontface = "bold",
    size = 3.5
  ) +

  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +

  theme(
    panel.background = element_rect(fill = 'white', color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_blank(),
    legend.position = 'bottom',
    legend.title.position = 'top',
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    axis.title.y = element_text(hjust = 0.5, vjust = 0, size = 9),
    axis.title.x = element_text(hjust = 0.5, vjust = 0, size = 9),
    axis.text = element_text(size = 8),
    legend.title.align = 0.5
  ) +
  guides(
    color = guide_legend(
      ncol = 3,
      byrow = TRUE
    )
  )


# Save outputs -------------------------------------------------------------

ggsave(
  plot = g_fire,
  'Figures/fire_l1_ts.pdf',
  width = 6.75,
  height = 9,
  dpi = 900
)

ggsave(
  plot = g_fire,
  'Figures/fire_l1_ts.jpg',
  width = 6.75,
  height = 9,
  dpi = 900
)




