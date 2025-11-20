############################################################
# Purpose:
#   - Load farm-level crop group counts
#   - Compute, for each farm, the proportion of species in each crop group
#   - Aggregate mean crop-group proportions by productive area class
#   - Export a tidy summary table
#   - Produce facetted pie charts (Figure 5) showing crop-group composition
#     by farm size class
############################################################

# ---- 0. Load packages ----
library(readxl)   # read Excel files
library(dplyr)    # data wrangling
library(tidyr)    # reshaping
library(ggplot2)  # plotting
library(writexl)  # write Excel files

# ---- 1. Read input data ----
# Input: Aug_crop_groups.xlsx
# Each row corresponds to a farm with counts of species in each crop group
farm <- read_excel("data/Aug_crop_groups.xlsx")

# Optional: inspect structure
# str(farm)
# colnames(farm)

# ---- 2. Prepare factors ----
farm <- farm %>%
  mutate(
    prod_area_classes = as.factor(prod_area_classes)
  )

# ---- 3. Define crop-group columns ----
crop_groups <- c(
  "Cereals",
  "Fruits_nuts",
  "Leguminous",
  "Oilseed_oleaginous",
  "Root_tuber",
  "Beverage_stimulant_spice_aromatic",
  "Sugar",
  "Vegetables_melons",
  "Others"
)

# ---- 4. Compute per-farm proportions by crop group ----
# 1) total_species: sum of all crop-group counts per farm
# 2) prop_<group>: share of each group in total species per farm

proportions_farm <- farm %>%
  rowwise() %>%
  mutate(
    total_species = sum(c_across(all_of(crop_groups)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    across(
      all_of(crop_groups),
      ~ .x / total_species,
      .names = "prop_{.col}"
    )
  )

# ---- 5. Mean crop-group proportions by farm size ----
mean_prop_by_size <- proportions_farm %>%
  group_by(prod_area_classes) %>%
  summarise(
    across(
      starts_with("prop_"),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Optional: inspect
# View(mean_prop_by_size)

# ---- 6. Reshape to long format & convert to percentages ----
mean_prop_long <- mean_prop_by_size %>%
  pivot_longer(
    cols      = starts_with("prop_"),
    names_to  = "crop_group",
    names_prefix = "prop_",
    values_to = "percentage"
  ) %>%
  mutate(
    percentage = percentage * 100
  )

# ---- 7. Set factor levels and labels ----
mean_prop_long <- mean_prop_long %>%
  mutate(
    crop_group = factor(
      crop_group,
      levels = c(
        "Beverage_stimulant_spice_aromatic",
        "Cereals",
        "Fruits_nuts",
        "Leguminous",
        "Oilseed_oleaginous",
        "Root_tuber",
        "Sugar",
        "Vegetables_melons",
        "Others"
      )
    ),
    prod_area_classes = factor(
      prod_area_classes,
      levels = c(
        "Small (< 3.5 ha)",
        "Medium (3.5 - 15 ha)",
        "Large (> 15 ha)"
      )
    )
  )

# Optional: check factor structure
# levels(mean_prop_long$prod_area_classes)
# str(mean_prop_long$prod_area_classes)

# ---- 8. Export tidy summary table ----
write_xlsx(mean_prop_long, path = "data/Aug_crop_groups_mean.xlsx")

############################################################
# 9. Plot: Facetted pie charts (Figure 5)
############################################################

# Custom color palette for crop groups
crop_palette <- c(
  "Beverage_stimulant_spice_aromatic" = "#3C5488",
  "Cereals"                           = "#E18727",
  "Fruits_nuts"                       = "#91D1C2",
  "Leguminous"                        = "#B09C85",
  "Oilseed_oleaginous"                = "#E64B35",
  "Root_tuber"                        = "#6F99AD",
  "Sugar"                             = "#631879",
  "Vegetables_melons"                 = "#00A087",
  "Others"                            = "#A20056"
)

crop_labels <- c(
  "Beverage/stimulant/spice/aromatic crops",
  "Cereals",
  "Fruits and nuts",
  "Leguminous crops",
  "Oilseed and oleaginous crops",
  "Root and tuber crops",
  "Sugar",
  "Vegetables and melons",
  "Others"
)

# Create pie charts facetted by farm size class
fig_5 <- ggplot(mean_prop_long, aes(x = "", y = percentage, fill = crop_group)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ prod_area_classes) +
  theme_void() +
  labs(
    title = "Crop groups by productive area",
    fill  = "Crop groups"
  ) +
  scale_fill_manual(
    values = crop_palette,
    labels = crop_labels
  ) +
  theme(
    plot.title = element_text(
      hjust  = 0.5,
      size   = 14,
      face   = "bold",
      margin = margin(b = 12)
    ),
    strip.text = element_text(
      size   = 12,
      margin = margin(b = 10, t = 10)
    ),
    legend.title = element_blank(),
    legend.text  = element_text(size = 10),
    legend.position = "bottom",
    legend.justification = "center"
  )

# Print in R session (optional)
# fig_5

# ---- 10. Save Figure 5 ----
ggsave(
  filename = "fig/Figure_5.jpg",
  plot     = fig_5,
  width    = 10,
  height   = 5,
  units    = "in",
  dpi      = 300
)

############################################################
# End of script
############################################################