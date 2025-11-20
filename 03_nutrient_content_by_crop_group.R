############################################################
# Purpose:
#   - Load crop- and nutrient-level data
#   - Clean and harmonize crop group classifications
#   - Reshape nutrient variables to long format
#   - Compute mean nutrient content by crop group
#   - Produce facetted bar plots of mean nutrient content
#   - Save publication-quality figure (Figure S1)
############################################################

# ---- 0. Load packages ----
library(readxl)   # read Excel files
library(dplyr)    # data wrangling
library(tidyr)    # data reshaping
library(ggplot2)  # plotting
library(writexl)  # (optional) write data if needed

# ---- 1. Read input data ----
data <- read_excel("data/Jun6-2025_edge_density_data.xlsx")

# Optional: inspect structure
# str(data)

# ---- 2. Basic type cleaning ----
data <- data %>%
  mutate(
    crop_group          = as.factor(crop_group),
    lifecycle           = as.factor(lifecycle),
    Crop_name_normalized = as.factor(Crop_name_normalized)
  )

# ---- 3. Extract unique nutrient profiles ----
# We keep one row per (Crop_name_normalized, crop_group, lifecycle)
nut_content <- data %>%
  select(
    Crop_name_normalized,
    crop_group,
    lifecycle,
    `protein in grams per 100g`,
    `lipid/fat in grams per 100g`,
    `energy in kcal per 100g`,
    `carbs in grams per 100g`,
    `vitamin c in grams per 100g`
  ) %>%
  distinct()

# Optional: inspect
# View(nut_content)
# str(nut_content)

# ---- 4. Harmonize crop-group categories ----
# Merge related crop groups into broader categories
nut_content <- nut_content %>%
  mutate(
    crop_group = as.character(crop_group),
    crop_group = ifelse(
      crop_group %in% c("Beverage and spice crops",
                        "Stimulant, spice and aromatic crops"),
      "Beverage/stimulant/spice/aromatic crops",
      crop_group
    ),
    crop_group = ifelse(
      crop_group %in% c("Potatoes and yams",
                        "High starch root/tuber crops"),
      "Root and tuber crops",
      crop_group
    ),
    crop_group = factor(crop_group)
  )

# Define intended order and display labels for crop groups
crop_levels <- c(
  "Beverage/stimulant/spice/aromatic crops",
  "Cereals",
  "Fruit and nuts",
  "Leguminous crops",
  "Oilseed crops and oleaginous fruits",
  "Root and tuber crops",
  "Sugar crops",
  "Vegetables and melons",
  "Other crops"
)

crop_labels <- c(
  "Beverage/stimulant/spice/\naromatic crops",
  "Cereals",
  "Fruit and nuts",
  "Leguminous crops",
  "Oilseed and oleaginous crops",
  "Root and tuber crops",
  "Sugar",
  "Vegetables and melons",
  "Others"
)

# ---- 5. Reshape nutrient variables to long format ----
nutrients_long <- nut_content %>%
  pivot_longer(
    cols = c(
      `protein in grams per 100g`,
      `lipid/fat in grams per 100g`,
      `energy in kcal per 100g`,
      `carbs in grams per 100g`,
      `vitamin c in grams per 100g`
    ),
    names_to  = "nutrient",
    values_to = "value"
  )

# ---- 6. Compute mean nutrient content by crop group ----
nutrients_summary <- nutrients_long %>%
  group_by(crop_group, nutrient) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    # enforce crop-group order
    crop_group = factor(crop_group, levels = crop_levels),
    # relabel nutrient variable for plotting
    nutrient = factor(
      nutrient,
      levels = c(
        "protein in grams per 100g",
        "lipid/fat in grams per 100g",
        "carbs in grams per 100g",
        "energy in kcal per 100g",
        "vitamin c in grams per 100g"
      ),
      labels = c(
        "Protein (g/100 g)",
        "Lipid (g/100 g)",
        "Carbohydrates (g/100 g)",
        "Energy (kcal/100 g)",
        "Vitamin C (g/100 g)"
      )
    )
  )

# ---- 7. Define colour palette for crop groups ----
crop_palette <- c(
  "Beverage/stimulant/spice/aromatic crops" = "#3C5488",
  "Cereals"                                  = "#E18727",
  "Fruit and nuts"                           = "#91D1C2",
  "Leguminous crops"                         = "#B09C85",
  "Oilseed crops and oleaginous fruits"      = "#E64B35",
  "Root and tuber crops"                     = "#6F99AD",
  "Sugar crops"                              = "#631879",
  "Vegetables and melons"                    = "#00A087",
  "Other crops"                              = "#A20056"
)

# ---- 8. Plot: nutrient content by crop group (vertical facets) ----
p_nutrients <- ggplot(
  nutrients_summary,
  aes(x = crop_group, y = mean_value, fill = crop_group)
) +
  geom_col() +
  facet_wrap(~ nutrient, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = crop_palette, labels = crop_labels) +
  labs(
    title = "Mean nutrient content by crop group",
    x     = "Crop group",
    y     = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text   = element_text(size = 12),
    axis.text.x  = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_blank(),
    legend.position  = "right",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10)
  )

# Display in R session (optional)
# print(p_nutrients)

# ---- 9. Save figure ----
ggsave(
  filename = "fig/Figure_S1.jpg",
  plot     = p_nutrients,
  width    = 7,
  height   = 12,
  units    = "in",
  dpi      = 300
)

############################################################
# End of script
############################################################