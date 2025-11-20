###############################################
# Purpose:
#   - Load plot-level crop and nutrition data
#   - Remove non-relevant (bee / apiculture) observations
#   - Aggregate to the field (country–farm–plot) level
#   - Compute per-hectare yield and nutritional production metrics
#   - Export a cleaned, analysis-ready dataset as an Excel file
###############################################

# ---- 0. Load packages ----
library(readxl)   # for reading Excel files
library(dplyr)    # for data wrangling
library(writexl)  # for writing Excel files

library(dplyr)
library(readxl)
library(writexl)
library(stringr)

###############################################
# ---- 1. Read raw data ----
# Input file: Jun6-2025_edge_density_data.xlsx
# Each row corresponds to a crop observation within a specific plot
field_raw <- read_excel("data/Jun6-2025_edge_density_data.xlsx")

# Inspect structure of the raw dataset
str(field_raw)

# ---- 2. Basic type cleaning ----
# Convert key identifiers to factors for safer grouping/plotting
field <- field_raw %>%
  mutate(
    country_name         = as.factor(country_name),
    farm_group           = as.factor(farm_group),
    farm_number          = as.factor(farm_number),
    location_type        = as.factor(location_type),
    location_id          = as.factor(location_id),
    Crop_name_normalized = as.factor(Crop_name_normalized)
  )

# ---- 3. Remove bee / apiculture observations ----
# These are not directly comparable to crop-based production
field <- field %>%
  filter(!crop_common_name %in% c("ABEJA", "Apicultura"))

# ---- 4. Aggregate to field/plot level ----
# Grouping key:
#   - country_name
#   - farm_group
#   - farm_number
#   - location_id  (plot / field identifier)
#
# For structural/landscape variables (area, edge density, shape index, etc.),
# we assume one unique value per group, so we take the first() value.
# For crop/nutrition variables, we sum across all crops in the plot.

field_data <- field %>%
  group_by(country_name, farm_group, farm_number, location_id) %>%
  summarise(
    # Plot-level structural attributes
    plot_area_ha              = first(`calculated area (ha) using polygon`),
    plot_edge_density_m_ha    = first(`edge density for plots m/ha`),
    plot_shape_index          = first(shape_index),

    # Farm-level structural attributes (same for all plots on a given farm)
    farm_productive_area_ha   = first(productive_area_ha),
    farm_site_boundary_area_ha= first(farm_site_boundary_area_ha),
    farm_edge_density_m_ha    = first(total_farm_edge_density),

    # Crop richness within the plot
    richness                  = n_distinct(Crop_name_normalized, na.rm = TRUE),

    # Total production per plot (summing across all crops)
    total_yield_kg            = sum(total_yield,        na.rm = TRUE),
    total_protein_kg          = sum(total_protein_kg,   na.rm = TRUE),
    total_lipid_kg            = sum(total_lipid_kg,     na.rm = TRUE),
    total_energy_kcal         = sum(total_energy_kcal,  na.rm = TRUE),
    total_carbs_kg            = sum(total_carbs_kg,     na.rm = TRUE),
    total_vit_c_kg            = sum(total_vit_c_kg,     na.rm = TRUE),

    .groups = "drop"
  )

# Optional: quick inspection
# View(field_data)

# ---- 5. Compute per-hectare production metrics ----
# All per-ha metrics are normalized by the plot’s area in hectares
field_data <- field_data %>%
  mutate(
    yield_kg_ha        = total_yield_kg      / plot_area_ha,
    prot_prod_kg_ha    = total_protein_kg    / plot_area_ha,
    lip_prod_kg_ha     = total_lipid_kg      / plot_area_ha,
    ener_prod_kcal_ha  = total_energy_kcal   / plot_area_ha,
    carbs_prod_kg_ha   = total_carbs_kg      / plot_area_ha,
    vitC_prod_kg_ha    = total_vit_c_kg      / plot_area_ha
  )

# ---- 6. Export cleaned dataset ----
# Output file contains one row per (country, farm_group, farm_number, location_id)
# with aggregated and per-hectare metrics, ready for analysis and visualization.

write_xlsx(field_data, path = "data/Aug_field_dataset.xlsx")