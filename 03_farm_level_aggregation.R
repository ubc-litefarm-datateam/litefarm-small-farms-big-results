############################################################
# Purpose:
#   - Load plot-level (field) dataset
#   - Aggregate metrics to the farm level
#   - Classify farms by boundary and productive area
#   - Derive mean per-location nutritional/yield metrics
#   - Export a farm-level dataset as an Excel file
############################################################

# ---- 0. Load packages ----
library(readxl)   # read Excel files
library(dplyr)    # data wrangling
library(writexl)  # write Excel files

# ---- 1. Read field-level dataset ----
field <- read_excel("data/Aug_field_dataset.xlsx")

# Optional: inspect structure
# str(field)

# ---- 2. Convert key identifiers to factors ----
field <- field %>%
  mutate(
    across(
      c(farm_number, farm_group, country_name, location_id),
      as.factor
    )
  )

# ---- 3. Aggregate to farm level ----
# Grouping key:
#   - farm_number
#   - farm_group
#   - country_name
#
# We assume farm-level structural variables (edge density, areas) are constant
# within a farm, so we use first(). Plot-level metrics are summed across
# locations, then later averaged by the number of locations per farm.

farm_level <- field %>%
  group_by(farm_number, farm_group, country_name) %>%
  summarise(
    farm_edge_density_m_ha  = first(farm_edge_density_m_ha),
    farm_productive_area_ha = first(farm_productive_area_ha),

    n_locations             = n_distinct(location_id),
    total_richness          = sum(richness, na.rm = TRUE),

    total_yield_kg_ha       = sum(yield_kg_ha,      na.rm = TRUE),
    total_prot_prod         = sum(prot_prod_kg_ha,  na.rm = TRUE),
    total_lip_prod          = sum(lip_prod_kg_ha,   na.rm = TRUE),
    total_ener_prod         = sum(ener_prod_kcal_ha,na.rm = TRUE),
    total_carbs_prod        = sum(carbs_prod_kg_ha, na.rm = TRUE),
    total_vitC_prod         = sum(vitC_prod_kg_ha,  na.rm = TRUE),

    .groups = "drop"
  )

# Optional: inspect
# View(farm_level)

# ---- 4. Classify farms by area and compute mean per-location metrics ----
# Area classes based on Riddhi's thresholds:
#   - Area classes based on quartiles sample distribution
#   - prod_area_classes:  uses farm_productive_area_ha

farm_level <- farm_level %>%
  mutate(
    # Productive area classes
    prod_area_classes = case_when(
      farm_productive_area_ha < 3.5                              ~ "small",
      farm_productive_area_ha >= 3.5 & farm_productive_area_ha <= 15 ~ "medium",
      farm_productive_area_ha > 15                               ~ "large",
      TRUE                                                       ~ NA_character_
    ),

    # Mean per-location metrics (averaging over locations on each farm)
    mean_yield_kg_ha    = total_yield_kg_ha   / n_locations,
    mean_prot_kg_ha     = total_prot_prod     / n_locations,
    mean_lip_kg_ha      = total_lip_prod      / n_locations,
    mean_ener_kcal_ha   = total_ener_prod     / n_locations,
    mean_carb_kg_ha     = total_carbs_prod    / n_locations,
    mean_vitC_kg_ha     = total_vitC_prod     / n_locations
  )

# Optional: inspect final table
# View(farm_level)

# ---- 5. Export farm-level dataset ----
write_xlsx(farm_level, path = "data/Aug_farm_level.xlsx")

############################################################
# End of script
############################################################
