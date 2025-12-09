## Project Overview

This repository contains the R scripts, cleaned datasets, and figures used in the analysis for:

***“Bigger isn’t better: Small productive lands and edge-rich systems outperform larger systems in crop diversity and productivity.”***

Anahí R. Fernandez, Hannah Wittman, Khanh Dao Duc, Riddhi Battu, Ilyas Siddique, Zia Mehrabi, Martina Propedo, & Lucas A. Garibaldi

The analyses use anonymized LiteFarm plot- and farm-level data to evaluate how productive area and landscape structure relate to crop richness, yield, and nutritional outputs.

---

## Repository Structure

```
ANAHI_PAPER_R_SCRIPTS/
│
├── data/                 # Cleaned, anonymized datasets used in the analysis
│   ├── Aug_crop_groups.xlsx
│   ├── Aug_crop_groups_mean.xlsx
│   ├── Aug_farm_level.xlsx
│   ├── Aug_field_dataset.xlsx
│   └── Jun6-2025_edge_density_data.xlsx
│
├── fig/                  # Exported figures for the manuscript
│   ├── Figure_3.jpg
│   ├── Figure_4.jpg
│   ├── Figure_5.jpg
│   └── Figure_S1.jpg
│
├── 01_field_edge_density_aggregation.R
├── 02_farm_level_aggregation.R
├── 03_nutrient_content_by_crop_group.R
├── 04_farm_level_mixed_models.R
├── 05_farm_level_figures_3_4.R
├── 06_crop_groups_by_farm_size_figure_5.R
└── 07_field_level_models.R
```

---

## Requirements

These scripts were written and tested using:

* **R ≥ 4.3**
* Key packages:

  * `dplyr`
  * `tidyr`
  * `ggplot2`
  * `readxl`
  * `writexl`
  * `lme4`
  * `MuMIn`
  * `cowplot`

You can install dependencies manually, e.g.:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "readxl",
                   "writexl", "lme4", "MuMIn", "cowplot"))
```

---

## Running the Analysis

Scripts are numbered in logical order:

1. **01_field_edge_density_aggregation.R**
   Aggregates raw plot-level data and generates the cleaned field-level dataset.

2. **02_farm_level_aggregation.R**
   Aggregates field-level data to farm level.

3. **03_nutrient_content_by_crop_group.R**
   Computes nutrient profiles by crop groups and generates Figure S1.

4. **04_farm_level_mixed_models.R**
   Fits mixed models exploring relationships between productive area and outcomes.

5. **05_farm_level_figures_3_4.R**
   Generates Figures 3 and 4.

6. **06_crop_groups_by_farm_size_figure_5.R**
   Computes proportional crop groups by farm size and generates Figure 5.

7. **07_field_level_models.R**
   Field-level mixed models evaluating area, shape, and edge effects.

---

## Data Privacy

All datasets included here are **anonymized**.
Identifiers such as farm names, location names, and internal LiteFarm IDs have been removed or replaced with non-identifiable labels.

Raw LiteFarm data **is not included** in this repository.

---

## Citation

If you use these scripts, please cite the accompanying manuscript once published.

---
