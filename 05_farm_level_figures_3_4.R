############################################################
# Purpose:
#   - Load farm-level dataset
#   - Create log-transformed predictors and responses
#   - Fit mixed models:
#       * response ~ ln_prod_area + (1 | country_name)
#       * response ~ ln_farm_edge + (1 | country_name)
#   - Generate prediction lines from fixed effects only
#   - Plot OBS vs ln(prod_area) and OBS vs ln(farm_edge)
#   - Assemble multi-panel Figures 3 and 4 and save to file
############################################################

# ---- 0. Load packages ----
library(readxl)   # read Excel files
library(dplyr)    # data wrangling
library(lme4)     # mixed models
library(ggplot2)  # plotting
library(cowplot)  # multi-panel figures

# ---- 1. Read farm-level dataset ----
farm_level <- read_excel("data/Aug_farm_level.xlsx")

# Optional: check structure
# str(farm_level)

# ---- 2. Factor conversion and log transforms ----
farm_level <- farm_level %>%
  mutate(
    # categorical variables
    farm_number        = as.factor(farm_number),
    country_name       = as.factor(country_name),
    farm_bound_classes = as.factor(farm_bound_classes),
    prod_area_classes  = as.factor(prod_area_classes),

    # log predictors
    ln_prod_area  = log(farm_productive_area_ha),
    ln_farm_bound = log(farm_boundary_ha),
    ln_farm_edge  = log(farm_edge_density_m_ha),

    # log responses (add 1 where needed to avoid log(0))
    ln_total_rich = log(total_richness),
    ln_yield      = log(mean_yield_kg_ha),
    ln_prot       = log(mean_prot_kg_ha   + 1),
    ln_lip        = log(mean_lip_kg_ha    + 1),
    ln_ener       = log(mean_ener_kcal_ha + 1),
    ln_carb       = log(mean_carb_kg_ha   + 1),
    ln_vitc       = log(mean_vitC_kg_ha   + 1)
  )

############################################################
# FIGURE 3: responses ~ ln_prod_area + (1 | country_name)
############################################################

# ---- 3. Fit models with productive area ----
mod_riq   <- lmer(ln_total_rich ~ ln_prod_area + (1 | country_name),
                  data = farm_level)
mod_yield <- lmer(ln_yield      ~ ln_prod_area + (1 | country_name),
                  data = farm_level)
mod_prot  <- lmer(ln_prot       ~ ln_prod_area + (1 | country_name),
                  data = farm_level)
mod_lip   <- lmer(ln_lip        ~ ln_prod_area + (1 | country_name),
                  data = farm_level)
mod_ener  <- lmer(ln_ener       ~ ln_prod_area + (1 | country_name),
                  data = farm_level)
mod_carb  <- lmer(ln_carb       ~ ln_prod_area + (1 | country_name),
                  data = farm_level)
mod_vit   <- lmer(ln_vitc       ~ ln_prod_area + (1 | country_name),
                  data = farm_level)

# ---- 4. Prediction grid for ln_prod_area (fixed effects only) ----
farm_area <- data.frame(
  ln_prod_area = seq(
    from = min(farm_level$ln_prod_area, na.rm = TRUE),
    to   = max(farm_level$ln_prod_area, na.rm = TRUE),
    length.out = 100
  ),
  country_name = NA  # re.form = NA → fixed-effects only; random effects ignored
)

# Predicted values on log scale
farm_area$lnrich_pred  <- predict(mod_riq,   newdata = farm_area, re.form = NA)
farm_area$lnyield_pred <- predict(mod_yield, newdata = farm_area, re.form = NA)
farm_area$lnprot_pred  <- predict(mod_prot,  newdata = farm_area, re.form = NA)
farm_area$lnlip_pred   <- predict(mod_lip,   newdata = farm_area, re.form = NA)
farm_area$lnener_pred  <- predict(mod_ener,  newdata = farm_area, re.form = NA)
farm_area$lncarb_pred  <- predict(mod_carb,  newdata = farm_area, re.form = NA)
farm_area$lnvit_pred   <- predict(mod_vit,   newdata = farm_area, re.form = NA)

# ---- 5. Helper for consistent plot theme ----
base_area_theme <- theme_minimal() +
  theme(
    axis.title.y    = element_text(size = 11),
    axis.title.x    = element_blank(),
    axis.text.x     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black")
  )

base_area_theme_x <- theme_minimal() +
  theme(
    axis.title.y    = element_text(size = 11),
    axis.title.x    = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black")
  )

# ---- 6. Individual panels: response vs ln_prod_area ----

# (a) Richness
rich_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_total_rich)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lnrich_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Richness (n°)", x = "Farm productive area (ha)") +
  base_area_theme +
  annotate(
    "text",
    x     = max(farm_level$ln_prod_area, na.rm = TRUE),
    y     = max(farm_level$ln_total_rich, na.rm = TRUE),
    label = "NS",
    hjust = 1, vjust = 0, size = 3
  )

# (b) Yield
yield_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_yield)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lnyield_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Yield (kg/ha)", x = "Farm productive area (ha)") +
  base_area_theme

# (c) Protein
prot_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_prot)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lnprot_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Protein (kg/ha)", x = "Farm productive area (ha)") +
  base_area_theme

# (d) Lipids
lip_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_lip)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lnlip_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Lipid (kg/ha)", x = "Farm productive area (ha)") +
  base_area_theme +
  annotate(
    "text",
    x     = max(farm_level$ln_prod_area, na.rm = TRUE),
    y     = max(farm_level$ln_lip, na.rm = TRUE),
    label = "NS",
    hjust = 1, vjust = 0, size = 3
  )

# (e) Energy
ener_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_ener)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lnener_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Energy (kcal/ha)", x = "Farm productive area (ha)") +
  base_area_theme_x +
  annotate(
    "text",
    x     = max(farm_level$ln_prod_area, na.rm = TRUE),
    y     = max(farm_level$ln_ener, na.rm = TRUE),
    label = "NS",
    hjust = 1, vjust = 0, size = 3
  )

# (f) Carbohydrates
carbs_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_carb)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lncarb_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Carbohydrate (kg/ha)", x = "Farm productive area (ha)") +
  base_area_theme_x +
  annotate(
    "text",
    x     = max(farm_level$ln_prod_area, na.rm = TRUE),
    y     = max(farm_level$ln_carb, na.rm = TRUE),
    label = "NS",
    hjust = 1, vjust = 0, size = 3
  )

# (g) Vitamin C
vit_area <- ggplot(farm_level, aes(x = ln_prod_area, y = ln_vitc)) +
  geom_point() +
  geom_line(
    data = farm_area,
    aes(x = ln_prod_area, y = lnvit_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Vitamin C (kg/ha)", x = "Farm productive area (ha)") +
  base_area_theme_x

# ---- 7. Assemble Figure 3 ----
fig_3 <- plot_grid(
  rich_area, yield_area, prot_area,
  lip_area, ener_area, carbs_area, vit_area,
  labels     = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"),
  label_size = 11,
  label_y    = 1.05,
  ncol       = 3,
  nrow       = 3,
  align      = "hv",
  axis       = "tblr"
) +
  theme(plot.margin = margin(10, 10, 10, 10))

# Print in R session (optional)
# fig_3

# Save Figure 3 (adjust path if needed)
ggsave(
  filename = "fig/Figure_3.jpg",
  plot     = fig_3,
  width    = 9,
  height   = 7,
  units    = "in",
  dpi      = 300
)

############################################################
# FIGURE 4: responses ~ ln_farm_edge + (1 | country_name)
############################################################

# ---- 8. Fit models with edge density ----
mod_riq_edge   <- lmer(ln_total_rich ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)
mod_yield_edge <- lmer(ln_yield      ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)
mod_prot_edge  <- lmer(ln_prot       ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)
mod_lip_edge   <- lmer(ln_lip        ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)
mod_ener_edge  <- lmer(ln_ener       ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)
mod_carb_edge  <- lmer(ln_carb       ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)
mod_vit_edge   <- lmer(ln_vitc       ~ ln_farm_edge + (1 | country_name),
                       data = farm_level)

# ---- 9. Prediction grid for ln_farm_edge (fixed effects only) ----
farm_edge <- data.frame(
  ln_farm_edge = seq(
    from = min(farm_level$ln_farm_edge, na.rm = TRUE),
    to   = max(farm_level$ln_farm_edge, na.rm = TRUE),
    length.out = 100
  ),
  country_name = NA
)

# Predicted y (log scale)
farm_edge$lnrich2_pred  <- predict(mod_riq_edge,   newdata = farm_edge, re.form = NA)
farm_edge$lnyield2_pred <- predict(mod_yield_edge, newdata = farm_edge, re.form = NA)
farm_edge$lnprot2_pred  <- predict(mod_prot_edge,  newdata = farm_edge, re.form = NA)
farm_edge$lnlip2_pred   <- predict(mod_lip_edge,   newdata = farm_edge, re.form = NA)
farm_edge$lnener2_pred  <- predict(mod_ener_edge,  newdata = farm_edge, re.form = NA)
farm_edge$lncarb2_pred  <- predict(mod_carb_edge,  newdata = farm_edge, re.form = NA)
farm_edge$lnvit2_pred   <- predict(mod_vit_edge,   newdata = farm_edge, re.form = NA)

# ---- 10. Theming for edge plots ----
base_edge_theme <- theme_minimal() +
  theme(
    axis.title.y    = element_text(size = 11),
    axis.title.x    = element_blank(),
    axis.text.x     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black")
  )

base_edge_theme_x <- theme_minimal() +
  theme(
    axis.title.y    = element_text(size = 11),
    axis.title.x    = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black")
  )

# ---- 11. Individual panels: response vs ln_farm_edge ----

# (a) Richness
rich_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_total_rich)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lnrich2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Richness (n°)", x = "Edge density (m/ha)") +
  base_edge_theme

# (b) Yield
yield_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_yield)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lnyield2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Yield (kg/ha)", x = "Edge density (m/ha)") +
  base_edge_theme

# (c) Protein
prot_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_prot)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lnprot2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Protein (kg/ha)", x = "Edge density (m/ha)") +
  base_edge_theme

# (d) Lipids
lip_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_lip)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lnlip2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Lipid (kg/ha)", x = "Edge density (m/ha)") +
  base_edge_theme

# (e) Energy
ener_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_ener)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lnener2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Energy (kcal/ha)", x = "Edge density (m/ha)") +
  base_edge_theme_x

# (f) Carbohydrates
carbs_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_carb)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lncarb2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Carbohydrate (kg/ha)", x = "Edge density (m/ha)") +
  base_edge_theme_x

# (g) Vitamin C
vit_edge <- ggplot(farm_level, aes(x = ln_farm_edge, y = ln_vitc)) +
  geom_point() +
  geom_line(
    data = farm_edge,
    aes(x = ln_farm_edge, y = lnvit2_pred),
    color = "blue",
    linewidth = 1
  ) +
  labs(y = "Vitamin C (kg/ha)", x = "Edge density (m/ha)") +
  base_edge_theme_x

# ---- 12. Assemble Figure 4 ----
fig_4 <- plot_grid(
  rich_edge, yield_edge, prot_edge,
  lip_edge,  ener_edge,  carbs_edge, vit_edge,
  labels     = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"),
  label_size = 11,
  label_y    = 1.05,
  ncol       = 3,
  nrow       = 3,
  align      = "hv",
  axis       = "tblr"
) +
  theme(plot.margin = margin(10, 10, 10, 10))

# Print in R session (optional)
# fig_4

# Save Figure 4 (adjust path if needed)
ggsave(
  filename = "fig/Figure_4.jpg",
  plot     = fig_4,
  width    = 9,
  height   = 7,
  units    = "in",
  dpi      = 300
)

############################################################
# End of script
############################################################