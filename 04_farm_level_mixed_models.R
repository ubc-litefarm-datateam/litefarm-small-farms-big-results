############################################################
# Purpose:
#   - Load farm-level dataset
#   - Create log-transformed response and predictor variables
#   - Explore correlation between productive area and edge density
#   - Fit linear mixed-effects models (LMMs) with country as a random effect:
#       * Richness, yield, and nutritional metrics ~ productive area
#       * Richness, yield, and nutritional metrics ~ farm edge density
#   - Produce basic diagnostic plots for each model
############################################################

# ---- 0. Load packages ----
library(readxl)  # read Excel files
library(dplyr)   # data wrangling
library(lme4)    # linear mixed-effects models

# ---- 1. Read farm-level dataset ----
farm_level <- read_excel("data/Aug_farm_level.xlsx")

# Optional: inspect structure
# str(farm_level)

# ---- 2. Factor conversion and log transforms ----
farm_level <- farm_level %>%
  mutate(
    # identifiers and grouping factors
    farm_number        = as.factor(farm_number),
    country_name       = as.factor(country_name),

    # log-transformed predictors
    ln_prod_area  = log(farm_productive_area_ha),
    ln_farm_edge  = log(farm_edge_density_m_ha),

    # log-transformed responses
    ln_total_rich = log(total_richness),
    ln_yield      = log(mean_yield_kg_ha),
    ln_prot       = log(mean_prot_kg_ha   + 1),
    ln_lip        = log(mean_lip_kg_ha    + 1),
    ln_ener       = log(mean_ener_kcal_ha + 1),
    ln_carb       = log(mean_carb_kg_ha   + 1),
    ln_vitc       = log(mean_vitC_kg_ha   + 1)
  )

# Optional: check transformed structure
# str(farm_level)

############################################################
# 2. Correlation between productive area and edge density
############################################################

cor_data <- farm_level %>%
  select(ln_prod_area, ln_farm_edge)

cor(cor_data, method = "pearson")

# Quick visual checks
plot(
  farm_level$ln_prod_area,
  farm_level$ln_farm_edge,
  xlab = "log(Productive area, ha)",
  ylab = "log(Farm edge density, m/ha)",
  main = "ln(Productive area) vs ln(Edge density)"
)

plot(
  farm_level$farm_productive_area_ha,
  farm_level$farm_edge_density_m_ha,
  xlab = "Productive area (ha)",
  ylab = "Farm edge density (m/ha)",
  main = "Productive area vs edge density"
)

############################################################
# 3. Helper function: fit LMM and produce diagnostics
############################################################

fit_lmm_with_diagnostics <- function(resp, pred, data,
                                      resp_label, pred_label,
                                      main_prefix = "") {
  # Build formula: resp ~ pred + (1|country_name)
  form <- as.formula(paste(resp, "~", pred, "+ (1|country_name)"))

  cat("\n============================================\n")
  cat("Model:", deparse(form), "\n")
  cat("============================================\n\n")

  # Fit model
  mod <- lmer(form, data = data)
  print(summary(mod))

  # Residuals and fitted values
  res <- resid(mod, scaled = TRUE)
  fit <- fitted(mod)

  # 1) Observed vs predicted
  plot(
    x = fit, y = data[[resp]],
    xlab = "Predicted",
    ylab = "Observed",
    main = paste(main_prefix, resp_label, "~", pred_label, "\nObserved vs Predicted")
  )
  abline(0, 1, col = "red", lwd = 3)

  # 2) Residuals vs fitted
  plot(
    x = fit, y = res,
    xlab = "Fitted values",
    ylab = "Standardized residuals",
    main = paste(main_prefix, resp_label, "~", pred_label, "\nResiduals vs Fitted")
  )
  abline(0, 0, col = "red", lwd = 3)

  # 3) Normal Q–Q plot of residuals
  qqnorm(res, main = paste(main_prefix, resp_label, "~", pred_label, "\nNormal Q–Q Plot"))
  qqline(res, col = "red", lwd = 2)

  # 4) Residuals vs predictor (for homoscedasticity)
  plot(
    x = data[[pred]], y = res,
    xlab = pred_label,
    ylab = "Standardized residuals",
    main = paste(main_prefix, resp_label, "~", pred_label, "\nResiduals vs", pred_label)
  )
  abline(0, 0, col = "red", lwd = 3)

  invisible(mod)
}

############################################################
# 4. Models with PRODUCTIVE AREA as predictor
#    response ~ ln_prod_area + (1 | country_name)
############################################################

# Richness
mod_riq_area <- fit_lmm_with_diagnostics(
  resp       = "ln_total_rich",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Total richness)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

# Yield
mod_yield_area <- fit_lmm_with_diagnostics(
  resp       = "ln_yield",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Mean yield, kg/ha)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

# Protein
mod_prot_area <- fit_lmm_with_diagnostics(
  resp       = "ln_prot",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Mean protein production, kg/ha + 1)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

# Lipids
mod_lip_area <- fit_lmm_with_diagnostics(
  resp       = "ln_lip",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Mean lipid production, kg/ha + 1)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

# Energy
mod_ener_area <- fit_lmm_with_diagnostics(
  resp       = "ln_ener",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Mean energy production, kcal/ha + 1)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

# Carbohydrates
mod_carb_area <- fit_lmm_with_diagnostics(
  resp       = "ln_carb",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Mean carbohydrate production, kg/ha + 1)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

# Vitamin C
mod_vit_area <- fit_lmm_with_diagnostics(
  resp       = "ln_vitc",
  pred       = "ln_prod_area",
  data       = farm_level,
  resp_label = "log(Mean vitamin C production, kg/ha + 1)",
  pred_label = "log(Productive area, ha)",
  main_prefix = "Productive area model:"
)

############################################################
# 5. Models with FARM EDGE DENSITY as predictor
#    response ~ ln_farm_edge + (1 | country_name)
############################################################

# Richness
mod_riq_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_total_rich",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Total richness)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

# Yield
mod_yield_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_yield",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Mean yield, kg/ha)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

# Protein
mod_prot_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_prot",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Mean protein production, kg/ha + 1)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

# Lipids
mod_lip_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_lip",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Mean lipid production, kg/ha + 1)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

# Energy
mod_ener_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_ener",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Mean energy production, kcal/ha + 1)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

# Carbohydrates
mod_carb_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_carb",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Mean carbohydrate production, kg/ha + 1)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

# Vitamin C
mod_vit_edge <- fit_lmm_with_diagnostics(
  resp       = "ln_vitc",
  pred       = "ln_farm_edge",
  data       = farm_level,
  resp_label = "log(Mean vitamin C production, kg/ha + 1)",
  pred_label = "log(Farm edge density, m/ha)",
  main_prefix = "Edge-density model:"
)

############################################################
# End of script
############################################################