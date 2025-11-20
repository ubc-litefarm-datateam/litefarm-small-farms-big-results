############################################################
# Purpose:
#   - Load plot-level dataset (Aug_field_dataset.xlsx)
#   - Create log-transformed predictors and responses
#   - For each response (richness, yield, nutrients):
#       * Fit a full LMM with ln(area), ln(shape), and their interaction
#         and nested random effects (country / farm)
#       * Use MuMIn::dredge to explore model selection
#       * Fit the selected "best" model (as chosen by AIC)
#       * Produce basic diagnostic plots (residuals, QQ plot, etc.)
#   - Then fit simpler models with ln(edge density) as the sole fixed effect
############################################################

# ---- 0. Load packages ----
library(readxl)  # read Excel files
library(lme4)    # linear mixed-effects models
library(MuMIn)   # dredge() for model selection

# ---- 1. Read data ----
field <- read_excel("data/Aug_field_dataset.xlsx")

# Optional: inspect structure
# str(field)

# ---- 2. Convert grouping vars to factors & create log transforms ----
field <- field %>%
  dplyr::mutate(
    country_name = as.factor(country_name),
    farm_number  = as.factor(farm_number),

    # log predictors
    lnarea  = log(plot_area_ha),
    lnshape = log(plot_shape_index),
    lnedge  = log(plot_edge_density_m_ha),

    # log responses (add 1 where needed to avoid log(0))
    lnrich   = log(richness),
    lnyield  = log(yield_kg_ha),
    lnprot   = log(prot_prod_kg_ha   + 1),
    lnlip    = log(lip_prod_kg_ha    + 1),
    lnener   = log(ener_prod_kcal_ha + 1),
    lncarbs  = log(carbs_prod_kg_ha  + 1),
    lnvitC   = log(vitC_prod_kg_ha   + 1)
  )

# MuMIn::dredge requires na.action = "na.fail" for the global model
options(na.action = "na.fail")

############################################################
# 2. Helper: diagnostics for a fitted LMM
############################################################

diagnostic_plots <- function(model, data, resp_var, x_for_resid, x_label = "") {
  # Standardized residuals and fitted values
  res <- resid(model, scaled = TRUE)
  fit <- fitted(model)

  y_obs <- data[[resp_var]]
  x_res <- data[[x_for_resid]]

  # 1) Observed vs Predicted
  plot(
    x = fit, y = y_obs,
    xlab = "Predicted",
    ylab = "Observed",
    main = paste(resp_var, "~", x_for_resid, "\nObserved vs Predicted")
  )
  abline(0, 1, col = "red", lwd = 3)

  # 2) Residuals vs Fitted
  plot(
    x = fit, y = res,
    xlab = "Fitted values",
    ylab = "Standardized residuals",
    main = paste(resp_var, "~", x_for_resid, "\nResiduals vs Fitted")
  )
  abline(0, 0, col = "red", lwd = 3)

  # 3) Normal Q–Q plot
  qqnorm(res, main = paste(resp_var, "~", x_for_resid, "\nNormal Q–Q plot"))
  qqline(res, col = "red", lwd = 2)

  # 4) Residuals vs selected predictor (for homoscedasticity)
  plot(
    x = x_res, y = res,
    xlab = x_label,
    ylab = "Standardized residuals",
    main = paste("Residuals vs", x_label)
  )
  abline(0, 0, col = "red", lwd = 3)

  invisible(NULL)
}

############################################################
# 3. Global models with ln(area) and ln(shape) + dredge
#    Response ~ lnarea * lnshape + (1 | country_name / farm_number)
############################################################

# ---- 3.1 Richness ----
mod_rich_full <- lmer(
  lnrich ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

rich_dredge <- dredge(mod_rich_full)
print(rich_dredge)

# Best model chosen: lnrich ~ lnshape + (1 | country_name / farm_number)
mod_rich_best <- lmer(
  lnrich ~ lnshape + (1 | country_name / farm_number),
  data = field
)
summary(mod_rich_best)

diagnostic_plots(
  model      = mod_rich_best,
  data       = field,
  resp_var   = "lnrich",
  x_for_resid = "lnshape",
  x_label    = "Shape index (log)"
)

# ---- 3.2 Yield ----
mod_yield_full <- lmer(
  lnyield ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

yield_dredge <- dredge(mod_yield_full)
print(yield_dredge)

# Best model chosen: lnyield ~ lnarea + (1 | country_name / farm_number)
mod_yield_best <- lmer(
  lnyield ~ lnarea + (1 | country_name / farm_number),
  data = field
)
summary(mod_yield_best)

diagnostic_plots(
  model      = mod_yield_best,
  data       = field,
  resp_var   = "lnyield",
  x_for_resid = "lnarea",
  x_label    = "Plot area (log ha)"
)

# ---- 3.3 Protein production ----
mod_prot_full <- lmer(
  lnprot ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

prot_dredge <- dredge(mod_prot_full)
print(prot_dredge)

# Best model chosen: lnprot ~ lnarea + (1 | country_name / farm_number)
mod_prot_best <- lmer(
  lnprot ~ lnarea + (1 | country_name / farm_number),
  data = field
)
summary(mod_prot_best)

diagnostic_plots(
  model      = mod_prot_best,
  data       = field,
  resp_var   = "lnprot",
  x_for_resid = "lnarea",
  x_label    = "Plot area (log ha)"
)

# ---- 3.4 Lipid production ----
mod_lip_full <- lmer(
  lnlip ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

lip_dredge <- dredge(mod_lip_full)
print(lip_dredge)

# Best model chosen: lnlip ~ lnarea + (1 | country_name / farm_number)
mod_lip_best <- lmer(
  lnlip ~ lnarea + (1 | country_name / farm_number),
  data = field
)
summary(mod_lip_best)

diagnostic_plots(
  model      = mod_lip_best,
  data       = field,
  resp_var   = "lnlip",
  x_for_resid = "lnarea",
  x_label    = "Plot area (log ha)"
)

# ---- 3.5 Energy production ----
mod_ener_full <- lmer(
  lnener ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

ener_dredge <- dredge(mod_ener_full)
print(ener_dredge)

# Best model chosen: lnener ~ lnarea + (1 | country_name / farm_number)
mod_ener_best <- lmer(
  lnener ~ lnarea + (1 | country_name / farm_number),
  data = field
)
summary(mod_ener_best)

diagnostic_plots(
  model      = mod_ener_best,
  data       = field,
  resp_var   = "lnener",
  x_for_resid = "lnarea",
  x_label    = "Plot area (log ha)"
)

# ---- 3.6 Carbohydrate production ----
mod_carbs_full <- lmer(
  lncarbs ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

carbs_dredge <- dredge(mod_carbs_full)
print(carbs_dredge)

# Best model chosen: lncarbs ~ lnshape + (1 | country_name / farm_number)
mod_carbs_best <- lmer(
  lncarbs ~ lnshape + (1 | country_name / farm_number),
  data = field
)
summary(mod_carbs_best)

diagnostic_plots(
  model      = mod_carbs_best,
  data       = field,
  resp_var   = "lncarbs",
  x_for_resid = "lnshape",
  x_label    = "Shape index (log)"
)

# ---- 3.7 Vitamin C production ----
mod_vitC_full <- lmer(
  lnvitC ~ lnarea * lnshape + (1 | country_name / farm_number),
  data = field
)

vitC_dredge <- dredge(mod_vitC_full)
print(vitC_dredge)

# Best model chosen: lnvitC ~ lnarea + (1 | country_name / farm_number)
mod_vitC_best <- lmer(
  lnvitC ~ lnarea + (1 | country_name / farm_number),
  data = field
)
summary(mod_vitC_best)

diagnostic_plots(
  model      = mod_vitC_best,
  data       = field,
  resp_var   = "lnvitC",
  x_for_resid = "lnarea",
  x_label    = "Plot area (log ha)"
)

############################################################
# 4. Models with edge density as the only fixed effect
#    response ~ lnedge + (1 | country_name / farm_number)
############################################################

mod_rich_edge  <- lmer(lnrich  ~ lnedge + (1 | country_name / farm_number), data = field)
mod_yield_edge <- lmer(lnyield ~ lnedge + (1 | country_name / farm_number), data = field)
mod_prot_edge  <- lmer(lnprot  ~ lnedge + (1 | country_name / farm_number), data = field)
mod_lip_edge   <- lmer(lnlip   ~ lnedge + (1 | country_name / farm_number), data = field)
mod_ener_edge  <- lmer(lnener  ~ lnedge + (1 | country_name / farm_number), data = field)
mod_carbs_edge <- lmer(lncarbs ~ lnedge + (1 | country_name / farm_number), data = field)
mod_vitC_edge  <- lmer(lnvitC  ~ lnedge + (1 | country_name / farm_number), data = field)

summary(mod_rich_edge)
summary(mod_yield_edge)
summary(mod_prot_edge)
summary(mod_lip_edge)
summary(mod_ener_edge)
summary(mod_carbs_edge)
summary(mod_vitC_edge)

############################################################
# End of script
############################################################