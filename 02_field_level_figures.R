# 1. Load Libraries -------------------------------------------------------
library(readxl)
library(ggplot2)
library(cowplot)
library(lme4)
library(dplyr) # Added for cleaner data manipulation

# 2. Setup Directories & Data ---------------------------------------------

# Create 'fig' directory if it doesn't exist
if (!dir.exists("fig")) {
  dir.create("fig")
}

# Load data using a relative path
# Assuming the script is in the project root and data is in a folder named 'data'
# If data is in the same folder as the script, just use the filename.
field <- read_excel("data/Aug_field_dataset.xlsx") 

# Check structure
str(field)

# 3. Data Transformation --------------------------------------------------

# Create log variables using dplyr (safer than attach)
field <- field %>%
  mutate(
    lnarea = log(plot_area_ha),
    lnyield = log(yield_kg_ha),
    lnprot  = log(prot_prod_kg_ha + 1),
    lnlip   = log(lip_prod_kg_ha + 1),
    lnvitC  = log(vitC_prod_kg_ha + 1)
  )

# 4. Modeling (LME4) ------------------------------------------------------

yield_best <- lmer(lnyield ~ lnarea + (1 | country_name/farm_number),
                  na.action = na.fail, data = field)

prot_best  <- lmer(lnprot ~ lnarea + (1 | country_name/farm_number),
                  na.action = na.fail, data = field)

lip_best   <- lmer(lnlip ~ lnarea + (1 | country_name/farm_number),
                  na.action = na.fail, data = field)

vitC_best  <- lmer(lnvitC ~ lnarea + (1 | country_name/farm_number),
                  na.action = na.fail, data = field)

# 5. Predictions ----------------------------------------------------------

# Create sequence for prediction line
new_field <- data.frame(
  lnarea = seq(min(field$lnarea, na.rm = TRUE),
               max(field$lnarea, na.rm = TRUE),
               length.out = 100),
  country_name = NA, 
  farm_number = NA
)

# Predict values (re.form = NA excludes random effects)
new_field$lnyield_pred <- predict(yield_best, newdata = new_field, re.form = NA)
new_field$lnprot_pred  <- predict(prot_best, newdata = new_field, re.form = NA)
new_field$lnlip_pred   <- predict(lip_best, newdata = new_field, re.form = NA)
new_field$lnvit_pred   <- predict(vitC_best, newdata = new_field, re.form = NA)

# 6. Plotting -------------------------------------------------------------

# Define a common theme to avoid code repetition
common_theme <- theme_minimal() +
  theme(
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Theme specifically for top plots (removes X axis labels)
top_plot_theme <- common_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

# A) Yield
yield_area <- ggplot(field, aes(x = lnarea, y = lnyield)) +
  geom_point() +
  geom_line(data = new_field, aes(x = lnarea, y = lnyield_pred), 
            color = "blue", linewidth = 1) +
  labs(y = "Yield (kg/ha)") +
  top_plot_theme

# B) Protein
prot_area <- ggplot(field, aes(x = lnarea, y = lnprot)) +
  geom_point() +
  geom_line(data = new_field, aes(x = lnarea, y = lnprot_pred), 
            color = "blue", linewidth = 1) +
  labs(y = "Protein (kg/ha)") +
  top_plot_theme

# C) Lipids
lip_area <- ggplot(field, aes(x = lnarea, y = lnlip)) +
  geom_point() +
  geom_line(data = new_field, aes(x = lnarea, y = lnlip_pred), 
            color = "blue", linewidth = 1) +
  labs(y = "Lipid (kg/ha)", x = "Field area (ha)") +
  common_theme

# D) Vitamin C
vit_area <- ggplot(field, aes(x = lnarea, y = lnvitC)) +
  geom_point() +
  geom_line(data = new_field, aes(x = lnarea, y = lnvit_pred), 
            color = "blue", linewidth = 1) +
  labs(y = "Vitamin C (kg/ha)", x = "Field area (ha)") +
  common_theme


# 7. Arrange and Save -----------------------------------------------------

fig.2 <- plot_grid(
  yield_area, prot_area, lip_area, vit_area,
  labels = c("a)", "b)", "c)", "d)"),
  label_size = 11, 
  label_y = 1.05,
  ncol = 2, 
  nrow = 2, 
  align = "hv", 
  axis = "tblr"
) + 
  theme(plot.margin = margin(10, 10, 10, 10))

print(fig.2)

# Save to the 'fig' folder using a relative path
ggsave("fig/Figure_2.jpg", fig.2, width = 7, height = 6, units = "in", dpi = 300)