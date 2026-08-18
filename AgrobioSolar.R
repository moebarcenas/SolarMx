################################################################################
#####---------- Solar Park AgroBiodiversity Analysis---------------#####

#This script performs a comprehensive analysis of solar park impacts on 
# agrobiodiversity, including:
#   1. Land cover analysis and visualization
#   2. Biodiversity impact scoring for agave and maize species
#   4. Geographical and Environmental variable extraction using GIS buffers
#   5. Generalized Linear Model (GLM) for presence prediction
#
# Author: Monica Barcenas-Pazos, MHEI, Freie Universität Berlin
# Date: 08.2026

################################################################################

# ==============================================================================
#####---------- PART 1: LAND COVER ANALYSIS--------------#####
# ==============================================================================

# Load libraries
library(tidyverse)
library(ggplot2)

# Read the dataset
data <- read_csv("H:/LUC_Sparks.csv")

# View first few rows
head(data)

# Replace NA with 0 in land cover columns
data <- data %>%
  mutate(
    Grassland = replace_na(Grassland, 0),
    Shrubland = replace_na(Shrubland, 0),
    Forest = replace_na(Forest, 0),
    Agro = replace_na(Agro, 0),
    Natural = Grassland + Shrubland + Forest,
    Transformed = Agro,
    Total_Converted = Natural + Agro
  )


####------- Plot 1: Total Land Cover Affected (Bar Chart) ----------####
total_cover <- data %>%
  summarise(
    Grassland = sum(Grassland),
    Shrubland = sum(Shrubland),
    Forest = sum(Forest),
    Agro = sum(Agro)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Land_Cover", values_to = "Area")

ggplot(total_cover, aes(x = Land_Cover, y = Area, fill = Land_Cover)) +
  geom_col() +
  labs(title = "Total Land Cover Affected by Solar Parks", x = NULL, y = "Area (km²)") +
  theme_minimal()

####------- Plot 2: Grouped Bar Chart per Park ----------####
data_long <- data %>%
  select(ID, Grassland, Shrubland, Forest, Agro) %>%
  pivot_longer(cols = -ID, names_to = "Land_Cover", values_to = "Area")

ggplot(data_long, aes(x = reorder(ID, -Area), y = Area, fill = Land_Cover)) +
  geom_col(position = "dodge") +
  labs(title = "Land Cover Affected per Solar Park", x = "Solar Park", y = "Area (km²)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####------- Plot 3: Scatter Plot – Park Size vs Natural Area ----------####
ggplot(data, aes(x = Total_Area, y = Natural)) +
  geom_point(color = "forestgreen") +
  geom_text(aes(label = ID), hjust = 0, vjust = 1, size = 2.5, check_overlap = TRUE) +
  labs(title = "Total Park Area vs Natural Vegetation Affected",
       x = "Total Park Area (km²)", y = "Natural Vegetation Affected (km²)") +
  theme_minimal()

####------- Plot 4: % of Land Cover per Park (Stacked % Bar) ----------####
data <- data %>%
  mutate(
    pct_grassland = round((Grassland / Total_Area) * 100, 2),
    pct_shrubland = round((Shrubland / Total_Area) * 100, 2),
    pct_forest = round((Forest / Total_Area) * 100, 2),
    pct_agro = round((Agro / Total_Area) * 100, 2)
  )

data_pct <- data %>%
  select(ID, pct_grassland, pct_shrubland, pct_forest, pct_agro) %>%
  pivot_longer(cols = -ID, names_to = "Land_Cover", values_to = "Percentage")

ggplot(data_pct, aes(x = reorder(ID, -Percentage), y = Percentage, fill = Land_Cover)) +
  geom_col(position = "stack") +
  labs(title = "Percentage of Land Cover Types per Solar Park", x = "Solar Park", y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####------- Plot 5: Number of Ecosystems Affected per Park ----------####
data <- data %>%
  mutate(
    Ecosystems_Affected = (Grassland > 0) + (Shrubland > 0) + (Forest > 0) + (Agro > 0)
  )

ggplot(data, aes(x = reorder(ID, -Ecosystems_Affected), y = Ecosystems_Affected, fill = Ecosystems_Affected)) +
  geom_col() +
  scale_y_continuous(breaks = 0:4) +
  labs(title = "Number of Land Cover Types Affected per Solar Park",
       x = "Solar Park", y = "Types Affected") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####------- Plot 6: % of Natural + Agro Area vs Park Size ----------####
data <- data %>%
  mutate(
    pct_natural = round((Natural / Total_Area) * 100, 2),
    pct_agro = round((Agro / Total_Area) * 100, 2)
  )

ggplot(data, aes(x = Total_Area, y = pct_natural + pct_agro)) +
  geom_point(color = "darkorange") +
  geom_text(aes(label = ID), size = 2.5, vjust = 1, hjust = 0, check_overlap = TRUE) +
  labs(title = "% of Natural + Agro Area vs Park Size",
       x = "Total Park Area (km²)", y = "% of Area from Natural + Agro") +
  theme_minimal()

####------- Summary statistics for all numeric columns ----------####
summary(data)

####------- Total number of solar parks ----------####
nrow(data)

####------- Total land cover conversion ----------####
data %>%
  summarise(
    total_grass = sum(Grassland, na.rm = TRUE),
    total_shrub = sum(Shrubland, na.rm = TRUE),
    total_forest = sum(Forest, na.rm = TRUE),
    total_agro = sum(Agro, na.rm = TRUE),
    total_vegetation = sum(Natural, na.rm = TRUE),
    total_converted = sum(Natural + Agro, na.rm = TRUE)
  )


# ==============================================================================
######----------  PART 2: AGROBIODIVERSITY IMPACT ANALYSIS ----------------######
# ==============================================================================

# Load required packages
library(tidyverse)
library(scales)

####------- Load the CSV data ----------####
data <- read.csv("H:/biod_spark.csv")

####------- Replace NAs with 0 (if any) ----------####
data[is.na(data)] <- 0

head(data)

####------- Normalize agave and maize values (0–1 scale) ----------####
data <- data %>%
  mutate(
    norm_agave = N.Agave / max(N.Agave, na.rm = TRUE),
    norm_maize = Maiz / max(Maiz), na.rm = TRUE)

####------- Create Impact Score (you can change weights as needed) ----------####
data <- data %>%
  mutate(
    Impact_Score = 0.6 * norm_agave + 0.4 * norm_maize
  )

####------- View summary stats ----------####
summary(data$Impact_Score)

####------- Plot 1: Impact Score distribution ----------####
ggplot(data, aes(x = Impact_Score)) +
  geom_histogram(bins = 20, fill = "darkgreen", color = "black") +
  labs(title = "Biodiversity Impact Score Distribution",
       x = "Impact Score (0–1)", y = "Number of Solar Parks") +
  theme_minimal()

####------- Plot 2: Scatter of Agave vs Maize richness ----------####
ggplot(data, aes(x = N.Agave, y = Maiz)) +
  geom_jitter(width = 0.3, height = 0.3, shape = 21, fill = "gray40", color = "black", size = 3) +
  labs(title = "Agave vs. Maize Diversity Overlap",
       x = "Agave Species Overlapping", y = "Maize Varieties") +
  theme_minimal()

####------- Plot 3: Impact Score per Park (sorted) ----------####
data_sorted <- data %>% arrange(desc(Impact_Score))
ggplot(data_sorted, aes(x = reorder(ID, -Impact_Score), y = Impact_Score)) +
  geom_col(fill = "gray40") +
  labs(title = "Impact Score per Solar Park",
       x = "Park ID", y = "Impact Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####------- Export results ----------####
write.csv(data, "H:/biodiversity_impact_summary.csv", row.names = FALSE)


# ==============================================================================
# PART 3: ENVIRONMENTAL VARIABLE EXTRACTION & GLM ANALYSIS 
# ==============================================================================

# ==============================================================================
#####---------- 3.1 ENVIRONMENTAL VARIABLE EXTRACTION ----------------#####
# ==============================================================================

# Load Required Libraries
library(sf)
library(dplyr)
library(ggplot2)
library(Metrics)
library(tools)

####------- Load Presence Polygons ----------####
presence_sf <- st_read("C:/SHP/Environment_var/sparkPOL_UTM.shp")
presence_sf$Presence <- 1

####------- Load Accessible Area & Generate Background Points ----------####
accessible_area <- st_read("C:/SHP/Environment_var/EcoCUT_utm.shp")
presence_sf <- st_transform(presence_sf, st_crs(accessible_area))

set.seed(42)
n_background <- nrow(presence_sf) * 10
bg_points <- st_sample(accessible_area, size = n_background, type = "random")
background_sf <- st_as_sf(bg_points)
background_sf$Presence <- 0

####------- Define Environmental Layers ----------####
shp_folder <- "C:/SHP/Environment_var"
env_layers <- list(
  D_ANP = "ANP.shp",
  D_URBN = "URBN.shp",
  D_CFE = "CFE.shp",
  D_ROADS = "ROADS.shp",
  B_AFIRE = "FIRE.shp",
  B_FAGRO = "AGRO.shp",
  B_COMMLAND = "COMMLAND.shp",
  B_Pastz = "PASTZ.shp",
  B_Matorr = "MATORR.shp",
  B_OForest = "FOREST.shp"
)

####------- Scale Presence Buffers Based on Area ----------####
presence_sf <- presence_sf %>%
  mutate(
    park_area = st_area(.),
    buffer_dist = sqrt(as.numeric(park_area) / pi) * 2
  ) %>%
  mutate(geometry = st_buffer(geometry, dist = buffer_dist)) %>%
  mutate(buffer_id = row_number(), buffer_area = st_area(geometry)) %>%
  select(buffer_id, buffer_area, Presence, geometry)

####------- Distance Extraction Function ----------####
extract_distance <- function(points, layer_path) {
  layer <- st_read(layer_path, quiet = TRUE)
  layer <- st_transform(layer, st_crs(points))
  distances <- st_distance(points, layer)
  apply(distances, 1, min) / 1000  # convert meters to km
}

####------- Buffer-Based Area Extraction Function ----------####
extract_buffer_areas <- function(target_sf) {
  target_sf$buffer_id <- seq_len(nrow(target_sf))
  target_sf$buffer_area <- st_area(target_sf)
  
  for (var_name in names(env_layers)) {
    if (startsWith(var_name, "B_")) {
      path <- file.path(shp_folder, env_layers[[var_name]])
      message("Processing ", var_name)
      
      land_layer <- st_read(path, quiet = TRUE)
      land_layer <- st_transform(land_layer, st_crs(target_sf))
      
      intersection <- tryCatch(st_intersection(target_sf, land_layer), error = function(e) NULL)
      
      if (is.null(intersection) || nrow(intersection) == 0) {
        message("No intersection for ", var_name, " — assigning 0")
        target_sf[[var_name]] <- 0
        next
      }
      
      intersection$area <- st_area(intersection)
      
      buffer_ids <- st_intersects(intersection, target_sf, sparse = TRUE)
      intersection$buffer_id <- sapply(buffer_ids, function(x) if (length(x) > 0) x[1] else NA)
      
      buffer_sums <- intersection %>%
        filter(!is.na(buffer_id)) %>%
        group_by(buffer_id) %>%
        summarise(var_area = sum(area), .groups = "drop")
      
      target_sf$var_area <- 0
      idx <- match(buffer_sums$buffer_id, target_sf$buffer_id)
      target_sf$var_area[idx] <- buffer_sums$var_area
      
      target_sf[[var_name]] <- as.numeric(target_sf$var_area / target_sf$buffer_area)
    }
  }
  
  target_sf$var_area <- NULL
  return(target_sf)
}

####------- Apply Distance Extraction to Background Points ----------####
background_sf <- st_transform(background_sf, crs = 32614)
for (var_name in names(env_layers)) {
  if (startsWith(var_name, "D_")) {
    path <- file.path(shp_folder, env_layers[[var_name]])
    message("Extracting ", var_name)
    background_sf[[var_name]] <- extract_distance(background_sf, path)
  }
}

####------- Apply Buffer Extraction ----------####
presence_sf <- extract_buffer_areas(presence_sf)
background_sf <- extract_buffer_areas(background_sf)

####------- Drop geometry after extraction ----------####
presence_df <- st_drop_geometry(presence_sf)
background_df <- st_drop_geometry(background_sf)

####------- Combine Presence and Background ----------####
combined_df <- bind_rows(presence_df, background_df)

####------- Fix types ----------####
combined_df$Presence <- as.factor(combined_df$Presence)
combined_df$buffer_area <- as.numeric(combined_df$buffer_area)

####------- Remove list columns ----------####
combined_df <- combined_df[ , !sapply(combined_df, is.list) ]

####------- Replace NAs with 0 ----------####
combined_df[is.na(combined_df)] <- 0

####------- Export Buffers to Shapefiles ----------####
st_write(presence_sf, "C:/SHP/Environment_var/presence_buffers.shp", delete_layer = TRUE)
st_write(background_sf, "C:/SHP/Environment_var/background_buffers.shp", delete_layer = TRUE)

####------- Save combined data for GLM ----------####
write_csv(combined_df, "C:/SHP/R/sparks.csv")

# ==============================================================================
#####---------- 3.2 GLM ANALYSIS - FINAL MODEL ----------------#####
# ==============================================================================

# Load required packages
library(readr)
library(dplyr)
library(corrr)
library(GGally)
library(bestNormalize)
library(ggplot2)
library(tidyr)
library(pROC)
library(broom)
library(MASS)

####------- Load the data ----------####
raw_data <- read_csv("C:/SHP/R/sparks.csv")

####------- Select numeric columns except Park ----------####
raw_numeric <- raw_data %>% select(where(is.numeric), -Park)

####------- Convert to long format for faceting ----------####
raw_long <- raw_numeric %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = factor(Variable, 
                           levels = c("B_AFIRE", "B_FAGRO", "B_COMML",
                                      "B_Pastz", "B_Matrr", "B_OFrst",
                                      "D_ANP", "D_URBN", "D_CFE",
                                      "D_ROADS")))

####------- Create density plot grid (raw data) ----------####
ggplot(raw_long, aes(x = Value)) +
  geom_density(color = "steelblue", linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Variable Distributions (Raw Data)",
    x = "Value",
    y = "Density"
  )

####------- Apply Yeo-Johnson and Z-score ----------####
transformed_scaled <- raw_numeric %>%
  mutate(across(everything(), ~ {
    trans <- yeojohnson(.x)
    scale(predict(trans))
  }))

####------- Correlation AFTER normalization ----------####
cor_after <- cor(transformed_scaled, use = "complete.obs")

####------- Plot correlation matrix ----------####
ggcorr(transformed_scaled, label = TRUE, label_alpha = TRUE, layout.exp = 1.2, name = "Corr", hjust = 0.8) +
  ggtitle("Correlation Matrix (After Yeo-Johnson + Z-score)")

####------- Convert transformed data to long format ----------####
transformed_long <- transformed_scaled %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = factor(Variable, 
                           levels = c("B_AFIRE", "B_FAGRO", "B_COMML",
                                      "B_Pastz", "B_Matrr", "B_OFrst",
                                      "D_ANP", "D_URBN", "D_CFE",
                                      "D_ROADS")))

####------- Create density plot grid for transformed data ----------####
ggplot(transformed_long, aes(x = Value)) +
  geom_density(color = "steelblue", linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Variable Distributions (After Yeo-Johnson + Z-score)",
    x = "Value",
    y = "Density"
  )

####------- Prepare data for GLM ----------####
data <- transformed_scaled
data$Park <- as.factor(raw_data$Park)

####------- Fit binary logistic regression model ----------####
glm_model <- glm(Park ~ ., data = data, family = "binomial")

####------- Summary ----------####
summary(glm_model)

####------- McFadden R² ----------####
ll_model <- logLik(glm_model)
null_model <- glm(Park ~ 1, data = data, family = "binomial")
ll_null <- logLik(null_model)
mcfadden_r2 <- 1 - (as.numeric(ll_model) / as.numeric(ll_null))
cat("McFadden R²:", round(mcfadden_r2, 4), "\n")

####------- RMSE ----------####
pred_probs <- predict(glm_model, type = "response")
actual_numeric <- as.numeric(as.character(data$Park))
rmse <- sqrt(mean((pred_probs - actual_numeric)^2))
cat("RMSE:", round(rmse, 4), "\n")

####------- AUC and ROC ----------####
roc_obj <- roc(data$Park, pred_probs)
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 4), "\n")

####------- Plot ROC curve ----------####
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve - GLM Logistic Model")
abline(a = 0, b = 1, lty = 2, col = "gray")

####------- Extract coefficients and standard errors ----------####
coefs <- summary(glm_model)$coefficients
z_scores <- abs(coefs[, "Estimate"] / coefs[, "Std. Error"])

####------- Convert to data frame ----------####
importance_df <- data.frame(
  Variable = names(z_scores),
  Z_Score = z_scores
)

####------- Remove intercept ----------####
importance_df <- importance_df %>% filter(Variable != "(Intercept)")

####------- Plot variable importance ----------####
ggplot(importance_df, aes(x = reorder(Variable, Z_Score), y = Z_Score)) +
  geom_col(fill = "#86C07C") +
  coord_flip() +
  labs(title = "Variable Importance (|Z|)",
       x = "Variable",
       y = "Importance") +
  theme_minimal()

####------- Forest plot with 95% CI ----------####
coef_df <- tidy(glm_model, conf.int = TRUE)
coef_df <- coef_df %>% filter(term != "(Intercept)")

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "#86C07C", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sprintf("p = %.3f", p.value)), 
            hjust = -0.1, size = 3.2) +
  labs(title = "Coefficients with 95% CI",
       x = "Coefficient (β)", y = "Variable") +
  theme_minimal()

####------- Odds Ratios with 95% CI ----------####
coef_df_odds <- coef_df %>%
  mutate(estimate = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high))

ggplot(coef_df_odds, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(aes(color = p.value < 0.05), size = 3) +
  scale_color_manual(values = c("gray40", "blue")) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Odds Ratios with 95% CI",
       x = "Odds Ratio", y = "Variable") +
  theme_minimal()

# ==============================================================================
#####---------- 3.3 STEPWISE SELECTION ----------------#####
# ==============================================================================

####------- Stepwise selection to minimize AIC ----------####
modelo_optimo <- step(glm_model,
                      scope = list(lower = null_model, upper = glm_model),
                      direction = "both",
                      trace = 1)

####------- Performance metrics for both models ----------####
evaluate_model <- function(model, data) {
  # McFadden R²
  ll_model <- logLik(model)
  ll_null <- logLik(null_model)
  mcfadden <- 1 - (as.numeric(ll_model)/as.numeric(ll_null))
  
  # Predictions
  pred_probs <- predict(model, type = "response")
  actual_numeric <- as.numeric(as.character(data$Park))
  
  # RMSE
  rmse <- sqrt(mean((pred_probs - actual_numeric)^2))
  
  # AUC
  roc_obj <- roc(data$Park, pred_probs)
  auc_val <- auc(roc_obj)
  
  return(list(mcfadden = mcfadden, 
              rmse = rmse, 
              auc = auc_val,
              model = model))
}

####------- Evaluate both models ----------####
initial_metrics <- evaluate_model(glm_model, data)
optimal_metrics <- evaluate_model(modelo_optimo, data)

####------- Print comparison ----------####
cat("\n=== Model Comparison ===\n")
cat(sprintf("Initial Model - McFadden R²: %.4f | RMSE: %.4f | AUC: %.4f\n", 
            initial_metrics$mcfadden, initial_metrics$rmse, initial_metrics$auc))
cat(sprintf("Optimal Model - McFadden R²: %.4f | RMSE: %.4f | AUC: %.4f\n", 
            optimal_metrics$mcfadden, optimal_metrics$rmse, optimal_metrics$auc))

####------- Plot ROC comparison ----------####
plot(roc(data$Park, predict(glm_model, type = "response")), 
     col = "blue", lwd = 2, main = "ROC Comparison")
plot(roc(data$Park, predict(modelo_optimo, type = "response")), 
     col = "red", lwd = 2, add = TRUE, lty = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = c("Initial", "Optimal"), 
       col = c("blue", "red"), lwd = 2, lty = c(1,2))

####------- Optimal model coefficients ----------####
cat("\n=== Optimal Model Coefficients ===\n")
coef_list <- summary(modelo_optimo)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  filter(Variable != "(Intercept)") %>%
  mutate(p_value = `Pr(>|z|)`) %>%
  select(Variable, Estimate, `Std. Error`, `z value`, p_value)

print(coef_list, digits = 4)

####------- Variable importance plot for optimal model ----------####
importance_df_opt <- data.frame(
  Variable = names(abs(coef(modelo_optimo))[-1]),
  Importance = abs(coef(modelo_optimo))[-1]
) %>% arrange(desc(Importance))

ggplot(importance_df_opt, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#86C07C") +
  coord_flip() +
  labs(title = "Optimal Model Variable Importance",
       x = "Variable",
       y = "|Coefficient|") +
  theme_minimal()

# ==============================================================================
#####---------- 3.4 MODEL VARIANT - WITHOUT FOREST ----------------#####
# ==============================================================================

####------- Model without B_OFrst ----------####
glm_modelF <- glm(Park ~ B_AFIRE + B_FAGRO + B_COMML + B_Pastz + B_Matrr + 
                    D_ANP + D_URBN + D_CFE + D_ROADS, 
                  data = data, family = "binomial")

####------- Summary ----------####
summary(glm_modelF)

####------- Get tidied coefficients ----------####
coef_dfF <- tidy(glm_modelF, conf.int = TRUE) %>% 
  filter(term != "(Intercept)")

####------- Plot coefficients with 95% CI ----------####
ggplot(coef_dfF, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3, color = "#86C07C") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Model Without Forest Variable",
       x = "Estimate",
       y = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==============================================================================
#####---------- 3.5 MODEL VARIANT - WITHOUT GRASSLAND ----------------#####
# ==============================================================================

####------- Model without B_Pastz ----------####
glm_modelG <- glm(Park ~ B_AFIRE + B_FAGRO + B_COMML + B_Matrr + 
                    D_ANP + D_URBN + D_CFE + D_ROADS, 
                  data = data, family = "binomial")

####------- Summary ----------####
summary(glm_modelG)

####------- Get tidied coefficients ----------####
coef_dfG <- tidy(glm_modelG, conf.int = TRUE) %>% 
  filter(term != "(Intercept)")

####------- Plot coefficients with 95% CI ----------####
ggplot(coef_dfG, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3, color = "#86C07C") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Model Without Grassland Variable",
       x = "Estimate",
       y = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==============================================================================
#####---------- 3.6 POTENTIALITY MAP PREDICTIONS ----------------#####
# ==============================================================================

####------- Get predicted probabilities from optimal model ----------####
pred_probs_opt <- predict(modelo_optimo, type = "response")

####------- Add predictions to data ----------####
data$predicted_prob <- pred_probs_opt

####------- Save predictions to CSV ----------####
write_csv(data %>% select(Park, predicted_prob), "C:/SHP/R/predictions.csv")

####------- Load new data for prediction map ----------####
library(readr)
library(dplyr)

####------- Read CSV with proper encoding ----------####
data_map <- read_csv("C:/SHP/MapaPotencial/Allpoints.csv", locale = locale(encoding = "UTF-8"))

# If UTF-8 fails, try Latin1:
# data_map <- read_csv("C:/SHP/MapaPotencial/Allpoints.csv", locale = locale(encoding = "Latin1"))

####------- Remove accents from numbers ----------####
data_clean <- data_map %>%
  mutate(across(where(is.numeric), ~ as.numeric(gsub("´", "", .x))))

####------- Ensure column names match GLM predictors ----------####
pred_probs_map <- predict(modelo_optimo, newdata = data_clean, type = "response")

####------- Add predictions to CSV ----------####
data_clean$predicted_prob <- pred_probs_map

####------- Save updated CSV ----------####
write_csv(data_clean, "C:/SHP/MapaPotencial/predicted_optimal.csv")

####------- Analysis Complete ----------####
cat("\n=== Analysis Complete ===\n")
cat("All outputs have been saved successfully.\n")