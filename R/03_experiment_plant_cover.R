# =============================================================
# Experimental Treatments: Plant Cover
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 24, 2025
# Purpose: Clean, document, and fully reproduce the analyses and
#          visualizations of plant cover across pika and 
#          poisonous plant treatment combinations.
# =============================================================

# --- Load Required Packages ---
library(tidyverse)       # Data wrangling and plotting
library(glmmTMB)         # Generalized linear mixed models
library(emmeans)         # Estimated marginal means
library(multcomp)        # Multiple comparisons
library(broom.mixed)     # Tidy model outputs from mixed models
library(DHARMa)          # GLMM diagnostics
library(ggpubr)          # Publication-ready ggplots
library(here)            # Relative file paths
library(glue)            # String interpolation

# --- Set Root Directory for Project ---
here::i_am("README.md")  # Anchor project for reproducible paths

# --- Custom Color Palette ---
pink <- '#ff8080'
green <- '#2fc09b'

# =============================================================
# Load and Prepare Data
# =============================================================

# --- Load all processed CSVs ---
csv_dir <- here("data/processed/experiment_plant_cover")
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

walk(csv_files, function(file) {
  obj_name <- tools::file_path_sans_ext(basename(file))
  assign(obj_name, read_csv(file, show_col_types = FALSE), envir = .GlobalEnv)
})

# --- Prepare plant cover dataset ---
plant_cover_by_treatment <- plant_cover_by_treatment |>
  group_by(plant) |>
  mutate(
    n_obs = n(),
    cover_beta = ((cover * (n_obs - 1) + 0.5) / n_obs) / 100
  ) |>
  dplyr::select(-n_obs) |>
  mutate(
    year = factor(year),
    block = factor(block)
  )

# --- Split data by plant type ---
plant_cover_by_treatment_split <- plant_cover_by_treatment |>
  group_by(plant) |>
  group_split() |>
  set_names(plant_cover_by_treatment |> group_by(plant) |> group_keys() |> pull(plant))



# =============================================================
# Fit GLMMs: Gaussian and Beta Families
# =============================================================

# --- Fit both Gaussian and Beta models ---
fit_models <- function(df) {
  list(
    beta = glmmTMB(
      cover_beta ~ pika_treatment * posion_plant_treatment +
        (1 | block) + (1 | year) + (1 | month),
      family = beta_family(),
      data = df
    ),
    gauss = glmmTMB(
      cover_beta ~ pika_treatment * posion_plant_treatment +
        (1 | block) + (1 | year) + (1 | month),
      family = gaussian(),
      data = df
    )
  )
}

plant_models <- map(plant_cover_by_treatment_split, fit_models)

# =============================================================
# Model Diagnostics
# =============================================================

# --- Save residual diagnostics for a single model ---
save_diagnostics <- function(plant_name, model, model_type) {
  png(
    filename = here("output/experiment_plant_cover/diagnostic_plots", 
                    glue("{plant_name}_{model_type}_diagnostics.png")),
    width = 1200, height = 600, res = 150
  )
  
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # 1 row, 2 plots

  resid <- simulateResiduals(model, n = 1000)
  plotQQunif(resid)
  plotResiduals(resid)

  mtext(glue("DHARMa Diagnostics – {plant_name} ({model_type})"), outer = TRUE, cex = 1.2)

  dev.off()
}

# --- Generate diagnostics for each plant and model type ---
iwalk(plant_models, function(models, plant_name) {
  save_diagnostics(plant_name, models$gauss, "gauss")
  save_diagnostics(plant_name, models$beta, "beta")
})

# =============================================================
# Choose Best Model (Beta)
# =============================================================

plant_models_beta <- map(plant_models, "beta")

# =============================================================
# Generate and Save Plots
# =============================================================

# --- Create list of ggplots by plant ---
plant_cover_plots <- map(names(plant_models_beta), function(name) {
  data <- plant_cover_by_treatment_split[[name]]
  model <- plant_models_beta[[name]]
  
  emms <- emmeans(model, ~ pika_treatment * posion_plant_treatment, type = "response")
  emms_df <- as_tibble(emms)
  contrasts <- as_tibble(pairs(emms, adjust = "tukey"))
  letters <- cld(emms, adjust = "tukey", Letters = letters) |>
    as_tibble() |>
    mutate(.group = str_trim(.group))
  
  plot_data <- data |>
    left_join(letters, by = c("pika_treatment", "posion_plant_treatment"))
  
  ggplot(plot_data, aes(x = pika_treatment, y = cover_beta, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = letters,
      aes(x = pika_treatment, y = response, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 8, pch = 21, color = "black"
    ) +
    geom_text(
      data = letters,
      aes(x = pika_treatment, y = 0.6, label = .group, group = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 5, color = "black", show.legend = FALSE
    ) +
    scale_fill_manual(values = c(green, pink)) +
    scale_color_manual(values = c(green, pink)) +
    labs(
      title = name,
      y = "Cover (beta-scale)",
      x = "",
      color = "Poison plant treatment",
      fill = "Poison plant treatment"
    ) +
    theme_pubr(base_size = 15) +
    theme(legend.position = "bottom", legend.title = element_blank())
}) |> set_names(names(plant_models_beta))

# --- Save plots to disk ---
iwalk(plant_cover_plots, ~ ggsave(
  here("output/experiment_plant_cover/plots", glue("plant_cover_{.y}.png")),
  plot = .x, width = 6, height = 5
))

# =============================================================
# Export Model Outputs
# =============================================================

# --- Named list of beta-family models for export ---
model_list <- list(
  s_chamaejasme_cover = plant_models_beta$s_chamaejasme,
  sedges_cover = plant_models_beta$sedges,
  grasses_cover = plant_models_beta$grasses,
  forbs_cover = plant_models_beta$forbs
)

# --- Function to export summary, EMMs, contrasts, and groups ---
save_model_outputs <- function(model, name) {
  base_dir <- here("data/processed/experiment_plant_cover/modeled_data", name)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Model summary
  model_summary <- tidy(model)
  write_csv(model_summary, file.path(base_dir, "model_summary.csv"))
  
  # Estimated Marginal Means
  emms <- emmeans(model, ~ pika_treatment * posion_plant_treatment, type = "response")
  emms_df <- as_tibble(emms)
  write_csv(emms_df, file.path(base_dir, "emms.csv"))
  
  # Post hoc pairwise contrasts
  contrasts <- as_tibble(pairs(emms))
  write_csv(contrasts, file.path(base_dir, "posthoc_contrasts.csv"))
  
  # Compact letter display (groupings)
  letters <- cld(emms, Letters = letters, type = "response") |>
    as_tibble() |>
    mutate(.group = str_trim(.group))
  write_csv(letters, file.path(base_dir, "posthoc_letters.csv"))
}

# --- Export results for each model ---
imap(model_list, save_model_outputs)
