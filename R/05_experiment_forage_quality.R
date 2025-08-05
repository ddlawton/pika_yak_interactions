# =============================================================
# Experiment: Foraging Quality Data Management, Modeling, and Visualization Script
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: August 04, 2024
# Purpose:
#   - To reproduce the full analytical and graphical workflow
#     for the study examining pika-plant
#     interactions on the Tibetan Plateau.
#   - Includes raw data visualization, GLMM fitting with model
#     selection, estimated marginal means analysis, and plot export.
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
options(width=150)
# --- Custom Color Palette ---
pink <- '#ff8080'
green <- '#2fc09b'


# =============================================================
# Load and Prepare Data
# =============================================================

# --- Load all processed CSVs ---
quality_dat <- read_csv(here('data/processed/experiment_foraging_quality/plant_foraging_quality.csv')) |>
  rename(quality_metric = plant) |>
  mutate(
    across(c(block,year,month),~as.factor(.)),
    forage_quality = forage_quality / 100
  ) 

summary(quality_dat$forage_quality) # ensuring that the percentage does not contain 0s or 1s


# =============================================================
# Fit GLMMs
# =============================================================

# we should use the following model fomrula:
# forage_quality ~ pike_treatment * posion_plant_treatment + (1 | block) + (1 | year) + (1 | month)
# for each metric (cp,adf,ndf,ee)

# --- Split data by plant type ---
quality_dat_split <- quality_dat |>
  ungroup() |>
  group_split(quality_metric)

names(quality_dat_split) <- quality_dat |>
  distinct(quality_metric) |>
  pull(quality_metric)

# --- Fit both Gaussian and Beta models ---
fit_models <- function(df) {
  list(
    beta = glmmTMB(
      forage_quality ~ pika_treatment * posion_plant_treatment +
        (1 | block) + (1 | year) + (1 | month),
      family = beta_family(),
      data = df
    ),
    gauss = glmmTMB(
      forage_quality ~ pika_treatment * posion_plant_treatment +
        (1 | block) + (1 | year) + (1 | month),
      family = gaussian(),
      data = df
    )
  )
}

forage_quality_models <- map(quality_dat_split, fit_models)


# =============================================================
# Model Diagnostics
# =============================================================

# --- Save residual diagnostics for a single model ---
save_diagnostics <- function(quality_metric, model, model_type) {
  png(
    filename = here("output/experiment_foraging_quality/diagnostic_plots", 
                    glue("{quality_metric}_{model_type}_diagnostics.png")),
    width = 1200, height = 600, res = 150
  )
  
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # 1 row, 2 plots

  resid <- simulateResiduals(model, n = 1000)
  plotQQunif(resid)
  plotResiduals(resid)

  mtext(glue("DHARMa Diagnostics – {quality_metric} ({model_type})"), outer = TRUE, cex = 1.2)

  dev.off()
}

# --- Generate diagnostics for each plant and model type ---
iwalk(forage_quality_models, function(models, quality_metric) {
  save_diagnostics(quality_metric, models$gauss, "gauss")
  save_diagnostics(quality_metric, models$beta, "beta")
})


# modeling diagnostic summaries:
# all modesl with the exception fo CP fit well.
# even then, CP % is not that bad.
# no big difference between Gaussian and Beta
# but since the nature of the data is percentage, I will stick wth 
# beta

# =============================================================
# Choose Best Model (Beta)
# =============================================================

forage_quality_models_beta <- map(forage_quality_models, "beta")




# =============================================================
# Generate and Save Plots
# =============================================================

# --- Create list of ggplots by plant ---
forage_quality_plots <- map(names(forage_quality_models_beta), function(name) {
  data <- quality_dat_split[[name]]
  model <- forage_quality_models_beta[[name]]
  
  emms <- emmeans(model, ~ pika_treatment * posion_plant_treatment, type = "response")
  emms_df <- as_tibble(emms)

  contrasts <- as_tibble(pairs(emms, adjust = "tukey"))
  letters_df <- cld(emms, adjust = "tukey", Letters = letters) |>
    as_tibble() |>
    mutate(.group = str_trim(.group))
  
  plot_data <- data |>
    left_join(letters_df, by = c("pika_treatment", "posion_plant_treatment"))

  ggplot(plot_data, aes(x = pika_treatment, y = forage_quality, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = letters_df,
      aes(x = pika_treatment, y = response, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 4, pch = 21, color = "black"
    ) +
      geom_errorbar(
        data = letters_df,
        aes(
          x = pika_treatment,
          ymin = response - SE,
          ymax = response + SE,
          group = posion_plant_treatment
        ),
        position = position_dodge(width = 0.7),
        inherit.aes = FALSE,
        width = 0
      ) +      
    geom_text(
      data = letters_df,
      aes(x = pika_treatment, y = (response * 1.25), label = .group, group = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 5, color = "black", show.legend = FALSE
    ) +
    scale_fill_manual(values = c(green, pink)) +
    scale_color_manual(values = c(green, pink)) +
    labs(
      title = name,
      y = "Forage Quality (beta-scale)",
      x = "",
      color = "Poison plant treatment",
      fill = "Poison plant treatment"
    ) +
    theme_pubr(base_size = 15) +
    theme(legend.position = "bottom", legend.title = element_blank())
}) |> set_names(names(forage_quality_models_beta))

# --- Save plots to disk ---
iwalk(forage_quality_plots, ~ ggsave(
  here("output/experiment_foraging_quality/plots", glue("plant_cover_{.y}.png")),
  plot = .x, width = 6, height = 5
))




# =============================================================
# Export Model Outputs
# =============================================================

# --- Named list of beta-family models for export ---
model_list <- list(
  cp = forage_quality_models_beta$cp,
  adf = forage_quality_models_beta$adf,
  ndf = forage_quality_models_beta$ndf,
  ee = forage_quality_models_beta$ee
)

# --- Function to export summary, EMMs, contrasts, and groups ---
save_model_outputs <- function(model, name) {
  base_dir <- here("data/processed/experiment_foraging_quality/modeled_data", name)
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
