# =============================================================
# Experiment: Foraging Efficiency: Data Management, Modeling, and Visualization Script
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: August 01, 2024
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

# --- Custom Color Palette ---
pink <- '#ff8080'
green <- '#2fc09b'

# =============================================================
# Load and Prepare Data
# =============================================================

# --- Load all processed CSVs ---
csv_dir <- here("data/processed/experiment_foraging_efficiency")
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

walk(csv_files, function(file) {
  obj_name <- tools::file_path_sans_ext(basename(file))
  assign(obj_name, read_csv(file, show_col_types = FALSE), envir = .GlobalEnv)
})
 
# --- further data refinement ---
yak_plant_bites <- yak_plant_bites |>
  mutate(plant = gsub("_[^_]*$", '',plant),
    across(c(block,year),~as.factor(.)),
    treatment_code = paste0(
      ifelse(pika_treatment == "pika", "P", "A"),
      ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
    ),
    # Create final yak ID, like B1_PP_Y1
    yak = paste0("B", block, "_", treatment_code, "_", yak)) |>
  dplyr::select(-treatment_code)

yak_total_steps <- yak_total_steps |>
  mutate(yak = gsub("_[^_]*$", '',yak),
    across(c(block,year),~as.factor(.)),
    treatment_code = paste0(
      ifelse(pika_treatment == "pika", "P", "A"),
      ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
    ),
    # Create final yak ID, like B1_PP_Y1
    yak = paste0("B", block, "_", treatment_code, "_", yak)) |>
  dplyr::select(-treatment_code)


yak_bite_steps_ratio <- yak_bite_steps_ratio |> 
  mutate(across(c(block,year),~as.factor(.))) |>
  group_by(plant) |>
  mutate(
    n_obs = n(),
    bites_steps_ratio_beta = ((bites_steps_ratio * (n_obs - 1) + 0.5) / n_obs) / 100
  ) |>
  dplyr::select(-n_obs) |>
  mutate(
    year = factor(year),
    block = factor(block),
    treatment_code = paste0(
      ifelse(pika_treatment == "pika", "P", "A"),
      ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
    ),
    # Create final yak ID, like B1_PP_Y1
    yak = paste0("B", block, "_", treatment_code, "_", yak_num)) |>
  dplyr::select(-c(treatment_code,yak_num))


yak_weight_gain <- yak_weight_gain |>
  mutate(yak = gsub("_[^_]*$", '',yak),
    across(c(block,year),~as.factor(.)),
    treatment_code = paste0(
      ifelse(pika_treatment == "pika", "P", "A"),
      ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
    ),
    # Create final yak ID, like B1_PP_Y1
    yak = paste0("B", block, "_", treatment_code, "_", yak)) |>
  dplyr::select(-treatment_code)


# =============================================================
# Fit GLMMs
# =============================================================


# --- Split datasets ---
split_by_plant <- function(df, group_var = "plant") {
  df |>
    ungroup() |>
    group_split(.data[[group_var]]) |>
    set_names(df |> group_by(.data[[group_var]]) |> group_keys() |> pull(.data[[group_var]]))
}

yak_plant_bites_split     <- split_by_plant(yak_plant_bites)
yak_bite_steps_ratio_split <- split_by_plant(yak_bite_steps_ratio)

# --- Model fitting functions ---
plant_bites_fit_models <- function(df) {
  warnings_list <- list()
  
  # Try Gaussian model first
  model <- tryCatch({
    withCallingHandlers(
      glmmTMB(
        forage_efficiency ~ pika_treatment * posion_plant_treatment +
          (1 | block) + (1 | year) + (1 | month),
        family = gaussian(),
        data = df
      ),
      warning = function(w) {
        warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) {
    warnings_list[[length(warnings_list) + 1]] <<- paste("Gaussian model failed:", conditionMessage(e))
    NULL
  })

  # If Gaussian failed to converge or produced a known convergence warning, try Tweedie
  if (
    is.null(model) ||
    any(grepl("Model convergence problem", warnings_list)) ||
    any(grepl("non-positive-definite Hessian", warnings_list))
  ) {
    warnings_list[[length(warnings_list) + 1]] <- "Trying Tweedie model due to Gaussian convergence issues"
    
    model <- tryCatch({
      withCallingHandlers(
        glmmTMB(
          forage_efficiency ~ pika_treatment * posion_plant_treatment +
            (1 | block) + (1 | year) + (1 | month),
          family = tweedie(link = "log"),
          data = df
        ),
        warning = function(w) {
          warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) {
      warnings_list[[length(warnings_list) + 1]] <<- paste("Tweedie model failed:", conditionMessage(e))
      NULL
    })
  }

  # Return the model and any captured warnings
  list(
    model = model,
    warnings = warnings_list
  )
}


bite_steps_ratio_models <- function(df) {
  warnings_list <- list(beta = list(), gauss = list())

  # Fit beta model and capture warnings
  beta_model <- withCallingHandlers(
    expr = glmmTMB(
      bites_steps_ratio_beta ~ pika_treatment * posion_plant_treatment +
        (1 | block) + (1 | year) + (1 | month),
      family = beta_family(),
      data = df
    ),
    warning = function(w) {
      warnings_list$beta[[length(warnings_list$beta) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  # Fit gauss model and capture warnings
  gauss_model <- withCallingHandlers(
    expr = glmmTMB(
      bites_steps_ratio ~ pika_treatment * posion_plant_treatment +
        (1 | block) + (1 | year) + (1 | month),
      family = gaussian(),
      data = df
    ),
    warning = function(w) {
      warnings_list$gauss[[length(warnings_list$gauss) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  list(
    beta = beta_model,
    gauss = gauss_model,
    warnings = warnings_list
  )
}




# --- Fit all models ---
plant_bites_models <- map(yak_plant_bites_split, plant_bites_fit_models)

bite_steps_models <- map(yak_bite_steps_ratio_split, bite_steps_ratio_models)


total_steps_model <-   glmmTMB(
  total_steps ~ pika_treatment * posion_plant_treatment +
    (1 | block) + (1 | year) + (1 | month),
  family = gaussian(),
  data = yak_total_steps
)

weight_gain_model <-   glmmTMB(
  weight_gain ~ pika_treatment * posion_plant_treatment +
    (1 | block) + (1 | year) + (1 | month),
  family = gaussian(),
  data = yak_weight_gain
)

# =============================================================
# Model Diagnostics
# =============================================================

# --- Save residual diagnostics for a single model ---
save_diagnostics <- function(plant_name, model, model_type,directory) {
  png(
    filename = here(directory,
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
iwalk(bite_steps_models, function(models, plant_name) {
  if (!is.null(models$gauss)) {
    try(
      save_diagnostics(
        plant_name,
        models$gauss,
        "gauss",
        directory = "output/experiment_foraging_efficiency/diagnostic_plots/bite_steps_ratio"
      ),
      silent = TRUE
    )
  } else {
    message(glue("[{plant_name}] Skipped gauss model (fit failed)"))
  }

  if (!is.null(models$beta)) {
    try(
      save_diagnostics(
        plant_name,
        models$beta,
        "beta",
        directory = "output/experiment_foraging_efficiency/diagnostic_plots/bite_steps_ratio"
      ),
      silent = TRUE
    )
  } else {
    message(glue("[{plant_name}] Skipped beta model (fit failed)"))
  }
})


iwalk(plant_bites_models, function(model, plant_name) {
  if (!is.null(model)) {
    try(
      save_diagnostics(
        plant_name,
        model,
        model_type = "gaussian",
        directory = "output/experiment_foraging_efficiency/diagnostic_plots/plant_bites"
      ),
      silent = TRUE
    )
  } else {
    message(glue("[{plant_name}] Skipped gauss model (fit failed)"))
  }
})


png(filename = here("output/experiment_foraging_efficiency/diagnostic_plots/total_steps/total_steps_fit_models_diagnostics.png"),
  width = 1200, height = 600, res = 150)

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # 1 row, 2 plots

resid <- simulateResiduals(total_steps_model, n = 1000)
plotQQunif(resid)
plotResiduals(resid)

mtext(glue("DHARMa Diagnostics – total steps"), outer = TRUE, cex = 1.2)

dev.off()



png(filename = here("output/experiment_foraging_efficiency/diagnostic_plots/weight_gain/weight_gain_models_diagnostics.png"),
  width = 1200, height = 600, res = 150)

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # 1 row, 2 plots

resid <- simulateResiduals(weight_gain_model, n = 1000)
plotQQunif(resid)
plotResiduals(resid)

mtext(glue("DHARMa Diagnostics – weight_gain"), outer = TRUE, cex = 1.2)

dev.off()

# =============================================================
# Export Model Outputs
# =============================================================
bite_steps_models_beta <- map(bite_steps_models, "beta")

# --- Named list of beta-family models for export ---
bite_steps_model_list <- list(
  sedges_bite_steps = bite_steps_models_beta$sedges,
  grasses_bite_steps = bite_steps_models_beta$grasses,
  forbs_bite_steps = bite_steps_models_beta$forbs
)
names(bite_steps_model_list) <- as.character(names(bite_steps_model_list))

plant_bites_model_list <- list(
  sedges_plant_bites = plant_bites_models$sedges,
  grasses_plant_bites = plant_bites_models$grasses,
  forbs_plant_bites = plant_bites_models$forbs,
  total_plant_bites = plant_bites_models$total
)
names(plant_bites_models)
#single models
weight_gain_model
total_steps_model


# --- Function to export summary, EMMs, contrasts, and groups ---

save_model_outputs <- function(model, model_name, base_dir_path) {
  # Construct output directory and ensure it exists
  model_dir <- file.path(base_dir_path, model_name)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Model Summary (fixed + random effects) ---
  model_summary <- broom.mixed::tidy(model)
  write_csv(model_summary, file.path(model_dir, "model_summary.csv"))

  # --- Estimated Marginal Means ---
  emms <- emmeans(model, ~ pika_treatment * posion_plant_treatment, type = "response")
  emms_df <- as_tibble(emms)
  write_csv(emms_df, file.path(model_dir, "emms.csv"))

  # --- Post hoc pairwise contrasts ---
  contrast_df <- as_tibble(pairs(emms))
  write_csv(contrast_df, file.path(model_dir, "posthoc_contrasts.csv"))

  # --- Compact Letter Display (group letters) ---
  cld_df <- cld(emms, Letters = letters, type = "response") |>
    as_tibble() |>
    mutate(.group = str_trim(.group))
  write_csv(cld_df, file.path(model_dir, "posthoc_letters.csv"))
}


# --- Export results for each model ---


# Example directory: adjust to your output path
bite_steps_output_dir <- here("data/processed/experiment_foraging_efficiency/modeled_data/bite_steps")
plant_bites_output_dir <- here("data/processed/experiment_foraging_efficiency/modeled_data/plant_bites")

# Loop through named list of glmmTMB models
iwalk(bite_steps_model_list, ~ save_model_outputs(.x, .y, bite_steps_output_dir))
iwalk(plant_bites_model_list, ~ save_model_outputs(.x$model, .y, plant_bites_output_dir))


# weight gain

# Model summary
weight_gain_model_summary <- tidy(weight_gain_model)
write_csv(weight_gain_model_summary, file.path("data/processed/experiment_foraging_efficiency/modeled_data/weight_gain", "model_summary.csv"))

# Estimated Marginal Means
emms <- emmeans(weight_gain_model, ~ pika_treatment * posion_plant_treatment, type = "response")
emms_df <- as_tibble(emms)
write_csv(emms_df, file.path("data/processed/experiment_foraging_efficiency/modeled_data/weight_gain", "emms.csv"))

# Post hoc pairwise contrasts
contrasts <- as_tibble(pairs(emms))
write_csv(contrasts, file.path("data/processed/experiment_foraging_efficiency/modeled_data/weight_gain", "posthoc_contrasts.csv"))

# Compact letter display (groupings)
letters <- cld(emms, Letters = letters, type = "response") |>
  as_tibble() |>
  mutate(.group = str_trim(.group))
write_csv(letters, file.path(here("data/processed/experiment_foraging_efficiency/modeled_data/weight_gain"), "posthoc_letters.csv"))


# total steps

# Model summary
total_steps_model_summary <- tidy(total_steps_model)
write_csv(total_steps_model_summary, file.path("data/processed/experiment_foraging_efficiency/modeled_data/total_steps", "model_summary.csv"))

# Estimated Marginal Means
emms <- emmeans(total_steps_model, ~ pika_treatment * posion_plant_treatment, type = "response")
emms_df <- as_tibble(emms)
write_csv(emms_df, file.path("data/processed/experiment_foraging_efficiency/modeled_data/total_steps", "emms.csv"))

# Post hoc pairwise contrasts
contrasts <- as_tibble(pairs(emms))
write_csv(contrasts, file.path("data/processed/experiment_foraging_efficiency/modeled_data/total_steps", "posthoc_contrasts.csv"))

# Compact letter display (groupings)
letters <- multcomp::cld(emms, Letters = 'AB', type = "response") |>
  as_tibble() |>
  mutate(.group = str_trim(.group))
write_csv(letters, file.path(here("data/processed/experiment_foraging_efficiency/modeled_data/total_steps"), "posthoc_letters.csv"))

