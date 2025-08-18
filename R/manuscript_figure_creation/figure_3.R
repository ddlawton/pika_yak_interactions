library(tidyverse)
library(ggpubr)
library(patchwork)
library(here)
library(mgcv)
library(knitr)
library(kableExtra)

# --- Set Root Directory for Project ---
here::i_am("README.md")  # Anchor project for reproducible paths



# --- Custom Color Palette ---
pink <- '#ff8080'
green <- '#2fc09b'
plant_colors <- c(green, pink)

# this figure has the following:
# panel a: weight gain
# panel b: cp %
# panel c: adf
# panel d: ee
# panel e: grass cover
# panel f: poison plant cover
# it should be two rows with three columns


# --- Weight Gain ---


weight_gain <- read_csv(here("data/processed/experiment_foraging_efficiency/yak_weight_gain.csv")) |>
  mutate(yak = gsub("_[^_]*$", '',yak))

# Define a function to load model data for a plant group
load_data <- function(base_path) {
  list(
    emms = read_csv(file.path(base_path, "emms.csv")),
    letters = read_csv(file.path(base_path, "posthoc_letters.csv")),
    model_summary = read_csv(file.path(base_path, "model_summary.csv")),
    model_contrasts = read_csv(file.path(base_path, "posthoc_contrasts.csv"))
  )
}


weight_gain_models <- load_data(here('data/processed/experiment_foraging_efficiency/modeled_data/weight_gain'))

panel_a_weight_gain <-  weight_gain |>
    left_join(weight_gain_models$emms, by = c('pika_treatment', 'posion_plant_treatment')) |>
    ggplot(aes(x = pika_treatment, y = weight_gain, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = weight_gain_models$letters,
      aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 8, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    geom_text(
      data = weight_gain_models$letters,
      aes(x = pika_treatment, y = 1, label = .group, group = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 5, color = "black", show.legend = FALSE, inherit.aes = FALSE
    ) +
    scale_fill_manual(values = plant_colors) +
    scale_color_manual(values = plant_colors) +
    labs(
      y = 'weight gain (kg/yak/day)',
      x = "",
      color = "Poison plant treatment",
      fill = "Poison plant treatment"
    ) +
    theme_pubr(base_size = 15) +
    theme(legend.position = "bottom", legend.title = element_blank())


# --- Forage Quality ---
# CP, ADF, and EE

forage_quality <- read_csv(here("data/processed/experiment_foraging_quality/plant_foraging_quality.csv"))

load_forage_quality_data <- function(quality_metric) {
  base_path <- here(glue::glue("data/processed/experiment_foraging_quality/modeled_data/{quality_metric}"))
  list(
    emms = read_csv(file.path(base_path, "emms.csv")),
    letters = read_csv(file.path(base_path, "posthoc_letters.csv")),
    model_summary = read_csv(file.path(base_path, "model_summary.csv")),
    model_contrasts = read_csv(file.path(base_path, "posthoc_contrasts.csv"))
  )
}

# Define your plant groups
quality_metrics <- c("adf", "cp", "ee")

# Load all model data into a named list
forage_quality_models <- map(quality_metrics, load_forage_quality_data) |> set_names(quality_metrics)

# Reusable function
plot_forage_quality <- function(quality_metric, y_label, label_y = NULL, letters = TRUE) {
  data <- forage_quality |> filter(plant == quality_metric)
  emms <- forag_quality_models[[quality_metric]]$emms
  letters_df <- forag_quality_models[[quality_metric]]$letters

  label_y <- label_y %||% max(data$forage_quality, na.rm = TRUE)

  p <- data |>
    left_join(emms, by = c('pika_treatment', 'posion_plant_treatment')) |>
    ggplot(aes(x = pika_treatment, y = forage_quality, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = letters_df,
      aes(x = pika_treatment, y = response * 100, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 4, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    scale_fill_manual(values = plant_colors) +
    scale_color_manual(values = plant_colors) +
    labs(
      y = y_label,
      x = "",
      color = "Poison plant treatment",
      fill = "Poison plant treatment"
    ) +
    theme_pubr(base_size = 15) +
    theme(legend.position = "bottom", legend.title = element_blank())

    if (letters) {
        p <- p  +
        geom_text(
            data = letters_df,
            aes(x = pika_treatment, y = label_y, label = .group, group = posion_plant_treatment),
            position = position_dodge(width = 0.7),
            size = 5, color = "black", show.legend = FALSE
        )
  }

  p
}

# Call the function for each plant type
panel_c_adf <- plot_forage_quality("adf", "adf (%)",label_y=36)
panel_b_cp  <- plot_forage_quality("cp", "cp (%)",label_y = 12)
panel_d_ee   <- plot_forage_quality("ee", "ee (%)",label_y=5)


# --- Plant Cover ---


grasses_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/emms.csv'))
grasses_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/posthoc_letters.csv'))
grasses_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/model_summary.csv'))
grasses_model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/posthoc_contrasts.csv'))

poison_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/emms.csv'))
poison_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/posthoc_letters.csv'))
poison_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/model_summary.csv'))
poison__model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/posthoc_contrasts.csv'))
