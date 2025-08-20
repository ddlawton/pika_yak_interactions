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
      size = 4, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    geom_text(
      data = weight_gain_models$letters,
      aes(x = pika_treatment, y = 0.60, label = .group, group = posion_plant_treatment),
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
    theme_pubr(base_size = 10) +
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
  emms <- forage_quality_models[[quality_metric]]$emms
  letters_df <- forage_quality_models[[quality_metric]]$letters

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
    theme_pubr(base_size = 10) +
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
panel_b_cp  <- plot_forage_quality("cp", "cp (%)",label_y = 10)
panel_c_adf <- plot_forage_quality("adf", "adf (%)",label_y=36)
panel_d_ee   <- plot_forage_quality("ee", "ee (%)",label_y=4)


# --- Plant Cover ---
cover_data <- read_csv(here('data/processed/experiment_plant_cover/plant_cover_by_treatment.csv'))

grasses_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/emms.csv'))
grasses_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/posthoc_letters.csv'))
grasses_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/model_summary.csv'))
grasses_model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/grasses_cover/posthoc_contrasts.csv'))

poison_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/emms.csv'))
poison_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/posthoc_letters.csv'))
poison_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/model_summary.csv'))
poison__model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/s_chamaejasme_cover/posthoc_contrasts.csv'))



make_cover_plot <- function(plant_name, letters_df, y_label, text_y = 41, letters = TRUE) {
  p <- cover_data |>
    filter(plant == plant_name) |>
    left_join(letters_df, by = c("pika_treatment", "posion_plant_treatment")) |>
    ggplot(aes(x = pika_treatment, y = cover, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
        data = letters_df,
        aes(x = pika_treatment, y = response * 100, fill = posion_plant_treatment),
        position = position_dodge(width = 0.7),
        size = 4, pch = 21, color = "black"
    ) +
    scale_fill_manual(values = c(green, pink)) +
    scale_color_manual(values = c(green, pink)) +
    labs(
      y = y_label,
      x = "",
      color = "Poison plant treatment",
      fill = "Poison plant treatment"
    ) +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank())

  if (letters) {
    p <- p  +
      geom_text(
        data = letters_df,
        aes(x = pika_treatment, y = text_y, label = .group, group = posion_plant_treatment),
        position = position_dodge(width = 0.7),
        size = 5, color = "black", show.legend = FALSE
      )
  }

  p
}


panel_e_grass_plot   <- make_cover_plot("grasses", grasses_letters, "grass cover (%)", text_y = 41)
panel_f_poison_plant <- make_cover_plot("s_chamaejasme", poison_letters, "S. chamaejasme cover (%)", text_y = 43)



#  combine the plots into one panel



layout <- "
AB
CD
EF
"


figure_3 <- panel_a_weight_gain +
  panel_b_cp +
  panel_c_adf +
  panel_d_ee +
  panel_e_grass_plot +
  panel_f_poison_plant +
  plot_layout(design = layout, guides = "collect") & theme(legend.position = 'bottom')

ggsave(figure_3,file = here('output/figure_3/figure_3.png'),width=10,height=8,dpi=300)
