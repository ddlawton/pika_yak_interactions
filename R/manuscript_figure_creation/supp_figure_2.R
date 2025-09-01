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


# panel a: sedge cover
# panel b: forbs cover
# panel c: NDF %
# panel d: forbs bites
# panel e: forbs bite/steps



# --- Plant Cover ---
cover_data <- read_csv(here('data/processed/experiment_plant_cover/plant_cover_by_treatment.csv'))

forbs_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/emms.csv'))
forbs_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/posthoc_letters.csv'))
forbs_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/model_summary.csv'))
forbs_model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/posthoc_contrasts.csv'))

sedges_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/sedges_cover/emms.csv'))
sedges_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/sedges_cover/posthoc_letters.csv')) |>
  mutate(.group = ifelse(.group=='a','b','a'))
sedges_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/sedges_cover/model_summary.csv'))
sedges__model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/sedges_cover/posthoc_contrasts.csv'))



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


panel_a <- make_cover_plot("sedges", sedges_letters, "Sedges cover (%)", text_y = 70,letters=TRUE)
panel_b <- make_cover_plot("forbs", forbs_letters, "Forbs cover (%)", text_y = 43,letters=TRUE)



# --- NDF % ---


# Load the bite_steps dataset once
forage_quality <- read_csv(here("data/processed/experiment_foraging_quality/plant_foraging_quality.csv"))

# Define a function to load model data for a plant group
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
quality_metrics <- c("ndf")

# Load all model data into a named list
forag_quality_models <- map(quality_metrics, load_forage_quality_data) |> set_names(quality_metrics)


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

panel_c   <- plot_forage_quality("ndf", "ndf (%)",label_y=68,letters=FALSE) + ylim(50,NA)





# --- forb bites and bite/steps ---

# Load the bite_steps dataset once
plant_bites <- read_csv(here("data/processed/experiment_foraging_efficiency/yak_plant_bites.csv")) |>
  mutate(plant = gsub("_[^_]*$", '',plant))

# Define a function to load model data for a plant group
load_plant_bites_data <- function(plant) {
  base_path <- here(glue::glue("data/processed/experiment_foraging_efficiency/modeled_data/plant_bites/{plant}_plant_bites"))
  list(
    emms = read_csv(file.path(base_path, "emms.csv")),
    letters = read_csv(file.path(base_path, "posthoc_letters.csv")),
    model_summary = read_csv(file.path(base_path, "model_summary.csv")),
    model_contrasts = read_csv(file.path(base_path, "posthoc_contrasts.csv"))
  )
}


# Define your plant groups
plant_groups <- c("forbs")

# Load all model data into a named list
plant_bites_models <- map(plant_groups, load_plant_bites_data) |> set_names(plant_groups)

# Reusable function

plot_plant_bites <- function(plant_name, y_label, label_y = NULL) {
  data <- plant_bites |> filter(plant == plant_name)
  emms <- plant_bites_models[[plant_name]]$emms
  letters <- plant_bites_models[[plant_name]]$letters

  if ("emmean" %in% names(emms)) {
    emms <- emms |> rename(response = emmean)
    letters <- letters |> rename(response = emmean)
  }

  label_y <- label_y %||% max(data$forage_efficiency, na.rm = TRUE) + 0.05

  data |>
    left_join(emms, by = c('pika_treatment', 'posion_plant_treatment')) |>
    ggplot(aes(x = pika_treatment, y = forage_efficiency, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = letters,
      aes(x = pika_treatment, y = response, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 8, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    # geom_text(
    #   data = letters,
    #   aes(x = pika_treatment, y = label_y, label = .group, group = posion_plant_treatment),
    #   position = position_dodge(width = 0.7),
    #   size = 5, color = "black", show.legend = FALSE, inherit.aes = FALSE
    # ) +
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
}

# Call the function for each plant type
panel_d  <- plot_plant_bites("forbs", "forb bites", label_y = 200)





# Plant bites


# Load the bite_steps dataset once
plant_bites <- read_csv(here("data/processed/experiment_foraging_efficiency/yak_plant_bites.csv")) |>
  mutate(plant = gsub("_[^_]*$", '',plant))

# Define a function to load model data for a plant group
load_plant_bites_data <- function(plant) {
  base_path <- here(glue::glue("data/processed/experiment_foraging_efficiency/modeled_data/plant_bites/{plant}_plant_bites"))
  list(
    emms = read_csv(file.path(base_path, "emms.csv")),
    letters = read_csv(file.path(base_path, "posthoc_letters.csv")),
    model_summary = read_csv(file.path(base_path, "model_summary.csv")),
    model_contrasts = read_csv(file.path(base_path, "posthoc_contrasts.csv"))
  )
}


# Define your plant groups
plant_groups <- c("forbs")

# Load all model data into a named list
plant_bites_models <- map(plant_groups, load_plant_bites_data) |> set_names(plant_groups)


# Reusable function

plot_plant_bites <- function(plant_name, y_label, label_y = NULL) {
  data <- plant_bites |> filter(plant == plant_name)
  emms <- plant_bites_models[[plant_name]]$emms
  letters <- plant_bites_models[[plant_name]]$letters

  if ("emmean" %in% names(emms)) {
    emms <- emms |> rename(response = emmean)
    letters <- letters |> rename(response = emmean)
  }

  label_y <- label_y %||% max(data$forage_efficiency, na.rm = TRUE) + 0.05

  data |>
    left_join(emms, by = c('pika_treatment', 'posion_plant_treatment')) |>
    ggplot(aes(x = pika_treatment, y = forage_efficiency, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = letters,
      aes(x = pika_treatment, y = response, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 4, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    # geom_text(
    #   data = letters,
    #   aes(x = pika_treatment, y = label_y, label = .group, group = posion_plant_treatment),
    #   position = position_dodge(width = 0.7),
    #   size = 5, color = "black", show.legend = FALSE, inherit.aes = FALSE
    # ) +
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
}

# Call the function for each plant type
panel_d   <- plot_plant_bites("forbs", "forb bites", label_y = 200)

# plant bite steps

# Load the bite_steps dataset once
bite_steps <- read_csv(here("data/processed/experiment_foraging_efficiency/yak_bite_steps_ratio.csv"))

# Define a function to load model data for a plant group
load_bite_step_data <- function(plant) {
  base_path <- here(glue::glue("data/processed/experiment_foraging_efficiency/modeled_data/bite_steps/{plant}_bite_steps"))
  list(
    emms = read_csv(file.path(base_path, "emms.csv")),
    letters = read_csv(file.path(base_path, "posthoc_letters.csv")),
    model_summary = read_csv(file.path(base_path, "model_summary.csv")),
    model_contrasts = read_csv(file.path(base_path, "posthoc_contrasts.csv"))
  )
}

# Define your plant groups
plant_groups <- c("forbs")

# Load all model data into a named list
bite_step_models <- map(plant_groups, load_bite_step_data) |> set_names(plant_groups)


# Reusable function
plot_bite_steps <- function(plant_name, y_label, label_y = NULL, letters = TRUE)  {
  data <- bite_steps |> filter(plant == plant_name)
  emms <- bite_step_models[[plant_name]]$emms
  letters_df <- bite_step_models[[plant_name]]$letters

  label_y <- label_y %||% max(data$bites_steps_ratio, na.rm = TRUE) + 0.05

  p <- data |>
    left_join(emms, by = c('pika_treatment', 'posion_plant_treatment')) |>
    ggplot(aes(x = pika_treatment, y = bites_steps_ratio, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = letters_df,
      aes(x = pika_treatment, y = response, fill = posion_plant_treatment),
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

panel_e   <- plot_bite_steps("forbs", "forbs bite/steps", label_y = 0.4,letters=FALSE)





layout <- "
ABC
DE#
"



supp_fig_2 <- panel_a + panel_b + panel_c + panel_d + panel_e +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect",design = layout) & theme(legend.position = 'bottom')


ggsave(supp_fig_2,file=here('output/supp_figures/supp_fig_2.png'),width = 4, height=8)

