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


# panel_a = total bites
# panel_b = sedge bites
# panel_c = grass bites
# panel d = total steps
# panel e = sedge bite steps
# panel f = gras bite steps



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
plant_groups <- c("forbs", "grasses", "sedges","total")

# Load all model data into a named list
plant_bites_models <- map(plant_groups, load_plant_bites_data) |> set_names(plant_groups)



plant_bites_models <- lapply(plant_bites_models, function(x) {
  x$letters <- x$letters |> 
    mutate(.group = ifelse(.group == 'a', 'b', 'a'))
  x
})


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
plant_groups <- c("forbs", "grasses", "sedges")

# Load all model data into a named list
bite_step_models <- map(plant_groups, load_bite_step_data) |> set_names(plant_groups)


bite_step_models <- lapply(bite_step_models, function(x) {
  x$letters <- x$letters |> 
    mutate(.group = ifelse(.group == 'a' | .group == 'A', 'b', 'a'))
  x
})


# bites

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
    geom_text(
      data = letters,
      aes(x = pika_treatment, y = label_y, label = .group, group = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 5, color = "black", show.legend = FALSE, inherit.aes = FALSE
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
}

# Call the function for each plant type
panel_c <- plot_plant_bites("grasses", "Grass bites", label_y = 350)
panel_b  <- plot_plant_bites("sedges", "Sedge bites", label_y = 450)
panel_a   <- plot_plant_bites("total", "Total bites", label_y = 800)



# bite steps


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
      size = 8, pch = 21, color = "black", inherit.aes = FALSE
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
panel_f <- grasses_plot <- plot_bite_steps("grasses", "Grass bite/steps")
panel_e <- sedges_plot  <- plot_bite_steps("sedges", "Sedges bite/steps", label_y = 1)


# total steps

total_steps <- read_csv(here("data/processed/experiment_foraging_efficiency/yak_total_steps.csv")) |>
  mutate(yak = gsub("_[^_]*$", '',yak))



total_steps_models <- load_data(here('data/processed/experiment_foraging_efficiency/modeled_data/total_steps'))

total_steps_models$letters <- total_steps_models$letters |>
    mutate(.group = case_when(
        pika_treatment == 'no pika' & posion_plant_treatment == 'S. chamaejasme' ~ 'b',
        TRUE ~ tolower(.group)
    ))


panel_d <- total_steps |>
  left_join(total_steps_models$emms, by = c('pika_treatment', 'posion_plant_treatment')) |>
  ggplot(aes(x = pika_treatment, y = total_steps, color = posion_plant_treatment)) +
  geom_jitter(
    size = 1, alpha = 0.4,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
  ) +
  geom_point(
    data = total_steps_models$letters,
    aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
    position = position_dodge(width = 0.7),
    size = 8, pch = 21, color = "black", inherit.aes = FALSE
  ) +
  geom_text(
    data = total_steps_models$letters,
    aes(x = pika_treatment, y = 1000, label = .group, group = posion_plant_treatment),
    position = position_dodge(width = 0.7),
    size = 5, color = "black", show.legend = FALSE, inherit.aes = FALSE
  ) +
  scale_fill_manual(values = plant_colors) +
  scale_color_manual(values = plant_colors) +
  labs(
    y = 'total steps',
    x = "",
    color = "Poison plant treatment",
    fill = "Poison plant treatment"
  ) +
  theme_pubr(base_size = 15) +
  theme(legend.position = "bottom", legend.title = element_blank())


# plot together

layout <- "
ABC
DEF
"


figure_4 <- panel_a +
  panel_b +
  panel_c +
  panel_d +
  panel_e +
  panel_f +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = layout, guides = "collect") & theme(legend.position = 'bottom')

ggsave(figure_4,file = here('output/figure_4/figure_4.png'),width=10,height=8,dpi=300)
