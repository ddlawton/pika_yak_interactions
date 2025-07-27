# -------------------------------------------------------------
# Figure 3: Modeling, EMMeans, and Plotting Utilities
# Author: Douglas Lawton
# Date: July 27, 2025
# Purpose: Consolidated functions to reduce redundancy in 
# modeling, visualizing, and analyzing data for Figure 3 A–D.
# -------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcomp)
library(DHARMa)
library(ggpubr)
library(here)
library(MetBrewer)
here::i_am("README.md")

# ----------------------------
# Generalized modeling utility
# ----------------------------
fit_models <- function(formula_base, data) {
  list(
    mod1 = glmmTMB(as.formula(formula_base), data = data),
    mod2 = glmmTMB(as.formula(paste(formula_base, "+ (1|block)")), data = data),
    mod3 = glmmTMB(as.formula(paste(formula_base, "+ (1|block) + (1|month)")), data = data),
    mod4 = glmmTMB(as.formula(paste(formula_base, "+ (1|month)")), data = data)
  )
}

model_selection_table <- function(models, model_names) {
  aic_tbl <- AIC(models$mod1, models$mod2, models$mod3, models$mod4) |>
    as_tibble(rownames = "model") |>
    mutate(deltaAIC = AIC - min(AIC)) |>
    dplyr::select(-df)

  bic_tbl <- BIC(models$mod1, models$mod2, models$mod3, models$mod4) |>
    as_tibble(rownames = "model") |>
    mutate(deltaBIC = BIC - min(BIC)) |>
      dplyr::select(-df)

  aic_tbl |>
    left_join(bic_tbl, by = "model") |>
    mutate(model_name = model_names)
}

extract_emms <- function(model, specs, adjust = "sidak") {
  # Allow either a character like "a * b" or a one-sided formula like ~ a * b
  if (is.character(specs)) {
    specs <- as.formula(paste("~", specs))
  }
  em <- emmeans(model, specs = specs)
  list(
    emms       = as_tibble(em),
    contrasts  = as_tibble(pairs(em, adjust = adjust)),
    letters    = cld(em, Letters = letters, adjust = adjust) |>
                   as_tibble() |>
                   mutate(.group = str_trim(.group))
  )
}

plot_emms <- function(data, emms_letters, yvar, ylab, y_letter_override = NULL) {
  dodge_width <- 0.75
  y_letter_override <- y_letter_override %||% (if (yvar == "weight_gain") 0.55 else 42)

  data |>
    left_join(emms_letters, by = c("pika_treatment", "posion_plant_treatment")) |>
    ggplot(aes(x = pika_treatment, y = .data[[yvar]], color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width)
    ) +
    geom_point(
      data = emms_letters,
      aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
      position = position_dodge(width = dodge_width),
      alpha = 1, size = 4, pch = 21, color = "black"
    ) +
    geom_text(
      data = emms_letters,
      aes(x = pika_treatment, y = y_letter_override, label = .group, group = posion_plant_treatment),
      position = position_dodge(width = dodge_width),
      size = 5, show.legend = FALSE, color = "black"
    ) +
    MetBrewer::scale_fill_met_d(name = "Lakota") +
    MetBrewer::scale_color_met_d(name = "Lakota") +
    labs(y = ylab, x = "", color = "Poison plant treatment", fill = "Poison plant treatment") +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank())
}

plot_random_effects <- function(model, data, grouping_var, response) {
  re_list <- ranef(model)$cond
  if (!grouping_var %in% names(re_list)) {
    stop("`grouping_var` must be one of: ", paste(names(re_list), collapse = ", "))
  }

  re_tbl <- re_list[[grouping_var]] |> 
    as.data.frame() |> 
    rownames_to_column(grouping_var) |> 
    rename(ranef_intercept = `(Intercept)`)

  intercept <- as.numeric(fixef(model)$cond["(Intercept)"])
  re_tbl <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

  # make join keys characters to be safe
  data2 <- data
  data2[[grouping_var]] <- as.character(data2[[grouping_var]])
  re_tbl[[grouping_var]] <- as.character(re_tbl[[grouping_var]])

  joined <- left_join(data2, re_tbl, by = grouping_var)

  ggplot(joined, aes(x = .data[[grouping_var]], y = .data[[response]])) +
    geom_jitter(width = 0.2, height = 0, size = 0.5, pch = 21) +
    geom_point(aes(y = conditional_mean), color = "blue", size = 2) +
    xlab(grouping_var) + ylab(response) +
    facet_grid(pika_treatment ~ posion_plant_treatment) +
    theme_pubr()
}

