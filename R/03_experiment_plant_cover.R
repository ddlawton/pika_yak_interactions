


fig3b_3c_dat_split <- fig3b_3c_dat |>
    ungroup() |>
    (plant)

names(fig3b_3c_dat_split) <- fig3b_3c_dat |>
  distinct(plant) |>
  pull(plant)

# Function to fit both models to a single plant-level dataset
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

# Apply to each plant type
plant_models <- map(fig3b_3c_dat_split, fit_models)

# Function to generate and save diagnostics for a single model
save_diagnostics <- function(plant_name, model, model_type) {
  png(
    filename = here("output/figure_3/diagnostic_plots", glue::glue("{plant_name}_{model_type}_diagnostics.png")),
    width = 1200, height = 600, res = 150
  )
  
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # 1 row, 2 plots, title space

  # Simulate and plot residuals
  resid <- simulateResiduals(model, n = 1000)
  plotQQunif(resid)
  plotResiduals(resid)

  # Add overall title
  mtext(glue::glue("DHARMa Diagnostics – {plant_name} ({model_type})"), outer = TRUE, cex = 1.2)

  dev.off()
}

# Loop over each plant and model type
iwalk(plant_models, function(models, plant_name) {
  save_diagnostics(plant_name, models$gauss, "gauss")
  save_diagnostics(plant_name, models$beta, "beta")
})


# I will stick with the beta family since it similar fit to the gaussian, but fit better
plant_models_beta <- map(plant_models, "beta")


# Create named plot list per plant
figbc_plots <- map(names(plant_models_beta), function(name) {
  
  data <- fig3b_3c_dat_split[[name]]
  model <- plant_models_beta[[name]]
  
  # Estimated marginal means and comparisons
  emms <- emmeans(model, ~ pika_treatment * posion_plant_treatment, type = "response")
  emms_df <- as_tibble(emms)
  contrasts <- as_tibble(pairs(emms, adjust = "tukey"))
  letters <- cld(emms, adjust = "tukey", Letters = "BAC") |>
    as_tibble() |>
    mutate(.group = str_trim(.group))
  
  # Merge .group letters into original data
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
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank())
}) |> set_names(names(plant_models_beta))


iwalk(figbc_plots, ~ ggsave(here::here("output/figure_3", 
  glue::glue("fig3bc_{.y}.png")), plot = .x, width = 6, height = 5))


