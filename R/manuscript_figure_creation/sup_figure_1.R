
# --- Plant Cover ---
cover_data <- read_csv(here('data/processed/experiment_plant_cover/plant_cover_by_treatment.csv'))

forbs_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/emms.csv'))
forbs_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/posthoc_letters.csv'))
forbs_model_summary  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/model_summary.csv'))
forbs_model_contrasts  <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/forbs_cover/posthoc_contrasts.csv'))

sedges_emms <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/sedges_cover/emms.csv'))
sedges_letters <- read_csv(here('data/processed/experiment_plant_cover/modeled_data/sedges_cover/posthoc_letters.csv'))
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


panel_a   <- make_cover_plot("sedges", sedges_letters, "grass cover (%)", text_y = 70,letters=TRUE)
panel_b <- make_cover_plot("forbs", forbs_letters, "S. chamaejasme cover (%)", text_y = 43,letters=TRUE)


supp_fig_1 <- panel_a / panel_b +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')


ggsave(supp_fig_1,file=here('output/supp_figures/supp_fig_1.png'),width = 4, height=8)
