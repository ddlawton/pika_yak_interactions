# =============================================================
# Figure 3 - Data Management, Modeling, and Visualization Script
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 24, 2025
# Purpose:
#   - To reproduce the full analytical and graphical workflow
#     for Figure 3 (panels A–D) of the study examining pika-plant
#     interactions on the Tibetan Plateau.
#   - Includes raw data visualization, GLMM fitting with model
#     selection, estimated marginal means analysis, and plot export.
# =============================================================

# --- Load Required Packages ---
library(tidyverse)       # for data wrangling and visualization
library(glmmTMB)         # for GLMMs
library(emmeans)         # for estimated marginal means
library(multcomp)        # for multiple comparisons
library(broom.mixed)     # for tidy model output
library(DHARMa)          # for GLMM diagnostics
library(ggpubr)          # for publication-ready ggplots
library(here)            # for reproducible file paths

# --- Set Root Directory for Project ---
here::i_am("README.md")  # ensures 'here()' resolves paths correctly

# --- Source Utility Functions for Figure 3 ---
source(here("functions/figure_3_utils.R"))

# --- Load Cleaned Data ---
fig3a_dat <- read_csv(here("data/processed/fig_3a_weight_gain.csv"), show_col_types = FALSE) |>
  mutate(yak = gsub("_[0-9]+$", "", yak))  # clean yak ID

fig3b_3c_dat <- read_csv(here("data/processed/fig_3bc_s1_plant_cover.csv"), show_col_types = FALSE)
fig3d_dat <- read_csv(here("data/processed/fig_3d_s2_forage_quality.csv"), show_col_types = FALSE)

# =============================================================
# FIGURE 3A — Yak Weight Gain by Pika and Poison Plant Treatments
# =============================================================

# Raw data plot
fig3a_weight_gain_by_month <- fig3a_dat |>
  select(block, year, month, yak, pika_treatment, weight_gain) |>
  ggplot(aes(x = month, y = weight_gain, color = factor(block))) +
  geom_jitter(width = 0.2) +
  facet_grid(~pika_treatment)

# Fit candidate GLMMs
fig3a_models <- fit_models("weight_gain ~ pika_treatment * posion_plant_treatment", fig3a_dat)

# Name and compare models
fig3a_model_names <- c(
  "Fixed interactive effects only",
  "Random effect of block",
  "Random effect: block and month",
  "Random effect: month"
)

fig3a_model_table <- model_selection_table(fig3a_models, model_names = fig3a_model_names)

# Extract estimated marginal means + compact letter display (Tukey-adjusted)
fig3a_emms <- extract_emms(fig3a_models$mod2, "pika_treatment * posion_plant_treatment")

# Plot EMMs with letters
fig3a_plot <- plot_emms(fig3a_dat, fig3a_emms$letters,
  yvar = "weight_gain", 
  ylab = "Weight gain (kg/yak/day)")

# Random effect plot
fig3a_re_plot <- plot_random_effects(fig3a_models$mod4, fig3a_dat, "month", "weight_gain")

# ---- Summary Notes (Figure 3A) ----
# A two-way fixed-effect ANOVA and the GLMM with block as a random effect gave similar AIC/BIC.
# I selected the GLMM to better reflect the experimental design, though it likely doesn't
# change inference. It's more robust to block-level variation.

# Export model summary and plots
fig3a_model_summary <- tidy(fig3a_models$mod4)

write_csv(fig3a_model_table, here("output/figure_3/tables/figure_3a_model_selection_criteria.csv"))
write_csv(fig3a_model_summary, here("output/figure_3/tables/figure_3a_model_summary.csv"))

ggsave(fig3a_re_plot, here("output/figure_3/plots/figure_3a_plant_month_conditional_mean.png"), width = 5, height = 5)
ggsave(fig3a_weight_gain_by_month, here("output/figure_3/plots/figure_3a_raw_weight_by_month.png"), width = 10, height = 5)

# =============================================================
# FIGURE 3B — S. chamaejasme Cover
# =============================================================

fig3b_dat <- fig3b_3c_dat |>
  filter(plant == "s_chamaejasme")

# Visualize raw cover data
grasscover_treatments_facet_month_plot <- fig3b_dat |>
  ggplot(aes(x = month, y = cover, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Grass cover (%)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) + theme(legend.position = "bottom", legend.title = element_blank())

# Model fitting
fig3b_models <- fit_models("cover ~ pika_treatment * posion_plant_treatment", fig3b_dat)

fig3b_model_table <- model_selection_table(fig3b_models, model_names = fig3a_model_names)

# Opted for `Random effect: month` for consistency
summary(fig3b_models$mod4)

fig3b_emms <- extract_emms(fig3b_models$mod4, "pika_treatment * posion_plant_treatment")
fig3b_plot <- plot_emms(fig3b_dat, fig3b_emms$letters, yvar = "cover", ylab = "S. chamaejasme (%)")
fig3b_re_plot <- plot_random_effects(fig3b_models$mod4, fig3b_dat, "month", "cover")

# ---- Summary Notes (Figure 3B) ----
# S. chamaejasme cover was significantly affected by pika presence.
# There is also a month effect, with cover declining into August.

# Export
fig3b_model_summary <- tidy(fig3b_models$mod4)

write_csv(fig3b_model_table, here("output/figure_3/tables/figure_3b_model_selection_criteria.csv"))
write_csv(fig3b_model_summary, here("output/figure_3/tables/figure_3b_model_summary.csv"))

ggsave(fig3b_re_plot, here("output/figure_3/plots/figure_3b_month_conditional_mean.png"), width = 5, height = 5)
ggsave(grasscover_treatments_facet_month_plot, here("output/figure_3/plots/figure_3b_grasscover_treatments_facet_month.png"), width = 10, height = 5)

# =============================================================
# FIGURE 3C — Grass Cover
# =============================================================

fig3c_dat <- fig3b_3c_dat |>
  filter(plant == "grasses")

cover_by_month_facet_pika <- fig3c_dat |>
  ggplot(aes(x = month, y = cover, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Grass cover (%)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

cover_by_month_facet_block_pika <- cover_by_month_facet_pika +
  facet_grid(block ~ pika_treatment)

# ---- Observational Note (3C) ----
# Pika presence seems to buffer grass cover dynamics.
# When pika are absent, S. chamaejasme plots show greater variability in grass cover.

# Fit and evaluate models
fig3c_models <- fit_models("cover ~ pika_treatment * posion_plant_treatment", fig3c_dat)
fig3c_model_table <- model_selection_table(fig3c_models, model_names = fig3c_model_names)
summary(fig3c_models$mod4)

fig3c_emms <- extract_emms(fig3c_models$mod4, "pika_treatment * posion_plant_treatment")
fig3c_plot <- plot_emms(fig3c_dat, fig3c_emms$letters, yvar = "cover", ylab = "Grasses (%)")
fig3c_re_plot <- plot_random_effects(fig3c_models$mod4, fig3c_dat, "month", "cover")

fig3c_model_summary <- tidy(fig3c_models$mod4)

write_csv(fig3c_model_table, here("output/figure_3/tables/figure_3c_model_selection_criteria.csv"))
write_csv(fig3c_model_summary, here("output/figure_3/tables/figure_3c_model_summary.csv"))

ggsave(fig3c_re_plot, here("output/figure_3/plots/figure_3c_month_conditional_mean.png"), width = 5, height = 5)
ggsave(cover_by_month_facet_pika, here("output/figure_3/plots/figure_3c_cover_by_month_facet_pika.png"), width = 5, height = 5)
ggsave(cover_by_month_facet_block_pika, here("output/figure_3/plots/figure_3c_cover_by_month_facet_block_pika.png"), width = 6, height = 6)

# =============================================================
# FIGURE 3D — Forage Quality (Crude Protein %)
# =============================================================

fig3d_dat <- fig3d_dat |>
  filter(plant == "cp")

cp_by_month_facet_pika <- fig3d_dat |>
  ggplot(aes(x = month, y = forage_quality, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "CP (%)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

cp_by_month_facet_block_pika <- cp_by_month_facet_pika + facet_grid(block ~ pika_treatment)

# ---- Observational Note (3D) ----
# When pika are present, CP % increases across both poison plant treatments.
# Month effects are also more prominent under pika presence.

# Model fitting
fig3d_models <- fit_models("forage_quality ~ pika_treatment * posion_plant_treatment", fig3d_dat)
fig3d_model_table <- model_selection_table(fig3d_models, model_names = fig3d_model_names)
summary(fig3d_models$mod4)

fig3d_emms <- extract_emms(fig3d_models$mod4, "pika_treatment * posion_plant_treatment")
fig3d_plot <- plot_emms(fig3d_dat, fig3d_emms$letters, yvar = "forage_quality", ylab = "CP (%)", y_letter_override = 10)
fig3d_re_plot <- plot_random_effects(fig3d_models$mod4, fig3d_dat, "month", "forage_quality")

fig3d_model_summary <- tidy(fig3d_models$mod4)

write_csv(fig3d_model_table, here("output/figure_3/tables/figure_3d_model_selection_criteria.csv"))
write_csv(fig3d_model_summary, here("output/figure_3/tables/figure_3d_model_summary.csv"))

ggsave(fig3d_re_plot, here("output/figure_3/plots/figure_3d_month_conditional_mean.png"), width = 5, height = 5)
ggsave(cp_by_month_facet_pika, here("output/figure_3/plots/figure_3d_cp_by_month_facet_pika.png"), width = 5, height = 5)
ggsave(cp_by_month_facet_block_pika, here("output/figure_3/plots/figure_3d_cp_by_month_facet_block_pika.png"), width = 5, height = 5)

# =============================================================
# FINAL COMPOSITE FIGURE: FIGURE 3 (A–D)
# =============================================================

fig3_plot <- (fig3a_plot + fig3b_plot) / (fig3c_plot + fig3d_plot) +
  plot_annotation(tag_levels = "A")

ggsave(fig3_plot, file = here("output/figure_3/figure_3.png"), width = 10, height = 10)
