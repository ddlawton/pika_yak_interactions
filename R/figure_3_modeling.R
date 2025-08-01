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
library(glue)


# --- Set Root Directory for Project ---
here::i_am("README.md")  # ensures 'here()' resolves paths correctly

# --- Source Utility Functions for Figure 3 ---
source(here("functions/figure_3_utils.R"))

# --- Load Cleaned Data ---
fig3a_dat <- read_csv(here("data/processed/fig_3a_weight_gain.csv"), show_col_types = FALSE) |>
  mutate(yak = gsub("_[0-9]+$", "", yak),
  treatment_code = paste0(
    ifelse(pika_treatment == "present", "P", "A"),
    ifelse(posion_plant_treatment == "present", "P", "A")
  ),
  # Create final yak ID, like B1_PP_Y1
  yak = paste0("B", block, "_", treatment_code, "_", yak),
  block = factor(block),
  year = factor(year)) |>
  dplyr::select(!treatment_code)

fig3b_3c_dat <- read_csv(here("data/processed/fig_3bc_s1_plant_cover.csv"), show_col_types = FALSE) |>
  group_by(plant) |>
  mutate(
    n_obs = n(),
    cover_beta = ((cover * (n_obs - 1) + 0.5) / n_obs) / 100
  ) |>
  dplyr::select(-n_obs) |>
  mutate(year = factor(year),
  block = factor(block))

fig3d_dat <- read_csv(here("data/processed/fig_3d_s2_forage_quality.csv"), show_col_types = FALSE)


# =============================================================
# FIGURE 3A — Yak Weight Gain by Pika and Poison Plant Treatments
# =============================================================

fig3a_dat |> summary()

fig3a_mod <- glmmTMB(weight_gain ~
  pika_treatment * posion_plant_treatment +
  (1|block) +
  (1|year) +
  (1|month) +
  (1|yak),
data= fig3a_dat)


# Gaussian is fitting better, so lets go with that
resid <- simulateResiduals(fig3a_mod,n=1000)

plotQQunif(resid)
plotResiduals(resid)



summary(fig3a_mod)

# no main effect of pika treatment
# a main effect of posion plant
# an interactive effect of pika x poison
# lets get the estimated marginal means for effects

fig3a_emms <- emmeans(fig3a_mod,spec = ~ 'pika_treatment * posion_plant_treatment',type='response')

fig3a_emms_df <- fig3a_emms |>
  as_tibble()

fig3a_contrasts = as_tibble(pairs(fig3a_emms, adjust = 'tukey'))

fig3a_letters = cld(fig3a_emms, adjust = 'tukey',Letters = 'BAC')  |>
as_tibble() |>
mutate(.group = str_trim(.group))


pink = '#ff8080'
green = '#2fc09b'
  
# now lets plot the main effect
fig3a_plot <- fig3a_dat |>
    left_join(fig3a_letters, by = c("pika_treatment", "posion_plant_treatment")) |>
    ggplot(aes(x = pika_treatment, y = weight_gain, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = fig3a_letters,
      aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      alpha = 1, size = 8, pch = 21, color = "black"
    ) +
    geom_text(
      data = fig3a_letters,
      aes(x = pika_treatment, y = 0.6, label = .group, group = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 5, show.legend = FALSE, color = "black"
    ) +
    scale_fill_manual(values = c(green,pink)) +
    scale_color_manual(values = c(green,pink)) +
    labs(y = 'weight gain kg/yak/day', x = "", color = "Poison plant treatment", fill = "Poison plant treatment") +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank())

fig3a_plot

# now plot the random effect (month and plot) BLUPs

fig3a_blup_plots <- map(names(re_list), function(rname) {
  re_tbl <- re_list[[rname]] |>
    as.data.frame() |>
    rownames_to_column(var = rname) |>
    rename(ranef_intercept = `(Intercept)`) |>
    mutate(conditional_mean = intercept + ranef_intercept)

  # Plot
  ggplot(re_tbl, aes(x = reorder(!!sym(rname), conditional_mean), y = conditional_mean)) +
    geom_point(color = "darkred", size = 3) +
    geom_hline(yintercept = intercept, linetype = "dashed") +
    labs(
      x = rname,
      y = "Conditional Mean",
      title = glue("BLUPs for {rname}")
    ) +
    theme_minimal(base_size = 14)
}) |> set_names(names(re_list))

fig3a_blup_plot <- (fig3a_blup_plots$block + fig3a_blup_plots$year) / (fig3a_blup_plots$month + fig3a_blup_plots$yak) +
    plot_annotation(tag_levels = 'A')

fig3a_blup_plot 


# =============================================================
# Plant cover by Pika and Poison Plant Treatments
# =============================================================

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
















# now lets plot the main effect
fig3a_plot <- fig3a_dat |>
    left_join(fig3a_letters, by = c("pika_treatment", "posion_plant_treatment")) |>
    ggplot(aes(x = pika_treatment, y = weight_gain, color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
      data = fig3a_letters,
      aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      alpha = 1, size = 8, pch = 21, color = "black"
    ) +
    geom_text(
      data = fig3a_letters,
      aes(x = pika_treatment, y = 0.6, label = .group, group = posion_plant_treatment),
      position = position_dodge(width = 0.7),
      size = 5, show.legend = FALSE, color = "black"
    ) +
    scale_fill_manual(values = c(green,pink)) +
    scale_color_manual(values = c(green,pink)) +
    labs(y = 'weight gain kg/yak/day', x = "", color = "Poison plant treatment", fill = "Poison plant treatment") +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank())

fig3a_plot

# now plot the random effect (month and plot) BLUPs

fig3a_blup_plots <- map(names(re_list), function(rname) {
  re_tbl <- re_list[[rname]] |>
    as.data.frame() |>
    rownames_to_column(var = rname) |>
    rename(ranef_intercept = `(Intercept)`) |>
    mutate(conditional_mean = intercept + ranef_intercept)

  # Plot
  ggplot(re_tbl, aes(x = reorder(!!sym(rname), conditional_mean), y = conditional_mean)) +
    geom_point(color = "darkred", size = 3) +
    geom_hline(yintercept = intercept, linetype = "dashed") +
    labs(
      x = rname,
      y = "Conditional Mean",
      title = glue("BLUPs for {rname}")
    ) +
    theme_minimal(base_size = 14)
}) |> set_names(names(re_list))

fig3a_blup_plot <- (fig3a_blup_plots$block + fig3a_blup_plots$year) / (fig3a_blup_plots$month + fig3a_blup_plots$yak) +
    plot_annotation(tag_levels = 'A')















mod_beta <- glmmTMB(cover_beta ~ 
  pika_treatment * posion_plant_treatment +
  (1 | block) +
  (1 | year) +
  (1 | month),
family = beta_family(),
data = fig3b_3c_dat
  )


mod_gauss <- glmmTMB(cover_beta ~ 
  pika_treatment * posion_plant_treatment +
  (1 | block) +
  (1 | year) +
  (1 | month),
family = beta_family(),
data = fig3b_3c_dat
  )





re_tbl <- re_list[['block']] |> 
  as.data.frame() |> 
  rownames_to_column('block') |> 
  rename(ranef_intercept = `(Intercept)`)

intercept <- as.numeric(fixef(fig2b_mod_gauss)$cond["(Intercept)"])
fig3a_block_blups <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

re_list <- ranef(fig2b_mod_gauss)$cond
re_tbl <- re_list[['transect']] |> 
  as.data.frame() |> 
  rownames_to_column('plot') |> 
  rename(ranef_intercept = `(Intercept)`)

intercept <- as.numeric(fixef(fig2b_mod_gauss)$cond["(Intercept)"])
fig2b_transect_blups <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

fig2b_month_blups_plot <- fig2b_month_blups |>
as_tibble() |>
mutate(month = factor(month, levels = c('June','July','August'))) |>
ggplot(aes(x=month,y=conditional_mean)) +
  geom_point(size=3) +
  theme_pubr() +
  ylab('conditional mean (BLUPs)') +
  ylim(0,25)

fig2b_transect_blups_plot <- fig2b_transect_blups |>
as_tibble() |>
mutate(plot = factor(as.integer(plot))) |>
ggplot(aes(x=plot,y=conditional_mean)) +
  geom_point(size=3) +
  theme_pubr() +
  ylab('conditional mean (BLUPs)')  +
  ylim(0,25)

fig2b_blup_plot <- fig2b_month_blups_plot + fig2b_transect_blups_plot
fig2b_blup_plot













# Raw data plot
fig3a_weight_gain_by_month <- fig3a_dat |>
  dplyr::select(block, year, month, yak, pika_treatment, weight_gain) |>
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

ggsave(fig3a_re_plot,file=here("output/figure_3/plots/figure_3a_plant_month_conditional_mean.png"), width = 5, height = 5)
ggsave(fig3a_weight_gain_by_month, file = here("output/figure_3/plots/figure_3a_raw_weight_by_month.png"), width = 10, height = 5)

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

write_csv(fig3b_model_table, file = here("output/figure_3/tables/figure_3b_model_selection_criteria.csv"))
write_csv(fig3b_model_summary, file = here("output/figure_3/tables/figure_3b_model_summary.csv"))

ggsave(fig3b_re_plot, file = here("output/figure_3/plots/figure_3b_month_conditional_mean.png"), width = 5, height = 5)
ggsave(grasscover_treatments_facet_month_plot, file = here("output/figure_3/plots/figure_3b_grasscover_treatments_facet_month.png"), width = 10, height = 5)

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
fig3c_model_table <- model_selection_table(fig3c_models, model_names = fig3a_model_names)
summary(fig3c_models$mod4)

fig3c_emms <- extract_emms(fig3c_models$mod4, "pika_treatment * posion_plant_treatment")
fig3c_plot <- plot_emms(fig3c_dat, fig3c_emms$letters, yvar = "cover", ylab = "Grasses (%)")
fig3c_re_plot <- plot_random_effects(fig3c_models$mod4, fig3c_dat, "month", "cover")

fig3c_model_summary <- tidy(fig3c_models$mod4)

write_csv(fig3c_model_table, file = here("output/figure_3/tables/figure_3c_model_selection_criteria.csv"))
write_csv(fig3c_model_summary, file = here("output/figure_3/tables/figure_3c_model_summary.csv"))

ggsave(fig3c_re_plot, file = here("output/figure_3/plots/figure_3c_month_conditional_mean.png"), width = 5, height = 5)
ggsave(cover_by_month_facet_pika, file = here("output/figure_3/plots/figure_3c_cover_by_month_facet_pika.png"), width = 5, height = 5)
ggsave(cover_by_month_facet_block_pika, file = here("output/figure_3/plots/figure_3c_cover_by_month_facet_block_pika.png"), width = 6, height = 6)

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
fig3d_model_table <- model_selection_table(fig3d_models, model_names = fig3a_model_names)
summary(fig3d_models$mod4)

fig3d_emms <- extract_emms(fig3d_models$mod4, "pika_treatment * posion_plant_treatment")
fig3d_plot <- plot_emms(fig3d_dat, fig3d_emms$letters, yvar = "forage_quality", ylab = "CP (%)", y_letter_override = 10)
fig3d_re_plot <- plot_random_effects(fig3d_models$mod4, fig3d_dat, "month", "forage_quality")

fig3d_model_summary <- tidy(fig3d_models$mod4)

write_csv(fig3d_model_table, file = here("output/figure_3/tables/figure_3d_model_selection_criteria.csv"))
write_csv(fig3d_model_summary, file = here("output/figure_3/tables/figure_3d_model_summary.csv"))

ggsave(fig3d_re_plot, file = here("output/figure_3/plots/figure_3d_month_conditional_mean.png"), width = 5, height = 5)
ggsave(cp_by_month_facet_pika, file = here("output/figure_3/plots/figure_3d_cp_by_month_facet_pika.png"), width = 5, height = 5)
ggsave(cp_by_month_facet_block_pika, file = here("output/figure_3/plots/figure_3d_cp_by_month_facet_block_pika.png"), width = 5, height = 5)

# =============================================================
# FINAL COMPOSITE FIGURE: FIGURE 3 (A–D)
# =============================================================

fig3_plot <- (fig3a_plot + fig3b_plot) / (fig3c_plot + fig3d_plot) +
  plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(fig3_plot, file = here("output/figure_3/figure_3.png"), width = 6, height = 6)
