# Pika and Yak Diet Selection Code
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 24, 2025
# Purpose: Clean, document, and fully reproduce the analyses and
#          visualisations of yak and pika diet selections
# -------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcomp)
library(DHARMa)
library(ggpubr)
library(here)
library(patchwork)
library(mgcv)
library(broom.mixed)


here::i_am('README.md')

# Read in the diet selection data
csv_dir <- here("data/processed/diet_selection")
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

walk(csv_files, function(file) {
  obj_name <- tools::file_path_sans_ext(basename(file))
  assign(obj_name, read_csv(file, show_col_types = FALSE), envir = .GlobalEnv)
})

# ---- Clean Variables ----

pika_feeding <- pika_feeding |>
  mutate(plot = factor(plot)) |>
  mutate(
    n_obs = n(),
    feeding_clipping_freq_beta = ((feeding_clipping_freq * (n_obs - 1) + 0.5) / n_obs) / 100
  ) |>
  dplyr::select(-n_obs)

yak_grazing <- yak_grazing |>
  mutate(transect = factor(transect)) |>
  filter(plant_group != 'S. chamaejasme') |>
  mutate(
    n_obs = n(),
    grazing_freq_beta = ((grazing_freq * (n_obs - 1) + 0.5) / n_obs) / 100
  ) |>
  dplyr::select(-n_obs)

dung_burrow_by_plot <- dung_burrow_by_plot |>
  mutate(plot = factor(plot)) |>
  drop_na() |>
  mutate(
    n_obs = n(),
    s_chamaejasme_cover_percent_beta = ((s_chamaejasme_cover_percent * (n_obs - 1) + 0.5) / n_obs) / 100
  ) |>
  dplyr::select(-n_obs)

# ---- Figure 2A: Pika Feeding ----

pika_feeding_mod_beta <- glmmTMB(
  feeding_clipping_freq_beta ~ plant_group + (1 | plot) + (1 | month),
  data = pika_feeding,
  family = beta_family()
)

pika_feeding_mod_gauss <- glmmTMB(
  feeding_clipping_freq ~ plant_group + (1 | plot) + (1 | month),
  data = pika_feeding,
  family = gaussian()
)

resid <- simulateResiduals(pika_feeding_mod_gauss, n = 500)
plotQQunif(resid)
plotResiduals(resid)
summary(pika_feeding_mod_gauss)

pika_feeding_emms <- emmeans(pika_feeding_mod_gauss, spec = ~plant_group, type = 'response')
pika_feeding_emms_df <- as_tibble(pika_feeding_emms)
pika_feeding_contrasts <- as_tibble(pairs(pika_feeding_emms, adjust = 'tukey'))
pika_feeding_letters <- cld(pika_feeding_emms, adjust = 'tukey', Letters = 'ABCD') |>
  as_tibble() |> mutate(.group = str_trim(.group))

pika_feeding_plot <- pika_feeding |>
  left_join(pika_feeding_emms_df, by = 'plant_group') |>
  ggplot(aes(x = reorder(plant_group, emmean), y = feeding_clipping_freq)) +
  geom_jitter(width = 0.2, height = 0) +
  geom_point(aes(y = emmean), size = 5, color = 'black', pch = 21, fill = 'dark red') +
  geom_text(data = pika_feeding_letters, aes(label = .group, y = 105)) +
  theme_pubr() +
  ylab('feeding/clipping frequency (%)') +
  xlab('')

# Random effects BLUPs for plot and month
re_tbl <- ranef(pika_feeding_mod_gauss)$cond$month |>
  as.data.frame() |> rownames_to_column('month') |> rename(ranef_intercept = `(Intercept)`)
intercept <- fixef(pika_feeding_mod_gauss)$cond["(Intercept)"]
pika_feeding_month_blups <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

re_tbl <- ranef(pika_feeding_mod_gauss)$cond$plot |>
  as.data.frame() |> rownames_to_column('plot') |> rename(ranef_intercept = `(Intercept)`)
pika_feeding_plot_blups <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

pika_feeding_month_blups_plot <- pika_feeding_month_blups |>
  mutate(month = factor(month, levels = c('June', 'July', 'August'))) |>
  ggplot(aes(x = month, y = conditional_mean)) +
  geom_point(size = 3) +
  theme_pubr() +
  ylab('conditional mean (BLUPs)') +
  ylim(0, 50)

pika_feeding_plot_blups_plot <- pika_feeding_plot_blups |>
  mutate(plot = factor(as.integer(plot))) |>
  ggplot(aes(x = plot, y = conditional_mean)) +
  geom_point(size = 3) +
  theme_pubr() +
  ylab('conditional mean (BLUPs)') +
  ylim(0, 50)

pika_feeding_blup_plot <- pika_feeding_month_blups_plot + pika_feeding_plot_blups_plot

# ---- Figure 2B: Yak Grazing ----

yak_grazing_mod_beta <- glmmTMB(
  grazing_freq_beta ~ plant_group + (1 | transect) + (1 | month),
  data = yak_grazing,
  family = beta_family()
)

yak_grazing_mod_gauss <- glmmTMB(
  grazing_freq ~ plant_group + (1 | transect) + (1 | month),
  data = yak_grazing,
  family = gaussian()
)

resid <- simulateResiduals(yak_grazing_mod_gauss, n = 1000)
plotQQunif(resid)
plotResiduals(resid)
summary(yak_grazing_mod_gauss)

yak_grazing_emms <- emmeans(yak_grazing_mod_gauss, spec = ~plant_group, type = 'response')
yak_grazing_emms_df <- as_tibble(yak_grazing_emms)
yak_grazing_contrasts <- as_tibble(pairs(yak_grazing_emms, adjust = 'tukey'))
yak_grazing_letters <- cld(yak_grazing_emms, adjust = 'tukey', Letters = 'ABCD') |>
  as_tibble() |> mutate(.group = str_trim(.group))

yak_grazing_plot <- yak_grazing |>
  left_join(yak_grazing_emms_df, by = 'plant_group') |>
  mutate(plant_group = factor(plant_group, levels = c('S. chamaejasme', 'Forbs', 'Sedges', 'Grasses'))) |>
  ggplot(aes(x = plant_group, y = grazing_freq)) +
  geom_jitter(width = 0.2, height = 0) +
  geom_point(aes(y = emmean), size = 5, color = 'black', pch = 21, fill = 'dark red') +
  geom_text(data = yak_grazing_letters, aes(label = .group, y = 105)) +
  theme_pubr() +
  ylab('grazing frequency (%)') +
  xlab('') +
  scale_x_discrete(breaks = c('S. chamaejasme', 'Forbs', 'Sedges', 'Grasses'), drop = FALSE)

# Random effect BLUPs
re_tbl <- ranef(yak_grazing_mod_gauss)$cond$month |>
  as.data.frame() |> rownames_to_column('month') |> rename(ranef_intercept = `(Intercept)`)
intercept <- fixef(yak_grazing_mod_gauss)$cond["(Intercept)"]
yak_grazing_month_blups <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

re_tbl <- ranef(yak_grazing_mod_gauss)$cond$transect |>
  as.data.frame() |> rownames_to_column('plot') |> rename(ranef_intercept = `(Intercept)`)
yak_grazing_transect_blups <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

yak_grazing_month_blups_plot <- yak_grazing_month_blups |>
  mutate(month = factor(month, levels = c('June', 'July', 'August'))) |>
  ggplot(aes(x = month, y = conditional_mean)) +
  geom_point(size = 3) +
  theme_pubr() +
  ylab('conditional mean (BLUPs)') +
  ylim(0, 25)

yak_grazing_transect_blups_plot <- yak_grazing_transect_blups |>
  mutate(plot = factor(as.integer(plot))) |>
  ggplot(aes(x = plot, y = conditional_mean)) +
  geom_point(size = 3) +
  theme_pubr() +
  ylab('conditional mean (BLUPs)') +
  ylim(0, 25)

yak_grazing_blup_plot <- yak_grazing_month_blups_plot + yak_grazing_transect_blups_plot

# ---- Figure 2C: Pika Burrow vs. S. chamaejasme Cover ----

fig2c_gam_model <- gam(s_chamaejasme_cover_percent ~
  s(active_pika_burrow_no_100m2, k = 8, bs = 'ts') +
    s(plot, bs = 're'),
  data = dung_burrow_by_plot,
  family = gaussian(),
  select = TRUE
)

gratia::draw(fig2c_gam_model, parametric = TRUE)
gratia::appraise(fig2c_gam_model)
summary(fig2c_gam_model)

gam_ests <- gratia::smooth_estimates(fig2c_gam_model)
gam_ests$adj_est <- gam_ests$.estimate + coef(fig2c_gam_model)[1]

fig2c_plot <- dung_burrow_by_plot |>
  ggplot(aes(x = active_pika_burrow_no_100m2, y = s_chamaejasme_cover_percent)) +
  geom_point() +
  geom_line(data = gam_ests, aes(y = adj_est), linewidth = 1.25, color = 'blue') +
  theme_pubr(base_size = 10) +
  xlab('active pika burrow density (no/100m^2)') +
  ylab('S. chamaejasme cover (%)')

# ---- Figure 2D: S. chamaejasme Cover vs. Yak Dung ----

fig2d_plot <- dung_burrow_by_plot |>
  ggplot(aes(x = s_chamaejasme_cover_percent, y = yak_dung_no_100m2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_pubr(base_size = 10) +
  ylab('Dung density (no/100m^2)') +
  xlab('S. chamaejasme cover (%)') +
  ylim(0, 25) +
  xlim(0, 35)

# ---- Export Model Tables ----


# Example: list of models
model_list <- list(
  pika_feeding = pika_feeding_mod_gauss,
  yak_grazing = yak_grazing_mod_gauss
)

# Function to compute and save outputs
save_model_outputs <- function(model, name) {
  # Define base output path
  base_dir <- here("data/processed/diet_selection/modeled_data", name)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  # === Model summary ===
  model_summary <- broom.mixed::tidy(model)
  write_csv(model_summary, file.path(base_dir, "model_summary.csv"))
  
  # === Estimated Marginal Means ===
  emms <- emmeans(model, ~ plant_group, type = "response")
  emms_df <- as_tibble(emms)
  write_csv(emms_df, file.path(base_dir, "emms.csv"))
  
  # === Post hoc contrasts ===
  contrasts <- as_tibble(pairs(emms))
  write_csv(contrasts, file.path(base_dir, "posthoc_contrasts.csv"))
  
  # === Post hoc letters (grouping) ===
  letters <- cld(emms, Letters = letters, type = "response") |>
    as_tibble() 

  write_csv(letters, file.path(base_dir, "posthoc_letters.csv"))
}

# Apply to each model in the list
imap(model_list, save_model_outputs)

gam_model <- bind_rows(
  tidy(fig2c_gam_model, parametric = TRUE),
  tidy(fig2c_gam_model, parametric = FALSE)
)

write_csv(gam_model, file = here("data/processed/diet_selection/modeled_data/pika_burrow_density/gam_model.csv"))

