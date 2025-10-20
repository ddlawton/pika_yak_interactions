# ===============================================================
# Script: 03_active_burrow_yak_gain_analysis.R
# Author: Douglas Lawton
# Date: 2025-10-18
# Project: Pika Facilitates Yaks by Suppressing Poisonous Plants
# ===============================================================
#
# Description:
# This script analyzes the relationship between the number of active pika burrows 
# and average yak weight gain under different fencing and treatment conditions.
# The analysis extends Figure 3 of the main manuscript by incorporating new data 
# (received Oct 1, 2025) on active burrow counts within fencing plots.
#
# Objectives:
#   1. Compute the average number of active burrows by treatment and month.
#   2. Merge with yak weight gain data (yak1 + yak2).
#   3. Filter for treatments with both pika and *S. chamaejasme* (yellow-highlighted plots).
#   4. Quantify the relationship between burrow activity and yak weight gain 
#      (e.g., correlation or mixed-effects modeling).
#   5. Generate updated visualizations to supplement or replace Figure 3.
#   6. Summarize key findings for manuscript integration.
#
# Notes:
# - Source: Dataset provided by Zhiwei Zhong (Oct 1, 2025)
# - Output: Updated plots and statistical summaries demonstrating pika–yak interaction.
#
# ===============================================================


# --- Load Required Packages ---
library(tidyverse)       # Data wrangling and plotting
library(glmmTMB)         # Generalized linear mixed models
library(mgcv)
library(gratia)
library(emmeans)         # Estimated marginal means
library(multcomp)        # Multiple comparisons
library(broom.mixed)     # Tidy model outputs from mixed models
library(DHARMa)          # GLMM diagnostics
library(ggpubr)          # Publication-ready ggplots
library(here)            # Relative file paths
library(glue)            # String interpolation

i_am('README.md')


# --- Custom Color Palette ---
pink <- '#ff8080'
green <- '#2fc09b'


plant_colors <- c(green, pink)

# Read in data
burrow_dat <- read_csv(here('data/processed/additional_data/active_burrows.csv'),
  show_col_types = FALSE)

yak_dat <- read_csv(here('data/processed/additional_data/yak_weight_gain.csv'),
  show_col_types = FALSE)


# active burrows by treatment

burrow_mod <- glmmTMB(active_burrows ~ pika_treatment * posion_plant_treatment +
  (1 | block) + (1 | month), data = burrow_dat)

burrow_emms <- emmeans(burrow_mod, ~pika_treatment * posion_plant_treatment) |>
  as_tibble()

tidy(burrow_mod)



ggplot(burrow_dat, aes(x = pika_treatment, y = active_burrows, 
  color = posion_plant_treatment)) +
    geom_jitter(
    size = 1, alpha = 0.4,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
    data = burrow_emms,
    aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
    position = position_dodge(width = 0.7),
    size = 4, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    scale_fill_manual(values = plant_colors) +
    scale_color_manual(values = plant_colors) +
    labs(x = NULL, y = 'No. active burrows/ha', color = NULL, fill = NULL) +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom")




# active burrows by treatment

burrow_mod <- glmmTMB(active_burrows ~ pika_treatment * posion_plant_treatment +
  (1 | block) + (1 | month), data = burrow_dat)

burrow_emms <- emmeans(burrow_mod, ~pika_treatment * posion_plant_treatment) |>
  as_tibble()

ggplot(burrow_dat, aes(x = pika_treatment, y = active_burrows, 
  color = posion_plant_treatment)) +
    geom_jitter(
    size = 1, alpha = 0.4,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)
    ) +
    geom_point(
    data = burrow_emms,
    aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
    position = position_dodge(width = 0.7),
    size = 4, pch = 21, color = "black", inherit.aes = FALSE
    ) +
    scale_fill_manual(values = plant_colors) +
    scale_color_manual(values = plant_colors) +
    labs(x = NULL, y = 'No. active burrows/ha', color = NULL, fill = NULL) +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom")

# Weight gain by active burrow

se <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

burrow_dat 

summed_yak <- yak_dat |>
  group_by(block,year,month,pika_treatment,posion_plant_treatment) |>
    summarize(weight_gain_mean = mean(weight_gain),
      weight_gain_se = se(weight_gain))
      

combined_dat <- summed_yak |>
  left_join(burrow_dat,by=c('block','year','month','pika_treatment','posion_plant_treatment')) |>
    filter(pika_treatment != 'no pika' & posion_plant_treatment != 'No S. chamaejasme') |>
    mutate(block = factor(block),
          month = factor(month),
          year = factor(year)
)

weight_gain_mod <- gam(
  weight_gain_mean ~
    s(active_burrows,bs='tp',k=15) +
    s(block,bs='re') +
    s(year,bs='re') +
    s(month,bs='re'),
  select=TRUE,
  data=combined_dat
)

ests <- smooth_estimates(weight_gain_mod)
ests$adj_est <- ests$.estimate + coef(weight_gain_mod)[1] 

ests  |>
  ggplot(aes(x=active_burrows,y=adj_est)) +
    geom_point(data=combined_dat,aes(y=weight_gain_mean),color = plant_colors[2]) +
    #geom_ribbon(aes(ymin=adj_est-.se, ymax=adj_est+.se)) +
    geom_line(aes(y = adj_est),size=1) + 
    theme_pubr() +
    xlab('No. active burrows/ha') +
    ylab('Weight gain kg/yak/day') +
    xlim(0,500)  +
    ylim(0,0.5)



# Now prepare the data for inclusion in the manuscript into 'data/processed/diet_selection/modeled_data'
# write out to csv two files one per figure. be sure to include the modeled estimates that are used in the ggplots as well

# active burrows
burrow_final_dat <- burrow_dat |>
    left_join(burrow_emms,by=c('pika_treatment','posion_plant_treatment'))

write_csv(burrow_final_dat,file=here('data/processed/additional_data/modeled_data/active_burrow_dat.csv'))

# active burrow x weight vain

weight_gain_raw <- combined

write_csv(combined_dat,file=here('data/processed/additional_data/modeled_data/weight_gain_raw.csv'))

write_csv(ests,file=here('data/processed/additional_data/modeled_data/weight_gain_modeled_dat.csv'))




# Exporting model output tables

# Function to compute and save outputs
save_model_outputs <- function(model, path,emm_variable) {
  # Define base output path
  base_dir <- here(path)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  # === Model summary ===
  model_summary <- broom.mixed::tidy(model)
  write_csv(model_summary, file.path(base_dir, "model_summary.csv"))
  
  # === Estimated Marginal Means ===
  emm_formula <- as.formula(paste("~", emm_variable))
  emms <- emmeans(model, emm_formula)
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


save_model_outputs(burrow_mod,"data/processed/additional_data/modeled_data/active_burrow_count",emm_variable= 'pika_treatment * posion_plant_treatment')
save_model_outputs(weight_gain_mod,"data/processed/additional_data/modeled_data/weight_gain",emm_variable= 'active_burrows')
