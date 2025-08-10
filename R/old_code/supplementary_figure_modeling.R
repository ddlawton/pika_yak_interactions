# =============================================================
# Supplementary Figures - Data Management, Modeling, and Visualization Script
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 27, 2025
# Purpose:
#   - To reproduce the full analytical and graphical workflow
#     for Figure S1-S3 of the study examining pika-plant
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


# --- Source Utility Functions for Figure 3 (since this is the same data) ---
source(here("functions/figure_3_utils.R"))


# --- Read in data ---
s1_dat <- read_csv(here('data/processed/fig_3bc_s1_plant_cover.csv')) |>
  filter(plant %in% c('sedges','forbs'))

s2_dat <- read_csv(here('data/processed/fig_3d_s2_forage_quality.csv')) |>
  filter(plant %in% c('adf','nsf','ee'))

s3a_dat <- read_csv(here('data/processed/fig_4ac_s3a_forage_efficiency.csv')) |>
  mutate(plant = gsub("_[0-9]+$", "", plant), plant %in% c('forbs')) 

s3b_dat <- read_csv(here('data/processed/fig_4ef_s3b_bites_steps_ratio.csv')) |>
  filter(plant %in% c('forbs'))


# Supplementary Figure 1

s1_dat 


s1_dat |>
  ggplot(aes(x=posion_plant_treatment,y=cover)) +
    geom_jitter(width = 0.2, height = 0) +
    MetBrewer::scale_color_met_d(name = "Lakota") +
    ggpubr::theme_pubr() +
    facet_wrap(~plant)

s1_dat |>
  ggplot(aes(x=pika_treatment,y=cover,color = posion_plant_treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2))  +
    MetBrewer::scale_color_met_d(name = "Lakota") +
    ggpubr::theme_pubr() +
    facet_wrap(~plant)

s1_dat |>
  ggplot(aes(x = month, y = cover, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment,scale = 'free') +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "cover (%)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())


s1_dat |>
  ggplot(aes(x = block, y = cover, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Total bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

# Raw visualizatio  summary
# the differences in cover between the treatments does look too big
# except for the obvious higher sedge cover than forb.
# lets model each plant independently using the figure 3 util functions

s1_model_names <- c(
  "Fixed interactive effects only",
  "Random effect of block",
  "Random effect: block and month",
  "Random effect: month"
)

s1_sedge_dat <- s1_dat |>
    filter(plant == 'sedges') |>
    mutate(year = factor(year))

mod <- glmmTMB(cover ~ 
  pika_treatment * posion_plant_treatment +
  (1 | year) + 
  (1 | month) +
  (1 | block),
data = s1_sedge_dat)

summary(mod)


# Fit and evaluate models
s1a_models <- fit_models("cover ~ pika_treatment * posion_plant_treatment", s1_sedge_dat)
s1a_model_table <- model_selection_table(s1a_models, model_names = s1_model_names)
summary(s1a_models$mod2)





performance::check_model(mod)
