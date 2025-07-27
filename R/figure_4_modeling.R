# =============================================================
# Figure 4 - Data Management, Modeling, and Visualization Script
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 27, 2025
# Purpose:
#   - To reproduce the full analytical and graphical workflow
#     for Figure 4 (panels a-f) of the study examining pika-plant
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


# --- Load Cleaned Data ---
fig4ac_dat <- read_csv(here("data/processed/fig_4ac_s3a_forage_efficiency.csv"), show_col_types = FALSE)

fig4d_dat <- read_csv(here("data/processed/fig_4d_total_steps.csv"), show_col_types = FALSE)
fig4ef_dat <- read_csv(here("data/processed/fig_4ef_s3b_bites_steps_ratio.csv"), show_col_types = FALSE)


# =============================================================
# FIGURE 4A — Yak oraging efficiency -- total bites
# =============================================================

fig4a_dat <- fig4ac_dat |>
    filter(str_detect(plant,'total'))


fig4a_dat |>
  ggplot(aes(x=pika_treatment,y=forage_efficiency,color = posion_plant_treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2))

fig4a_dat |>
  ggplot(aes(x = month, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Total bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())


unique(fig4ac_dat$plant)
