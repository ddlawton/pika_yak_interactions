# --- Load Required Packages ---
library(tidyverse)       # for data wrangling and visualization
library(glmmTMB)         # for GLMMs
library(emmeans)         # for estimated marginal means
library(multcomp)        # for multiple comparisons
library(broom.mixed)     # for tidy model output
library(DHARMa)          # for GLMM diagnostics
library(ggpubr)          # for publication-ready ggplots
library(here)            # for reproducible file paths


fig3b_3c_dat <- read_csv(here("data/processed/fig_3bc_s1_plant_cover.csv"), show_col_types = FALSE) |>
  mutate(year = factor(year),
  block = factor(block),
  cover = cover / 100 + 0.00000000000001)


fig3b_3c_dat |>
  ggplot(aes(x= plant,y=cover)) +
    geom_jitter(width = 0.2, height = 0)


grouped_dat <- fig3b_3c_dat |>
  group_split(plant)

names(grouped_dat) <- fig3b_3c_dat |>
  distinct(plant) |>
  pull(plant)

mod_list <- list()

for (i in seq_along(grouped_dat)){

  loop_dat <- grouped_dat[[i]]

  mod_list[[i]] <- glmmTMB(cover ~
    pika_treatment * posion_plant_treatment +
    (1 | block) +
    (1 | year) +
    (1 | month),
  data = loop_dat,
  family = beta_family(link='logit'))

}

# Assign names to mod_list based on plant names
names(mod_list) <- names(grouped_dat)

summary(mod_list$s_chamaejasme)
summary(mod_list$sedges)
summary(mod_list$grasses)
summary(mod_list$forbs)


summary(mod_list[3])
performance::check_model(mod_list[[1]])

performance::check_model(mod_list$s_chamaejasme)
performance::check_model(mod_list$sedges)
performance::check_model(mod_list$grasses)
performance::check_model(mod_list$forbs)

library(DHARMa)
resid <- simulateResiduals(mod_list$forbs,n=500)
plotQQunif(resid)
plotResiduals(resid)
