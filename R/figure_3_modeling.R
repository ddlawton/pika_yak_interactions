# Figure 3 data management, modeling, and visualizing
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 24, 2025
# Purpose: Clean, document, and fully reproduce the analyses and
#          visualisations that produce Figure 3 (A–D).
# -------------------------------------------------------------


library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcomp)
library(DHARMa)
library(ggpubr)
library(here)

here::i_am('README.md')


# read in the data
fig3a_dat <- read_csv(here('data/processed/fig_3a_weight_gain.csv'), show_col_types = FALSE) |>
  mutate(yak = gsub("_[0-9]+$", "", yak))

fig3b_3c_dat <- read_csv(here('data/processed/fig_3bc_s1_plant_cover.csv'), show_col_types = FALSE) 
fig3d_dat <- read_csv(here('data/processed/fig_3d_s2_forage_quality.csv'), show_col_types = FALSE)


# Figure 3A

fig3a_dat |>
  dplyr::select(block,year,month,yak,pika_treatment,weight_gain) |>
  ggplot(aes(x=month,y=weight_gain,color=factor(block))) +
    geom_jitter(width = 0.2) +
    facet_grid(~pika_treatment)


fig3a_mod1 <- glmmTMB(weight_gain ~  pika_treatment * posion_plant_treatment,
  data = fig3a_dat)

fig3a_mod2 <- glmmTMB(weight_gain ~  
    pika_treatment * posion_plant_treatment +
    (1|block),
  data = fig3a_dat)

fig3a_mod3 <- glmmTMB(weight_gain ~  
  pika_treatment * posion_plant_treatment +
  (1|block:yak),
  data = fig3a_dat)

fig3a_mod4 <- glmmTMB(weight_gain ~  
  pika_treatment * posion_plant_treatment +
  (1|block) +
  (1|month),
  data = fig3a_dat)

fig3a_mod5 <- glmmTMB(weight_gain ~  
  pika_treatment * posion_plant_treatment +
  (1|month),
  data = fig3a_dat)

figure3a_AIC_table <- AIC(fig3a_mod1,fig3a_mod2,fig3a_mod3,fig3a_mod4,fig3a_mod5) |>
  as_tibble(rownames = 'model') |>
  mutate(deltaAIC = AIC - min(AIC)) |>
  dplyr::select(-df) |>
  arrange(deltaAIC)
  
figure3a_BIC_table <- BIC(fig3a_mod1,fig3a_mod2,fig3a_mod3,fig3a_mod4,fig3a_mod5) |>
  as_tibble(rownames = 'model') |>
  mutate(deltaBIC = BIC - min(BIC)) |>
    dplyr::select(-df) |>
    arrange(deltaBIC) 
  
model_names <- c(
  "Fixed interactive effects only",
  "Random effect of block",
  "Random effect: block × yak",
  "Random effect: block × yak and month",
  "Random effect: month"
)

figure3a_model_selection_table <- figure3a_AIC_table |>
    left_join(figure3a_BIC_table,by='model') |>
      mutate(model = model_names)

