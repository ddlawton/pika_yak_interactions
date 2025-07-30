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

# modeling steps
# so for plots A-F, the same basic modeling structure should be used
# with differing response variables. Lets start with the most complex 
# we have the following variables:
# pika_treatment (two levels), posion_plant_treatment (two levels), block (4 levels),
# yak (2 yaks per treatment plot) and # month (three months)
# I think I will test these following structures, from most to least complex:

# response ~ pika_treatment * posion_plant_treatment + (1|block) + (1|block:yak:month) 
# response ~ pika_treatment * posion_plant_treatment + (1 | block) + (1 | month)
# response ~ pika_treatment * posion_plant_treatment + (1 | block)
# response ~ pika_treatment * posion_plant_treatment + (1 | month)
# response ~ pika_treatment * posion_plant_treatment 

# I have built functions to help reduce redunancy in this script

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


# --- Source Utility Functions for Figure 4 ---
source(here("functions/figure_4_utils.R"))

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
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2))  +
    MetBrewer::scale_color_met_d(name = "Lakota")

fig4a_dat |>
  ggplot(aes(x = month, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Total bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

fig4a_dat |>
  ggplot(aes(x = yak, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Total bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(posion_plant_treatment~block)


fig4a_dat |>
  ggplot(aes(x = block, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Total bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

# raw data viz summary
# total bites per yak differed in posion plant treatment between in the no pika treatments
# with the exception of s chamaejasme plots in the no poike treatments, pika and no pika treatments 
# didnt vary all that much
# really no differnce months or yak 
# there might be some differences between the blocks.

# Figure 4A modeling

fig4a_models <- fit_candidate_models('forage_efficiency',
  data = fig4a_dat,
  family = gaussian())

fig4a_model_selection_table <- fig4a_models$table

# fixed effect only model, month ranef, and block ranef models
# have the must suuport.
# I think that keeping the block as a random effect makes the most since
# given that this follows the experimental design closely


fig4a_best_model <- fig4a_models$models$mod_b
summary(fig4a_best_model)

#diagnostics
performance::check_model(fig4a_best_model)

# Extract EMMs

fig4a_emms <- extract_emms(fig4a_best_model, 
  ~ pika_treatment * posion_plant_treatment, 
  adjust = "sidak")

# now plotting fixed effect

fig4a_plot <- plot_emms(data = fig4a_dat, 
  emms_letters = fig4a_emms$cld, 
  yvar = 'forage_efficiency', 
  ylab = 'total bites\\h', 
  y_letter_override = 800) 

# now plotting random effect

fig4a_ranef_plot <- plot_random_effects(model = fig4a_best_model, 
  data = fig4a_dat, 
  grouping_var = 'block', 
  response = 'forage_efficiency') 

# figure 4A modeling summary
# there was no difference between no pika and pika
# there was an interactive effect with no pike x s. chamaejasme treatment
# having lower bites overall. All other combinations were very similiar
# block as suggested by the BLUPs, really had a small impact

# Figure 4B

fig4b_dat <- fig4ac_dat |>
    filter(str_detect(plant,'sedges'))


# Raw visualization

fig4b_dat |>
  ggplot(aes(x=pika_treatment,y=forage_efficiency,color = posion_plant_treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2))  +
    MetBrewer::scale_color_met_d(name = "Lakota")

fig4b_dat |>
  ggplot(aes(x = month, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Sedge bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

fig4b_dat |>
  ggplot(aes(x = yak, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Demuth") +
  labs(y = "Sedge bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(posion_plant_treatment~block)


fig4b_dat |>
  ggplot(aes(x = block, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Sedge bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

# Figure 4B raw visualization summary
# this looks very similiar to 4A.
# pika v no pika -- very similiar probably not significant
# pika x posion treat -- probably signficant following same pattern as Figure 4A
# little variation in block or yak. maybe some with month.

# Figure 4B modeling

fig4b_models <- fit_candidate_models('forage_efficiency',
  data = fig4b_dat,
  family = gaussian())

fig4b_model_selection_table <- fig4b_models$table

# fixed effect only model, month ranef, and block ranef models
# have the must suuport.
# I think that keeping the block as a random effect makes the most since
# given that this follows the experimental design closely


fig4b_best_model <- fig4b_models$models$mod_b
summary(fig4b_best_model)

#diagnostics
performance::check_model(fig4b_best_model)

# Extract EMMs

fig4b_emms <- extract_emms(fig4b_best_model, 
  ~ pika_treatment * posion_plant_treatment, 
  adjust = "sidak")

# now plotting fixed effect

fig4b_plot <- plot_emms(data = fig4b_dat, 
  emms_letters = fig4b_emms$cld, 
  yvar = 'forage_efficiency', 
  ylab = 'sedge bites\\h', 
  y_letter_override = 460) 

# now plotting random effect

fig4b_ranef_plot <- plot_random_effects(model = fig4b_best_model, 
  data = fig4b_dat, 
  grouping_var = 'block', 
  response = 'forage_efficiency') 


# figure 4B modeling summary
# As suggested by raw visualization, the story is very similiar to Figure 4A
# there was no difference between no pika and pika
# there was an interactive effect with no pike x s. chamaejasme treatment
# having lower bites overall. All other combinations were very similiar
# block as suggested by the BLUPs, really had a small impact
# but i chose the glmm model just to keep things consistent and due to the experimental design

# Figure 4C

fig4c_dat <- fig4ac_dat |>
    filter(str_detect(plant,'grasses'))

fig4c_dat |>
  ggplot(aes(x=pika_treatment,y=forage_efficiency,color = posion_plant_treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2))  +
    MetBrewer::scale_color_met_d(name = "Lakota")

fig4c_dat |>
  ggplot(aes(x = month, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "grass bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

fig4c_dat |>
  ggplot(aes(x = yak, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Demuth") +
  labs(y = "grass bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(posion_plant_treatment~block)

fig4c_dat |>
  ggplot(aes(x = block, y = forage_efficiency, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "grass bites/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

# raw data viz summary
# very similiar 
# total bites per yak differed in posion plant treatment between in the no pika treatments
# with the exception of s chamaejasme plots in the no poike treatments, pika and no pika treatments 
# didnt vary all that much
# really no differnce months or yak 
# there might be some differences between the blocks.

# Figure 4C modeling

fig4c_models <- fit_candidate_models('forage_efficiency',
  data = fig4c_dat,
  family = gaussian())

fig4c_model_selection_table <- fig4c_models$table

# fixed effect only model, month ranef, and block ranef models
# have the most support.
# I think that keeping the block as a random effect makes the most since
# given that this follows the experimental design closely


fig4c_best_model <- fig4c_models$models$mod_b
summary(fig4c_best_model)

#diagnostics
performance::check_model(fig4c_best_model)

# Extract EMMs

fig4c_emms <- extract_emms(fig4c_best_model, 
  ~ pika_treatment * posion_plant_treatment, 
  adjust = "sidak")

# now plotting fixed effect

fig4c_plot <- plot_emms(data = fig4c_dat, 
  emms_letters = fig4c_emms$cld, 
  yvar = 'forage_efficiency', 
  ylab = 'grass bites\\h', 
  y_letter_override = 320) 

# now plotting random effect

fig4c_ranef_plot <- plot_random_effects(model = fig4c_best_model, 
  data = fig4c_dat, 
  grouping_var = 'block', 
  response = 'forage_efficiency') 

# Figure 4C modeling summary
# There was no main effect of pika treatment on forage efficiency.
# S. chamaejasme treatment alone significantly reduced bites.
# However, this effect was moderated by an interaction: in pika-present plots,
# the negative impact of S. chamaejasme was largely mitigated.
# In other words, S. chamaejasme only lowered bites when pika were absent.
# All other combinations resulted in similar bite rates.
# Block-level variation was minimal, with low random intercept variance.

# Figure 4D
fig4d_dat <- fig4d_dat |>
  mutate(yak = gsub("_[^_]*$", "", yak))


fig4d_dat |>
  ggplot(aes(x=pika_treatment,y=total_steps,color = posion_plant_treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2))  +
    MetBrewer::scale_color_met_d(name = "Lakota")

fig4d_dat |>
  ggplot(aes(x = month, y = total_steps, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "total steps/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

fig4d_dat |>
  ggplot(aes(x = yak, y = total_steps, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Demuth") +
  labs(y = "total steps/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(posion_plant_treatment~block)

fig4d_dat |>
  ggplot(aes(x = block, y = total_steps, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "total steps/h", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())


# raw data viz summary
# It appears that yaks took more steps in the no pika x S. chamaejasme plots as compare to other treatments
# doesnt look like a strong effect of block, yak, or month on total steps.


# Figure 4D modeling

fig4d_models <- fit_candidate_models('total_steps',
  data = fig4d_dat,
  family = gaussian())

fig4d_model_selection_table <- fig4d_models$table

# fixed effect only model, month ranef, and block ranef models
# have the must suuport.
# the full model didnt converged which isnt a problem
# I think that keeping the block as a random effect makes the most since
# given that this follows the experimental design closely


fig4d_best_model <- fig4d_models$models$mod_b
summary(fig4d_best_model)

#diagnostics
performance::check_model(fig4d_best_model)

# Extract EMMs

fig4d_emms <- extract_emms(fig4d_best_model, 
  ~ pika_treatment * posion_plant_treatment, 
  adjust = "sidak")

# now plotting fixed effect

fig4d_plot <- plot_emms(data = fig4d_dat, 
  emms_letters = fig4d_emms$cld, 
  yvar = 'total_steps', 
  ylab = 'total steps\\h', 
  y_letter_override = 1000) 

# now plotting random effect

fig4d_ranef_plot <- plot_random_effects(model = fig4d_best_model, 
  data = fig4d_dat, 
  grouping_var = 'block', 
  response = 'total_steps') 

# Figure 4D modeling summary
# There was no main effect of pika treatment on total steps taken.
# S. chamaejasme treatment alone significantly increased total steps.
# However, there was a strong interaction: in pika-present plots,
# this increase in steps was entirely reversed.
# That is, S. chamaejasme increased movement only when pika were absent.
# All other treatment combinations showed relatively similar movement levels.
# Block-level variance was minimal, suggesting low site-level influence.


# =============================================================
# Figure 4E — Foraging efficiency (bites/step ratio) — Sedges
# =============================================================

fige_dat <- fig4ef_dat |>
  filter(plant == 'sedges') |>
  rename(yak = yak_num)

# Raw visualizations
fige_dat |>
  ggplot(aes(x = pika_treatment, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2)) +
  MetBrewer::scale_color_met_d(name = "Lakota")

fige_dat |>
  ggplot(aes(x = month, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Bites/step (sedges)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

fige_dat |>
  ggplot(aes(x = yak, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Demuth") +
  labs(y = "Bites/step (sedges)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(posion_plant_treatment~block)

fige_dat |>
  ggplot(aes(x = block, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Bites/step (sedges)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

# Raw visualization summary
# very similiar to all other panels. THe interactive effect of the pike and posion plant treatments

# Modeling
fig4e_models <- fit_candidate_models(
  response = 'bites_steps_ratio',
  data = fige_dat,
  family = gaussian()
)

fig4e_model_selection_table <- fig4e_models$table

fig4e_best_model <- fig4e_models$models$mod_b
summary(fig4e_best_model)

performance::check_model(fig4e_best_model)

fig4e_emms <- extract_emms(
  model = fig4e_best_model,
  spec = ~ pika_treatment * posion_plant_treatment,
  adjust = "sidak"
)

fig4e_plot <- plot_emms(
  data = fige_dat,
  emms_letters = fig4e_emms$cld,
  yvar = 'bites_steps_ratio',
  ylab = 'sedge bites per step',
  y_letter_override = 1
)

fig4e_ranef_plot <- plot_random_effects(
  model = fig4e_best_model,
  data = fige_dat,
  grouping_var = 'block',
  response = 'bites_steps_ratio'
)

# Figure 4E modeling summary
# There was no main effect of pika treatment on sedge bite-to-step ratio.
# S. chamaejasme treatment significantly reduced the ratio of sedge bites to steps.
# However, this effect was reversed in pika-present plots:
# the negative effect of S. chamaejasme was almost entirely offset.
# As a result, bite efficiency was only reduced when pika were absent.
# Other combinations showed very similar efficiency levels.
# Block-level variation was minimal, with very low random effect variance.



# =============================================================
# Figure 4F — Foraging efficiency (bites/step ratio) — Grasses
# =============================================================

figf_dat <- fig4ef_dat |>
  filter(plant == 'grasses')  |>
  rename(yak = yak_num)
  

# Raw visualizations
figf_dat |>
  ggplot(aes(x = pika_treatment, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2)) +
  MetBrewer::scale_color_met_d(name = "Lakota")

figf_dat |>
  ggplot(aes(x = month, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Bites/step (grasses)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

figf_dat |>
  ggplot(aes(x = yak, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Demuth") +
  labs(y = "Bites/step (grasses)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(posion_plant_treatment~block)

figf_dat |>
  ggplot(aes(x = block, y = bites_steps_ratio, color = posion_plant_treatment)) +
  geom_jitter(size = 1, position = position_jitterdodge(jitter.width = 0.2)) +
  facet_wrap(~pika_treatment) +
  MetBrewer::scale_color_met_d(name = "Lakota") +
  labs(y = "Bites/step (grasses)", color = "Poison plant treatment") +
  theme_pubr(base_size = 10) +
  theme(legend.position = "bottom", legend.title = element_blank())

# It is the same story

# Modeling
fig4f_models <- fit_candidate_models(
  response = 'bites_steps_ratio',
  data = figf_dat,
  family = gaussian()
)

fig4f_model_selection_table <- fig4f_models$table

fig4f_best_model <- fig4f_models$models$mod_b
summary(fig4f_best_model)

performance::check_model(fig4f_best_model)

fig4f_emms <- extract_emms(
  model = fig4f_best_model,
  spec = ~ pika_treatment * posion_plant_treatment,
  adjust = "sidak"
)

fig4f_plot <- plot_emms(
  data = figf_dat,
  emms_letters = fig4f_emms$cld,
  yvar = 'bites_steps_ratio',
  ylab = 'grass bites per step',
  y_letter_override = 0.7
)

fig4f_ranef_plot <- plot_random_effects(
  model = fig4f_best_model,
  data = figf_dat,
  grouping_var = 'block',
  response = 'bites_steps_ratio'
)

# Figure 4F modeling summary
# There was no main effect of pika treatment on bite-to-step ratio.
# S. chamaejasme treatment significantly reduced foraging efficiency.
# This effect was again moderated by an interaction: in the presence of pika,
# the negative impact of S. chamaejasme was partially reversed.
# That is, forage efficiency dropped only when pika were absent and S. chamaejasme was present.
# All other combinations produced similar bite efficiency levels.
# Block-level variation was negligible, with near-zero random intercept variance.


# =============================================================
# FINAL COMPOSITE FIGURE: FIGURE 4 (A–F)
# =============================================================

fig4_plot <- (fig4a_plot | fig4b_plot | fig4c_plot) / (fig4d_plot | fig4e_plot | fig4f_plot) +
    plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
ggsave(fig4_plot, file = here("output/figure_4/figure_4.png"), width = 10, height = 5)
  