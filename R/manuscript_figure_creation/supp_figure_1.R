library(tidyverse)
library(ggpubr)
library(patchwork)
library(here)
library(mgcv)
library(knitr)
library(kableExtra)

# --- Set Root Directory for Project ---
here::i_am("README.md")  # Anchor project for reproducible paths



# --- Custom Color Palette ---
pink <- '#ff8080'
green <- '#2fc09b'
plant_colors <- c(green, pink)


dat <- read_csv(here('data/processed/initial_conditions/initial_plant_condition.csv')) |>
  pivot_longer(cols = !c('block','pika_treatment','poisonous_plants_treatment'))

unique(dat$name)


panel_a <- dat |>
  filter(name == 's_chamaejasme_cover_percent') |>
  ggplot(aes(x=pika_treatment,y = value, color = factor(block))) +
    geom_jitter(width=0.2,height=0) +
    ggpubr::theme_pubr() +
    xlab('') +
    ylab('S. chamaejsme cover (%)') +
    MetBrewer::scale_color_met_d(name='Degas')


panel_b <- dat |>
  filter(name == 'sedge_cover_percent') |>
  ggplot(aes(x=pika_treatment,y = value, color = factor(block))) +
    geom_jitter(width=0.2,height=0) +
    ggpubr::theme_pubr() +
    xlab('') +
    ylab('Sedge cover (%)') +
    MetBrewer::scale_color_met_d(name='Degas')
  
panel_c <- dat |>
  filter(name == 'grass_cover_percent') |>
  ggplot(aes(x=pika_treatment,y = value, color = factor(block))) +
    geom_jitter(width=0.2,height=0) +
    ggpubr::theme_pubr() +
    xlab('') +
    ylab('Grass cover (%)') +
    MetBrewer::scale_color_met_d(name='Degas')

panel_d <- dat |>
  filter(name == 'forb_cover_percent') |>
  ggplot(aes(x=pika_treatment,y = value, color = factor(block))) +
    geom_jitter(width=0.2,height=0) +
    ggpubr::theme_pubr() +
    xlab('') +
    ylab('Forb cover (%)') +
    MetBrewer::scale_color_met_d(name='Degas')

panel_d <- dat |>
  filter(name == 'species_richness') |>
  ggplot(aes(x=pika_treatment,y = value, color = factor(block))) +
    geom_jitter(width=0.2,height=0) +
    ggpubr::theme_pubr() +
    xlab('') +
    ylab('Species richness') +
    MetBrewer::scale_color_met_d(name='Degas')
  
panel_e <- dat |>
  filter(name == 'active_pika_burrow_100m2') |>
  ggplot(aes(x=pika_treatment,y = value, color = factor(block))) +
    geom_jitter(width=0.2,height=0) +
    ggpubr::theme_pubr() +
    xlab('') +
    ylab('Active pika burrow 100 m^2') +
    MetBrewer::scale_color_met_d(name='Degas')
  
layout <- "
ABC
DEF
"


supp_figure_1 <- panel_a +
  panel_b +
  panel_c +
  panel_d +
  panel_e +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = layout, guides = "collect") & theme(legend.position = 'bottom')


ggsave(supp_figure_1,file=here('output/supp_figures/supp_fig_1.png'),width = 4, height=8)