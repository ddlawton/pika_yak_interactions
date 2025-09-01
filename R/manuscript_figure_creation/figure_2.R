library(tidyverse)
library(ggpubr)
library(patchwork)
library(here)
library(mgcv)
library(knitr)
library(kableExtra)

# --- Set Root Directory for Project ---
here::i_am("README.md")  # Anchor project for reproducible paths

# --- read in data ---

pika_data <- read_csv(here('data/processed/diet_selection/pika_feeding.csv'))
pika_feeding_model_summary <- read_csv(here('data/processed/diet_selection/modeled_data/pika_feeding/model_summary.csv'))
pika_feeding_contrasts <- read_csv(here('data/processed/diet_selection/modeled_data/pika_feeding/posthoc_contrasts.csv'))
pika_feeding_emms <- read_csv(here('data/processed/diet_selection/modeled_data/pika_feeding/emms.csv'))
pika_feeding_letters <- read_csv(here('data/processed/diet_selection/modeled_data/pika_feeding/posthoc_letters.csv'))

yak_data <- read_csv(here('data/processed/diet_selection/yak_grazing.csv'))
yak_feeding_model_summary <- read_csv(here('data/processed/diet_selection/modeled_data/yak_grazing/model_summary.csv'))
yak_feeding_contrasts <- read_csv(here('data/processed/diet_selection/modeled_data/yak_grazing/posthoc_contrasts.csv'))
yak_feeding_emms <- read_csv(here('data/processed/diet_selection/modeled_data/yak_grazing/emms.csv'))
yak_feeding_letters <- read_csv(here('data/processed/diet_selection/modeled_data/yak_grazing/posthoc_letters.csv'))

dung_burrow_by_plot <- read_csv(here('data/processed/diet_selection/dung_burrow_by_plot.csv')) |>
    mutate(plot = factor(plot))

burrow_model <- gam(s_chamaejasme_cover_percent ~
  s(active_pika_burrow_no_100m2, k = 8, bs = 'ts') +
    s(plot, bs = 're'),
  data = dung_burrow_by_plot,
  family = gaussian(),
  select = TRUE
)

gam_ests <- gratia::smooth_estimates(burrow_model)
gam_ests$adj_est <- gam_ests$.estimate + coef(burrow_model)[1]


# --- plot figures ---

pika_feeding_plot <- pika_data |>
  left_join(pika_feeding_emms, by = 'plant_group') |>
  ggplot(aes(x = reorder(plant_group, emmean), y = feeding_clipping_freq)) +
  geom_jitter(width = 0.2, height = 0,color = 'dark grey') +
  geom_point(aes(y = emmean), size = 5, color ='blue') +
  geom_text(data = pika_feeding_letters, aes(label = .group, y = 105)) +
  theme_pubr() +
  ylab('feeding/clipping frequency (%)') +
  xlab('')

yak_grazing_plot <- yak_data |>
  left_join(yak_feeding_emms, by = 'plant_group') |>
  mutate(plant_group = factor(plant_group, levels = c('S. chamaejasme', 'Forbs', 'Sedges', 'Grasses'))) |>
  ggplot(aes(x = plant_group, y = grazing_freq)) +
  geom_jitter(width = 0.2, height = 0,color='dark grey') +
  geom_point(aes(y = emmean), size = 5,color='blue') +
  geom_text(data = yak_feeding_letters, aes(label = .group, y = 105)) +
  theme_pubr() +
  ylab('grazing frequency (%)') +
  xlab('') +
  scale_x_discrete(breaks = c('S. chamaejasme', 'Forbs', 'Sedges', 'Grasses'), drop = FALSE)

pika_burrow <- dung_burrow_by_plot |>
  ggplot(aes(x = active_pika_burrow_no_100m2, y = s_chamaejasme_cover_percent)) +
  geom_point(color='dark grey')  +
    geom_ribbon(data = gam_ests, 
                aes(x = active_pika_burrow_no_100m2, 
                    ymin = adj_est - .se, 
                    ymax = adj_est + .se),
                alpha = .2, inherit.aes = FALSE) +
    geom_line(data = gam_ests, 
              aes(x = active_pika_burrow_no_100m2, y = adj_est), color = 'blue', linewidth = 1.25) +
  #geom_line(data = gam_ests, aes(y = adj_est), linewidth = 1.25, color = 'blue') +
  #geom_ribbon(data = gam_ests, aes(y = adj_est), linewidth = 1.25, color = 'blue') +
  theme_pubr() +
  xlab('active pika burrow density (no/100m^2)') +
  ylab('S. chamaejasme cover (%)')

dung_plot <- dung_burrow_by_plot |>
  ggplot(aes(x = s_chamaejasme_cover_percent, y = yak_dung_no_100m2)) +
  geom_point(color='dark grey') +
  geom_smooth(method = 'lm', se = TRUE, color = 'blue') +
  theme_pubr() +
  ylab('Yak dung density (no/100m^2)') +
  xlab('S. chamaejasme cover (%)') +
  ylim(0, 25) +
  xlim(0, 35)


figure_2_plot <- (pika_feeding_plot + yak_grazing_plot) / (pika_burrow + dung_plot) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(figure_2_plot, file = here('output/figure_2/figure_2.png'),width=10,height=8,dpi=300)
