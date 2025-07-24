# figure two data mangement, modeling, and visualizing

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcomp)
library(DHARMa)
library(ggpubr)
library(here)

here::i_am('README.md')


# read in the data
fig2a_dat <- read_csv(here('data/processed/fig_2a_feeding_clipping_freq.csv'), show_col_types = FALSE)
fig2b_dat <- read_csv(here('data/processed/fig_2b_grazing_freq.csv'), show_col_types = FALSE) 
fig2c_d_dat <- read_csv(here('data/processed/fig_2c_d_dung_density.csv'), show_col_types = FALSE)



# Figure 2A

fig2a_dat |>
  mutate(month = factor(month,levels = c('June','July','August'))) |>
  ggplot(aes(x=month,y=feeding_clipping_freq)) +
    geom_jitter(width=0.2,height=0) +
    facet_wrap(~plant_group) +
    xlab('') +
    ylab('Feeding/Clipping Frequency (%)') +
    theme_pubr() 


fig2a_mod1 <- glmmTMB(
  feeding_clipping_freq ~ plant_group,
  data = fig2a_dat,
  family = gaussian()
)

fig2a_mod2 <- glmmTMB(
  feeding_clipping_freq ~ plant_group + (1|month),
  data = fig2a_dat,
  family = gaussian()
)

fig2a_mod3 <- glmmTMB(
  feeding_clipping_freq ~ plant_group + (1|month) + (1|month:plant_group),
  data = fig2a_dat,
  family = gaussian()
)

fig2a_mod4 <- glmmTMB(
  feeding_clipping_freq ~ plant_group + (1|month:plant_group),
  data = fig2a_dat,
  family = gaussian()
)

AIC(fig2a_mod1,fig2a_mod2,fig2a_mod3,fig2a_mod4)
BIC(fig2a_mod1,fig2a_mod2,fig2a_mod3,fig2a_mod4)

summary(fig2a_mod1)
summary(fig2a_mod2)
summary(fig2a_mod3)
summary(fig2a_mod4)

fig2a_mod4_resid <- simulateResiduals(fig2a_mod4,n=500)
plot(fig2a_mod4_resid)

fig2a_emms <- emmeans(fig2a_mod4,pairwise ~plant_group)$emmeans |>
  as_tibble()

fig2a_emms_contrasts <- emmeans(fig2a_mod4,pairwise ~plant_group)$contrasts |>
  as_tibble()

fig2a_emms_letters <- cld(em_out$emmeans, Letters = letters, adjust = "tukey") |>
  as_tibble() |>
  mutate(.group = str_trim(.group))  # Clean extra whitespace


fig2a_dat |>
  left_join(fig2a_emms,by='plant_group') |>
  ggplot(aes(x = reorder(plant_group,-emmean),y = feeding_clipping_freq)) +
    geom_jitter(size=1,height=0,width=0.2,alpha=0.6) +
    geom_point(aes(y=emmean),size=3,color='blue') +
    #geom_errorbar(aes(ymin = lower.CL,ymax = upper.CL),width=0) +
    ylim(0,100) +
    xlab('') +
    ylab('Feeding/Clipping Frequency (%)') +
    theme_pubr()


# Figure 2B


fig2b_dat |>
  mutate(month = factor(month,levels = c('June','July','August'))) |>
  ggplot(aes(x=month,y=grazing_freq)) +
    geom_jitter(width=0.2,height=0) +
    facet_wrap(~plant_group,ncol=2) +
    xlab('') +
    ylab('Feeding/Clipping Frequency (%)') +
    theme_pubr() 


fig2b_mod1 <- glmmTMB(
  grazing_freq ~ plant_group,
  data = fig2b_dat,
  family = gaussian()
)

fig2b_mod2 <- glmmTMB(
  grazing_freq ~ plant_group + (1|month),
  data = fig2b_dat,
  family = gaussian()
)

fig2b_mod3 <- glmmTMB(
  grazing_freq ~ plant_group + (1|month) + (1|month:plant_group),
  data = fig2b_dat,
  family = gaussian()
)

fig2b_mod4 <- glmmTMB(
  grazing_freq ~ plant_group + (1|month:plant_group),
  data = fig2b_dat,
  family = gaussian()
)

AIC(fig2b_mod1,fig2b_mod2,fig2b_mod3,fig2b_mod4)
BIC(fig2b_mod1,fig2b_mod2,fig2b_mod3,fig2b_mod4)

summary(fig2b_mod1)
summary(fig2b_mod2)
summary(fig2b_mod3)
summary(fig2b_mod4)

resid <- simulateResiduals(mod1,n=500)
plot(resid)


fig2b_emms <- emmeans(fig2b_mod1,pairwise ~plant_group)$emmeans |>
  as_tibble()

fig2b_emms_contrasts <- emmeans(fig2b_mod1,pairwise ~plant_group)$contrasts |>
  as_tibble()

fig2b_dat |>
  left_join(fig2b_emms,by='plant_group') |>
  ggplot(aes(x = reorder(plant_group,-emmean),y = grazing_freq)) +
    geom_jitter(size=1,height=0,width=0.2,alpha=0.6) +
    geom_point(aes(y=emmean),size=3,color='blue') +
    #geom_errorbar(aes(ymin = lower.CL,ymax = upper.CL),width=0) +
    ylim(0,100) +
    xlab('') +
    ylab('grazing frequency (%)') +
    theme_pubr()




