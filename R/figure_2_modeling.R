

# Figure 2 data management, modeling, and visualizing
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: July 24, 2025
# Purpose: Clean, document, and fully reproduce the analyses and
#          visualisations that produce Figure 2 (A–D).
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


figure2a_AIC_table <- AIC(fig2a_mod1,fig2a_mod2,fig2a_mod3,fig2a_mod4) |>
  as_tibble(rownames = 'model') |>
  mutate(deltaAIC = AIC - min(AIC)) |>
  dplyr::select(-df) |>
  arrange(deltaAIC)

figure2a_BIC_table <- BIC(fig2a_mod1,fig2a_mod2,fig2a_mod3,fig2a_mod4) |>
  as_tibble(rownames = 'model') |>
  mutate(deltaBIC = BIC - min(BIC)) |>
    dplyr::select(-df) |>
    arrange(deltaBIC) 

model_names <- c(
  "Fixed effects only",
  "Random intercept for month",
  "Random intercepts: month + month × plant group",
  "Random intercept: month × plant group only"
)

figure2a_model_selection_table <- figure2a_AIC_table |>
    left_join(figure2a_BIC_table,by='model') |>
      mutate(model = model_names)


summary(fig2a_mod4)

fig2a_mod4_resid <- simulateResiduals(fig2a_mod4,n=500)
plot(fig2a_mod4_resid)

# fixed effect estimation

fig2a_emms <- emmeans(fig2a_mod4,pairwise ~plant_group)$emmeans |>
  as_tibble()

fig2a_emms_contrasts <- emmeans(fig2a_mod4,pairwise ~plant_group)$contrasts |>
  as_tibble()


fig2a_emms_letters <- cld(fig2a_emms, Letters = letters, adjust = "tukey") |>
  as_tibble() |>
  mutate(.group = str_trim(.group))

fig2a_plot <- fig2a_dat |>
  left_join(fig2a_emms_letters,by='plant_group') |>
  ggplot(aes(x = reorder(plant_group,emmean),y = feeding_clipping_freq)) +
    geom_jitter(size=1,height=0,width=0.2,alpha=0.6) +
    geom_point(aes(y=emmean),size=3,color='blue') +
    geom_text(aes(y = 100, label = .group), size = 5) +  # shift letters 3 units above points    #geom_errorbar(aes(ymin = lower.CL,ymax = upper.CL),width=0) +
    ylim(0,100) +
    xlab('') +
    ylab('Feeding/Clipping Frequency (%)') +
    theme_pubr(base_size = 10)

# random effect estimation


# 1. Extract random effects
re_tbl <- ranef(fig2a_mod4)$cond[[1]] %>%
  rownames_to_column("month_plant_group")

# 2. Separate the combined name
re_tbl <- re_tbl %>%
  separate(month_plant_group, into = c("month", "plant_group"), sep = ":")

# 3. Get fixed effects
fixefs <- fixef(fig2a_mod4)$cond
ref_level <- fixefs["(Intercept)"]
plant_effects <- fixefs[names(fixefs) != "(Intercept)"]

# 4. Add fixed effect per plant_group
re_tbl <- re_tbl %>%
  mutate(
    fixed_effect = ref_level + coalesce(plant_effects[plant_group], 0),
    conditional_mean = `(Intercept)` + fixed_effect
  )

# now lets plot it along with the raw data
plant_month_conditional_mean <- fig2a_dat |>
  left_join(re_tbl,by=c('month','plant_group')) |>
    mutate(month = factor(month,levels = c('June','July','August'))) |>
    ggplot(aes(x=month,y=feeding_clipping_freq)) +
      geom_jitter(width=0.2,height=0,size=0.5,pch=21) +
      geom_point(aes(y=conditional_mean),color='blue',size=2) +
      facet_wrap(~plant_group) +
      xlab('') +
      ylab('Feeding/Clipping Frequency (%)') +
      theme_pubr() 
  
# saving tables and figures

write_csv(figure2a_model_selection_table,file=here('output/figure_2/tables/figure_2a_model_selection_criteria.csv'))
ggsave(plant_month_conditional_mean,file=here('output/figure_2/plots/figure_2a_plant_month_conditional_mean.png'),width=5,height=5)



# Figure 2A summary
# because Pika reduced their consumption of the posionous plant through the months, the glmm with
# an interactive random effect of plant_type:month was the best selected model.
# in the end, this doesnt change the overall story, but is interesting to show off.




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


figure2b_AIC_table <- AIC(fig2b_mod1,fig2b_mod2,fig2b_mod3,fig2b_mod4) |>
  as_tibble(rownames = 'model') |>
  mutate(deltaAIC = AIC - min(AIC)) |>
  dplyr::select(-df) |>
  arrange(deltaAIC)

figure2b_BIC_table <- BIC(fig2b_mod1,fig2b_mod2,fig2b_mod3,fig2b_mod4) |>
  as_tibble(rownames = 'model') |>
  mutate(deltaBIC = BIC - min(BIC)) |>
    dplyr::select(-df) |>
    arrange(deltaBIC) 

model_names <- c(
  "Fixed effects only",
  "Random intercept for month",
  "Random intercepts: month + month × plant group",
  "Random intercept: month × plant group only"
)

figure2b_model_selection_table <- figure2b_AIC_table |>
    left_join(figure2b_BIC_table,by='model') |>
      mutate(model = model_names)

summary(fig2b_mod4)


resid <- simulateResiduals(fig2b_mod4,n=500)
plot(resid)


# Fixed effects

fig2b_emms <- emmeans(fig2b_mod4,pairwise ~plant_group)

fig2b_emms_contrasts <- emmeans(fig2b_mod4,pairwise ~plant_group)$contrasts |>
  as_tibble()


fig2b_emms_letters <- cld(fig2b_emms, Letters = letters, adjust = "tukey") |>
  as_tibble() |>
  mutate(.group = str_trim(.group))

fig2b_plot <- fig2b_dat |>
  left_join(fig2b_emms_letters,by='plant_group') |>
  ggplot(aes(x = reorder(plant_group,emmean),y = grazing_freq)) +
    geom_jitter(size=1,height=0,width=0.2,alpha=0.6) +
    geom_point(aes(y=emmean),size=3,color='blue') +
    geom_text(aes(y = 100, label = .group), size = 5) +  # shift letters 3 units above points    #geom_errorbar(aes(ymin = lower.CL,ymax = upper.CL),width=0) +
    ylim(0,100) +
    xlab('') +
    ylab('Grazing Frequency (%)') +
    theme_pubr(base_size = 10)


# random effect estimation


# 1. Extract random effects
re_tbl <- ranef(fig2b_mod4)$cond[[1]] %>%
  rownames_to_column("month_plant_group")

# 2. Separate the combined name
re_tbl <- re_tbl %>%
  separate(month_plant_group, into = c("month", "plant_group"), sep = ":")

# 3. Get fixed effects
fixefs <- fixef(fig2b_mod4)$cond
ref_level <- fixefs["(Intercept)"]
plant_effects <- fixefs[names(fixefs) != "(Intercept)"]

# 4. Add fixed effect per plant_group
re_tbl <- re_tbl %>%
  mutate(
    fixed_effect = ref_level + coalesce(plant_effects[plant_group], 0),
    conditional_mean = `(Intercept)` + fixed_effect
  )

# now lets plot it along with the raw data
plant_month_conditional_mean <- fig2b_dat |>
  left_join(re_tbl,by=c('month','plant_group')) |>
    mutate(month = factor(month,levels = c('June','July','August'))) |>
    ggplot(aes(x=month,y=grazing_freq)) +
      geom_jitter(width=0.2,height=0,size=0.5,pch=21) +
      geom_point(aes(y=conditional_mean),color='blue',size=2) +
      facet_wrap(~plant_group) +
      xlab('') +
      ylab('Feeding/Clipping Frequency (%)') +
      theme_pubr() 
  
  
# saving tables and figures

write_csv(figure2b_model_selection_table,file=here('output/figure_2/tables/figure_2b_model_selection_criteria.csv'))
ggsave(plant_month_conditional_mean,file=here('output/figure_2/plots/grazing_freq_plant_month_conditional_mean.png'),width=5,height=5)

# Figure 2B summary
# THere wasnt any change in Yak feeding frequency through the time periods
# this is seen in the raw data distirbution and in the AIC/BIC selection
# However to keep the models the same between 2A and 2B I went with the full structure
# this *really* has no impact on the findings as the random effects have very small effects
# as can be seen in the grazing_freq_plant_month_conditional_mean.png plot


# Figure 2C and 2D
## Figure 2C

fig_2c_2d_dat <- read_csv(here('data/processed/fig_2c_d_dung_density.csv')) |>
    drop_na()

library(mgcv)

gam_model <- gam(s_chamaejasme_cover_percent ~ s(active_pika_burrow_no_100m2,k=8),
  data=fig_2c_2d_dat,
  select=TRUE)

gratia::draw(gam_model,parametric=TRUE)
gratia::appraise(gam_model)

gam_ests <- gratia::smooth_estimates(gam_model)
gam_ests$adj_est <- gam_ests$.estimate + coef(gam_model)[1] #This is how I adjust to TWS units

fig2c_plot <- fig_2c_2d_dat |>
  ggplot(aes(x = active_pika_burrow_no_100m2, y = s_chamaejasme_cover_percent)) +
  geom_point() +
  geom_line(data = gam_ests,aes(y=adj_est),linewidth = 1.25,color='blue') +
  theme_pubr(base_size = 10) +
  xlab('active pike burrow density (no/100m^2)') +
  ylab('S chamaejasme cover (%)') +
  ylim(0,40) +
  xlim(0,10)

# Figure 2C summary
# so I was going to leave it with a linear regression
# but the data looked very non-linear so I decided to fit a gam curve to it

# Figure 2D


fig2d_plot <- fig_2c_2d_dat |>
  ggplot(aes(x = s_chamaejasme_cover_percent,y = yak_dung_no_100m2)) +
    geom_point() +
    geom_smooth(method = 'lm',se=FALSE) +
    theme_pubr(base_size = 10) +
    ylab('Dung density (no/100m^2)') +
    xlab('S. chamaejasme cover (%)') +
    ylim(0,25) +
    xlim(0,35)

# Figure 2D summary
# there is a weird grouping of the data into two clusters
# i am not sure what it is and there are no variables to consider
# so I am going to leave this as a linear regression. 


# Combining the figurte 2 plots together

fig2_plot <- (fig2a_plot + fig2b_plot) / (fig2c_plot + fig2d_plot) + 
  plot_annotation(tag_levels = 'A')

ggsave(fig2_plot, file = here('output/figure_2/figure_2.png'),width=10,height=10)


