#####
# Pika facilitates yaks by suppressing poisonous plants in the Tibetan Plateau
# Data Management Script
#
# This script reads the raw Excel file and transforms each figure panel's data 
# into clean CSV files suitable for downstream analysis.
#
# Author: Douglas Lawton
# Date: August 1st, 2025
#####

# ---- Load Required Libraries ----
library(tidyverse)
library(janitor)
library(readxl)
library(here)

# ---- Set Project Root (used by `here`) ----
here::i_am("README.md")

# ---- Define File Path to Raw Excel Workbook ----
raw_file <- here("data/raw/Raw data-pika-yak interaction - 副本.xls")

# ---- Utility Function to Extract Month from Column Names ----
extract_month <- function(name, june_pattern, july_pattern, aug_pattern) {
  case_when(
    str_detect(name, june_pattern) ~ "June",
    str_detect(name, july_pattern) ~ "July",
    str_detect(name, aug_pattern) ~ "August",
    TRUE ~ NA_character_
  )
}

# ---- Figure 2A: Pika Feeding Frequency by Plant Group ----
fig2a_pika_feeding <- read_excel(raw_file, skip = 1, sheet = "Fig. 2") |>
  select(Plot...2:...6) |>
  pivot_longer(cols = c(`Feeding/clipping frequency (%)`, ...5, ...6),
               names_to = "name", values_to = "feeding_clipping_freq") |>
  mutate(
    month = case_when(
      name == "...5" ~ "July",
      name == "...6" ~ "August",
      name == "Feeding/clipping frequency (%)" ~ "June"
    ),
    animal = "pika"
  ) |>
  select(-name) |>
  rename(plot = Plot...2, plant_group = `Plant groups...3`)

# ---- Figure 2B: Yak Grazing Frequency by Plant Group ----
fig2b_yak_grazing <- read_excel(raw_file, skip = 1, sheet = "Fig. 2") |>
  select(Transect:...14) |>
  pivot_longer(cols = c(`Grazing frequency (%)`, ...13, ...14),
               names_to = "name", values_to = "grazing_freq") |>
  mutate(
    month = case_when(
      name == "...13" ~ "July",
      name == "...14" ~ "August",
      name == "Grazing frequency (%)" ~ "June"
    ),
    animal = "yak"
  ) |>
  select(-name) |>
  rename(transect = Transect, plant_group = `Plant groups...11`) |>
  mutate(grazing_freq = replace_na(grazing_freq, 0))

# ---- Figure 2C/D: Dung and Burrow Density by Plot ----
fig2cd_dung_burrow <- read_excel(raw_file, skip = 1, sheet = "Fig. 2") |>
  select(Plot...18:`Yak  dung(no./100m2)`) |>
  clean_names() |>
  rename(plot = plot_18)

# ---- Figure 3A: Yak Weight Gain ----
fig3a_yak_weight <- read_excel(raw_file, skip = 2, sheet = "Fig. 3a") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = yak_1_6:yak_2_11, names_to = "yak", values_to = "weight_gain") |>
  mutate(month = case_when(
    yak %in% c("yak_1_6", "yak_2_7") ~ "June",
    yak %in% c("yak_1_8", "yak_2_9") ~ "July",
    yak %in% c("yak_1_10", "yak_2_11") ~ "August"
  ))

# ---- Figure 3B/C and S1: Plant Cover (Experimental Plots) ----
fig3bc_s1_plant_cover <- read_excel(raw_file, skip = 3, sheet = "Fig. 3b,c and Fig. S1") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = s_chamaejasme_percent_5:forbs_percent_16,
               names_to = "plant", values_to = "cover") |>
  mutate(
    month = extract_month(plant, "_5$|_6$|_7$|_8$", "_9$|_10$|_11$|_12$", "_13$|_14$|_15$|_16$"),
    plant = gsub("_percent_[0-9]+$", "", plant)
  )

# ---- Figure 3D and S2: Plant Forage Quality ----
fig3d_s2_quality <- read_excel(raw_file, skip = 2, sheet = "Fig. 3d and Fig. S2") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = cp_percent_6:ee_percent_17,
               names_to = "plant", values_to = "forage_quality") |>
  mutate(
    month = extract_month(plant, "_6$|_7$|_8$|_9$", "_10$|_11$|_12$|_13$", "_14$|_15$|_16$|_17$"),
    plant = gsub("_percent_[0-9]+$", "", plant)
  )

# ---- Figure 4A-C and S3A: Forage Efficiency (Plant Bites) ----
fig4ac_s3a_bites <- read_excel(raw_file, skip = 3, sheet = "Fig.4a-c and Fig. S3a") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = sedges_6:total_29,
               names_to = "plant", values_to = "forage_efficiency") |>
  mutate(
    num = as.numeric(str_extract(plant, "\\d+$")),
    yak = case_when(
      num %in% c(6:9, 14:17, 22:29) ~ "yak_1",
      num %in% c(10:13, 18:21) ~ "yak_2",
      TRUE ~ NA_character_
    ),
    month = case_when(
      num %in% 6:13 ~ "June",
      num %in% 14:21 ~ "July",
      num %in% 22:29 ~ "August"
    )
  ) |>
  select(-num)

# ---- Figure 4D: Total Steps by Yak ----
fig4d_steps <- read_excel(raw_file, skip = 2, sheet = "Fig.4d-f and Fig.S3b") |>
  remove_empty() |>
  clean_names() |>
  select(block:yak_2_11) |>
  pivot_longer(cols = yak_1_6:yak_2_11, names_to = "yak", values_to = "total_steps") |>
  mutate(
    num = as.numeric(str_extract(yak, "\\d+$")),
    month = case_when(
      num %in% 6:7 ~ "June",
      num %in% 8:9 ~ "July",
      num %in% 10:11 ~ "August"
    )
  ) |>
  select(-num)

# ---- Figure 4E/F and S3B: Bite-to-Step Ratio ----
fig4ef_s3b_ratio <- read_excel(raw_file, skip = 2, sheet = "Fig.4d-f and Fig.S3b") |>
  remove_empty() |>
  clean_names() |>
  select(block:year, sedges_13:forbs_30) |>
  pivot_longer(cols = sedges_13:forbs_30,
               names_to = "plant", values_to = "bites_steps_ratio") |>
  mutate(
    num = as.numeric(str_extract(plant, "\\d+$")),
    yak_num = case_when(
      num %in% c(13:15, 19:21, 25:30) ~ "yak_1",
      num %in% c(16:18, 22:24) ~ "yak_2"
    ),
    month = case_when(
      num %in% 13:18 ~ "June",
      num %in% 19:24 ~ "July",
      num %in% 25:30 ~ "August"
    ),
    plant = gsub("_[0-9]+$", "", plant)
  )

# ---- Initial Plant Condition ----
initial_plant_condition <- read_excel(raw_file, skip = 4, sheet = "initial plant condition") |>
  remove_empty() |>
  clean_names()


# ---- Active Burrow & Yak Weight Gain Relationship  ----


weight_gain <- read_excel(here('data/raw/burrow data.xlsx'), skip = 2, sheet = "Sheet1") |>
  remove_empty() |>
  clean_names() |>
  select(block,pika_treatment,posion_plant_treatment,year,yak_1_6:yak_2_11) |>
  pivot_longer(cols = starts_with('yak_'),values_to = 'weight_gain') |>
  mutate(
    month = case_when(
      str_detect(name,'_6|_7') ~ 'June',
      str_detect(name,'_8|_9') ~ 'July',
      str_detect(name,'_10|_11') ~ 'August'
    ),
    yak = sub("_[^_]+$", "", name)
  ) |>
  select(!name)

active_burrows <- read_excel(here('data/raw/burrow data.xlsx'), skip = 2, sheet = "Sheet1") |>
  remove_empty() |>
  clean_names() |>
  select(block,pika_treatment,posion_plant_treatment,year,x19,x20,x21) |>
  pivot_longer(cols = starts_with('x'),values_to = 'active_burrows') |>
  mutate(
    month = case_when(
      name == 'x19' ~ 'June',
      name == 'x20' ~ 'July',
      name == 'x21' ~ 'August'
    )
  ) |>
  select(block,pika_treatment,posion_plant_treatment,year,month,active_burrows)



# ---- Utility to Save CSVs ----
write_clean_csv <- function(df, path, name) {
  write_csv(df, here(path, paste0(name, ".csv")))
}

# ---- Save Cleaned Data ----

## Diet Selection
write_clean_csv(fig2a_pika_feeding, "data/processed/diet_selection", "pika_feeding")
write_clean_csv(fig2b_yak_grazing, "data/processed/diet_selection", "yak_grazing")
write_clean_csv(fig2cd_dung_burrow, "data/processed/diet_selection", "dung_burrow_by_plot")

## Experimental Plant Cover
write_clean_csv(fig3bc_s1_plant_cover, "data/processed/experiment_plant_cover", "plant_cover_by_treatment")

## Experimental Foraging Efficiency
write_clean_csv(fig3a_yak_weight, "data/processed/experiment_foraging_efficiency", "yak_weight_gain")
write_clean_csv(fig4ac_s3a_bites, "data/processed/experiment_foraging_efficiency", "yak_plant_bites")
write_clean_csv(fig4d_steps, "data/processed/experiment_foraging_efficiency", "yak_total_steps")
write_clean_csv(fig4ef_s3b_ratio, "data/processed/experiment_foraging_efficiency", "yak_bite_steps_ratio")

## Experimental Foraging Quality
write_clean_csv(fig3d_s2_quality, "data/processed/experiment_foraging_quality", "plant_foraging_quality")

## Initial condition
write_clean_csv(initial_plant_condition, "data/processed/initial_conditions", "initial_plant_condition")

## Active burrows and Yak Gain Weight
write_clean_csv(weight_gain, "data/processed/additional_data", "yak_weight_gain")
write_clean_csv(active_burrows, "data/processed/additional_data", "active_burrows")
