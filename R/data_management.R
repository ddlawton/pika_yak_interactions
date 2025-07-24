#####
# Pika facilitates yaks by suppressing poisonous plants in the Tibetan Plateau
# data management script
#
# the goal of this script is to take the original raw excel file and transform it 
# into useable csvs for analysis
#####

# Load required libraries
library(tidyverse)
library(janitor)
library(readxl)
library(here)

# Set project root
here::i_am("README.md")

# File path
raw_file <- here("data/raw/Raw data-pika-yak interaction - 副本.xls")

# ---- Utility Functions ----

# Extract month from column name suffix
extract_month <- function(name, june_pattern, july_pattern, aug_pattern) {
  case_when(
    str_detect(name, june_pattern) ~ "June",
    str_detect(name, july_pattern) ~ "July",
    str_detect(name, aug_pattern) ~ "August",
    TRUE ~ NA_character_
  )
}

# ---- Figure 2 ----

fig_2a <- read_excel(raw_file, skip = 1, sheet = "Fig. 2") |>
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

fig_2b <- read_excel(raw_file, skip = 1, sheet = "Fig. 2") |>
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
  mutate(grazing_freq = replace_na(grazing_freq,0))

fig_2c_d <- read_excel(raw_file, skip = 1, sheet = "Fig. 2") |>
  select(Plot...18:`Yak  dung(no./100m2)`) |>
  clean_names() |>
  rename(plot = plot_18)

# ---- Figure 3A ----

fig3a_dat <- read_excel(raw_file, skip = 2, sheet = "Fig. 3a") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = yak_1_6:yak_2_11, names_to = "yak", values_to = "weight_gain") |>
  mutate(month = case_when(
    yak %in% c("yak_1_6", "yak_2_7") ~ "June",
    yak %in% c("yak_1_8", "yak_2_9") ~ "July",
    yak %in% c("yak_1_10", "yak_2_11") ~ "August"
  ))

# ---- Figure 3B, 3C, and S1 ----

fig3bc_s1_dat <- read_excel(raw_file, skip = 3, sheet = "Fig. 3b,c and Fig. S1") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = s_chamaejasme_percent_5:forbs_percent_16,
               names_to = "plant", values_to = "cover") |>
  mutate(
    month = extract_month(plant, "_5$|_6$|_7$|_8$", "_9$|_10$|_11$|_12$", "_13$|_14$|_15$|_16$"),
    plant = gsub("_percent_[0-9]+$", "", plant)
  )

# ---- Figure 3D and S2 ----

fig3d_s2 <- read_excel(raw_file, skip = 2, sheet = "Fig. 3d and Fig. S2") |>
  remove_empty() |>
  clean_names() |>
  pivot_longer(cols = cp_percent_6:ee_percent_17,
               names_to = "plant", values_to = "forage_quality") |>
  mutate(
    month = extract_month(plant, "_6$|_7$|_8$|_9$", "_10$|_11$|_12$|_13$", "_14$|_15$|_16$|_17$"),
    plant = gsub("_percent_[0-9]+$", "", plant)
  )

# ---- Figure 4A-C and S3A ----

fig4ac_s3a <- read_excel(raw_file, skip = 3, sheet = "Fig.4a-c and Fig. S3a") |>
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
      num %in% 22:29 ~ "August",
      TRUE ~ NA_character_
    )
  ) |>
  select(-num)

# ---- Figure 4D-F and S3B ----

fig_4d <- read_excel(raw_file, skip = 2, sheet = "Fig.4d-f and Fig.S3b") |>
  remove_empty() |>
  clean_names() |>
  select(block:yak_2_11) |>
  pivot_longer(cols = yak_1_6:yak_2_11, names_to = "yak", values_to = "total_steps") |>
  mutate(
    num = as.numeric(str_extract(yak, "\\d+$")),
    month = case_when(
      num %in% 6:7 ~ "June",
      num %in% 8:9 ~ "July",
      num %in% 10:11 ~ "August",
      TRUE ~ NA_character_
    )
  ) |>
  select(-num)

fig_4ef_s3b <- read_excel(raw_file, skip = 2, sheet = "Fig.4d-f and Fig.S3b") |>
  remove_empty() |>
  clean_names() |>
  select(block:year, sedges_13:forbs_30) |>
  pivot_longer(cols = sedges_13:forbs_30,
               names_to = "plant", values_to = "bites_steps_ratio") |>
  mutate(
    num = as.numeric(str_extract(plant, "\\d+$")),
    yak_num = case_when(
      num %in% c(13:15, 19:21, 25:30) ~ "yak_1",
      num %in% c(16:18, 22:24) ~ "yak_2",
      TRUE ~ NA_character_
    ),
    month = case_when(
      num %in% 13:18 ~ "June",
      num %in% 19:24 ~ "July",
      num %in% 25:30 ~ "August",
      TRUE ~ NA_character_
    ),
    plant = gsub("_[0-9]+$", "", plant)
  )

# ---- Initial Plant Condition ----

initial_plant_condition <- read_excel(raw_file, skip = 4, sheet = "initial plant condition") |>
  remove_empty() |>
  clean_names()

# ---- saving out figure data to 'data/processed' ----

write_clean_csv <- function(df, name) {
  write_csv(df, here("data/processed", paste0(name, ".csv")))
}

write_clean_csv(fig_2a, "fig_2a_feeding_clipping_freq")
write_clean_csv(fig_2b, "fig_2b_grazing_freq")
write_clean_csv(fig_2c_d, "fig_2c_d_dung_density")

write_clean_csv(fig3a_dat, "fig_3a_weight_gain")
write_clean_csv(fig3bc_s1_dat, "fig_3bc_s1_plant_cover")
write_clean_csv(fig3d_s2, "fig_3d_s2_forage_quality")

write_clean_csv(fig4ac_s3a, "fig_4ac_s3a_forage_efficiency")
write_clean_csv(fig_4d, "fig_4d_total_steps")
write_clean_csv(fig_4ef_s3b, "fig_4ef_s3b_bites_steps_ratio")

write_clean_csv(initial_plant_condition, "initial_plant_condition")
