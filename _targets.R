# =============================================================
# Targets Pipeline: Pika-Yak Facilitation Research
# -------------------------------------------------------------
# Author: Douglas Lawton
# Date: May 2026
# Purpose: Reproducible workflow for data preprocessing, modeling,
#          postprocessing, and output generation
# =============================================================

library(targets)
library(tarchetypes)
library(qs2)

# Source all function definitions
source("R/functions/data_preprocessing.R")
source("R/functions/model_fitting.R")
source("R/functions/model_postprocessing.R")
source("R/functions/visualization.R")

# Source modular target definitions
source("R/targets/data_targets.R")
source("R/targets/model_targets.R")
source("R/targets/output_targets.R")

# Set global options
tar_option_set(
  packages = c(
    "tidyverse", "janitor", "readxl", "here",
    "glmmTMB", "mgcv", "emmeans", "multcomp", 
    "broom.mixed", "DHARMa", "ggpubr", "patchwork",
    "gt", "glue", "gratia"
  ),
  format = "qs",  # Use qs2 for efficient serialization (supports model objects)
  error = "continue"
)

# Define the complete pipeline
list(
  # ===========================================================
  # DATA PREPROCESSING TARGETS
  # ===========================================================
  tar_target(
    name = raw_data_main,
    command = here::here("data/raw/Raw data-pika-yak interaction - 副本.xls"),
    format = "file"
  ),
  
  tar_target(
    name = raw_data_initial,
    command = here::here("data/raw/Initial plant and pika condition in May 2022.xlsx"),
    format = "file"
  ),
  
  tar_target(
    name = raw_data_burrows,
    command = here::here("data/raw/burrow data.xlsx"),
    format = "file"
  ),
  
  # Clean data targets (return tibbles, saved as .qs by default)
  data_targets,
  
  # ===========================================================
  # MODELING TARGETS
  # ===========================================================
  model_targets,
  
  # ===========================================================
  # POSTPROCESSING & OUTPUT TARGETS
  # ===========================================================
  output_targets
  
  # ===========================================================
  # NOTE: Quarto reports are rendered separately by GitHub Actions
  # after the pipeline completes. They read from data/clean/ and
  # data/models/ directories created by the pipeline.
  # ===========================================================
)
