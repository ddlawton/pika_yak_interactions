# =============================================================
# Data Preprocessing Targets
# -------------------------------------------------------------
# Targets for reading and cleaning all raw data
# =============================================================

data_targets <- list(
  # ===========================================================
  # DIET SELECTION DATA (Figure 2)
  # ===========================================================
  tar_target(
    name = pika_feeding,
    command = read_pika_feeding(raw_data_main)
  ),
  
  tar_target(
    name = yak_grazing,
    command = read_yak_grazing(raw_data_main)
  ),
  
  tar_target(
    name = dung_burrow_by_plot,
    command = read_dung_burrow(raw_data_main)
  ),
  
  # ===========================================================
  # EXPERIMENTAL PLOT DATA (Figure 3)
  # ===========================================================
  tar_target(
    name = yak_weight_gain,
    command = read_yak_weight_gain(raw_data_main)
  ),
  
  tar_target(
    name = plant_cover_by_treatment,
    command = read_plant_cover(raw_data_main)
  ),
  
  tar_target(
    name = plant_forage_quality,
    command = read_forage_quality(raw_data_main)
  ),
  
  # ===========================================================
  # FORAGING EFFICIENCY DATA (Figure 4)
  # ===========================================================
  tar_target(
    name = yak_plant_bites,
    command = read_plant_bites(raw_data_main)
  ),
  
  tar_target(
    name = yak_bite_steps_ratio,
    command = read_bite_steps_ratio(raw_data_main)
  ),
  
  tar_target(
    name = yak_total_steps,
    command = read_total_steps(raw_data_main)
  ),
  
  # ===========================================================
  # INITIAL CONDITIONS & BURROW DATA
  # ===========================================================
  tar_target(
    name = initial_plant_condition,
    command = read_initial_plant_condition(raw_data_initial)
  ),
  
  tar_target(
    name = active_burrows,
    command = read_active_burrows(raw_data_burrows)
  ),
  
  # ===========================================================
  # SPLIT DATASETS BY PLANT TYPE/QUALITY METRIC
  # ===========================================================
  tar_target(
    name = plant_cover_split,
    command = split_by_group(plant_cover_by_treatment, "plant")
  ),
  
  tar_target(
    name = forage_quality_split,
    command = split_by_group(plant_forage_quality, "plant")
  ),
  
  tar_target(
    name = plant_bites_split,
    command = split_by_group(yak_plant_bites, "plant")
  ),
  
  tar_target(
    name = bite_steps_ratio_split,
    command = split_by_group(yak_bite_steps_ratio, "plant")
  )
)
