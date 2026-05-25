# =============================================================
# Model Fitting Targets
# -------------------------------------------------------------
# Targets for fitting all GLMMs and GAMs
# Uses dynamic branching for plant-type and quality-metric models
# =============================================================

model_targets <- list(
  # ===========================================================
  # DIET SELECTION MODELS (Figure 2)
  # ===========================================================
  tar_target(
    name = model_pika_feeding,
    command = fit_pika_feeding_model(pika_feeding)
  ),
  
  tar_target(
    name = model_yak_grazing,
    command = fit_yak_grazing_model(yak_grazing)
  ),
  
  tar_target(
    name = model_burrow_cover_gam,
    command = fit_burrow_cover_gam(dung_burrow_by_plot)
  ),
  
  tar_target(
    name = model_dung_cover,
    command = fit_dung_cover_model(dung_burrow_by_plot)
  ),
  
  # ===========================================================
  # PLANT COVER MODELS (Figure 3B/C and S1)
  # Dynamic branching over plant types
  # ===========================================================
  tar_target(
    name = plant_types_cover,
    command = names(plant_cover_split)
  ),
  
  tar_target(
    name = model_plant_cover,
    command = fit_plant_cover_model(plant_cover_split[[plant_types_cover]], use_beta = TRUE),
    pattern = map(plant_types_cover)
  ),
  
  # ===========================================================
  # FORAGE QUALITY MODELS (Figure 3D and S2)
  # Dynamic branching over quality metrics (CP, ADF, NDF, EE)
  # ===========================================================
  tar_target(
    name = quality_metrics,
    command = names(forage_quality_split)
  ),
  
  tar_target(
    name = model_forage_quality,
    command = fit_forage_quality_model(forage_quality_split[[quality_metrics]], use_beta = TRUE),
    pattern = map(quality_metrics)
  ),
  
  # ===========================================================
  # PLANT BITES MODELS (Figure 4A-C)
  # Adaptive Gaussian → Tweedie fitting
  # ===========================================================
  tar_target(
    name = plant_types_bites,
    command = names(plant_bites_split)
  ),
  
  tar_target(
    name = model_plant_bites,
    command = fit_plant_bites_model(plant_bites_split[[plant_types_bites]]),
    pattern = map(plant_types_bites)
  ),
  
  # ===========================================================
  # BITE-TO-STEP RATIO MODELS (Figure 4D-F)
  # Beta family models
  # ===========================================================
  tar_target(
    name = plant_types_ratio,
    command = names(bite_steps_ratio_split)
  ),
  
  tar_target(
    name = model_bite_steps_ratio,
    command = fit_bite_steps_model(bite_steps_ratio_split[[plant_types_ratio]], use_beta = TRUE),
    pattern = map(plant_types_ratio)
  ),
  
  # ===========================================================
  # WEIGHT GAIN & TOTAL STEPS MODELS
  # ===========================================================
  tar_target(
    name = model_weight_gain,
    command = fit_weight_gain_model(yak_weight_gain)
  ),
  
  tar_target(
    name = model_total_steps,
    command = {
      if (!is.null(yak_total_steps)) {
        fit_total_steps_model(yak_total_steps)
      } else {
        NULL
      }
    }
  ),
  
  # ===========================================================
  # ACTIVE BURROW MODELS (Figure 5)
  # ===========================================================
  tar_target(
    name = model_active_burrows,
    command = {
      if (!is.null(active_burrows)) {
        fit_active_burrows_model(active_burrows)
      } else {
        NULL
      }
    }
  ),
  
  tar_target(
    name = model_weight_gain_burrow_gam,
    command = {
      if (!is.null(active_burrows) && !is.null(yak_weight_gain)) {
        # Merge data for GAM analysis
        # Aggregate both datasets to treatment level first
        burrow_agg <- active_burrows |>
          dplyr::group_by(block, year, month, pika_treatment, posion_plant_treatment) |>
          dplyr::summarise(active_burrows = mean(active_burrows, na.rm = TRUE), .groups = "drop")
        
        weight_agg <- yak_weight_gain |>
          dplyr::group_by(block, year, month, pika_treatment, posion_plant_treatment) |>
          dplyr::summarise(weight_gain_mean = mean(weight_gain, na.rm = TRUE), .groups = "drop")
        
        # Join the aggregated data
        burrow_weight_data <- dplyr::left_join(
          burrow_agg,
          weight_agg,
          by = c("block", "year", "month", "pika_treatment", "posion_plant_treatment")
        ) |>
          dplyr::filter(!is.na(weight_gain_mean))
        
        fit_weight_gain_burrow_gam(burrow_weight_data)
      } else {
        NULL
      }
    }
  )
)
