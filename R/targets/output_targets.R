# =============================================================
# Output Generation Targets
# -------------------------------------------------------------
# Targets for postprocessing, diagnostics, and saving outputs
# Includes model objects (.qs), summaries (CSV), and diagnostics (PNG)
# =============================================================

output_targets <- list(
  # ===========================================================
  # POSTPROCESSED MODEL OUTPUTS
  # ===========================================================
  
  # --- Diet Selection Models ---
  tar_target(
    name = postprocessed_pika_feeding,
    command = postprocess_diet_model(model_pika_feeding, "plant_group")
  ),
  
  tar_target(
    name = postprocessed_yak_grazing,
    command = postprocess_diet_model(model_yak_grazing, "plant_group")
  ),
  
  tar_target(
    name = postprocessed_burrow_cover_gam,
    command = postprocess_gam(model_burrow_cover_gam)
  ),
  
  tar_target(
    name = postprocessed_dung_cover,
    command = postprocess_lm(model_dung_cover)
  ),
  
  # --- Plant Cover Models ---
  tar_target(
    name = postprocessed_plant_cover,
    command = postprocess_treatment_model(model_plant_cover),
    pattern = map(model_plant_cover)
  ),
  
  # --- Forage Quality Models ---
  tar_target(
    name = postprocessed_forage_quality,
    command = postprocess_treatment_model(model_forage_quality),
    pattern = map(model_forage_quality)
  ),
  
  # --- Plant Bites Models ---
  tar_target(
    name = postprocessed_plant_bites,
    command = postprocess_treatment_model(model_plant_bites),
    pattern = map(model_plant_bites)
  ),
  
  # --- Bite-Steps Ratio Models ---
  tar_target(
    name = postprocessed_bite_steps_ratio,
    command = postprocess_treatment_model(model_bite_steps_ratio),
    pattern = map(model_bite_steps_ratio)
  ),
  
  # --- Weight Gain & Steps ---
  tar_target(
    name = postprocessed_weight_gain,
    command = postprocess_treatment_model(model_weight_gain)
  ),
  
  tar_target(
    name = postprocessed_total_steps,
    command = {
      if (!is.null(model_total_steps)) {
        postprocess_treatment_model(model_total_steps)
      } else {
        NULL
      }
    }
  ),
  
  # --- Active Burrows ---
  tar_target(
    name = postprocessed_active_burrows,
    command = {
      if (!is.null(model_active_burrows)) {
        postprocess_treatment_model(model_active_burrows)
      } else {
        NULL
      }
    }
  ),
  
  tar_target(
    name = postprocessed_weight_gain_burrow_gam,
    command = {
      if (!is.null(model_weight_gain_burrow_gam)) {
        postprocess_gam(model_weight_gain_burrow_gam)
      } else {
        NULL
      }
    }
  ),
  
  # ===========================================================
  # SAVE MODEL OBJECTS (.qs format)
  # ===========================================================
  
  tar_target(
    name = saved_models_diet_selection,
    command = {
      files <- c(
        here::here("data/models/pika_feeding.qs"),
        here::here("data/models/yak_grazing.qs"),
        here::here("data/models/burrow_cover_gam.qs"),
        here::here("data/models/dung_cover.qs")
      )
      save_model_object(model_pika_feeding, files[1])
      save_model_object(model_yak_grazing, files[2])
      save_model_object(model_burrow_cover_gam, files[3])
      save_model_object(model_dung_cover, files[4])
      files
    },
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_models_plant_cover,
    command = {
      path <- here::here("data/models", glue::glue("plant_cover_{plant_types_cover}.qs"))
      save_model_object(model_plant_cover, path)
      path
    },
    pattern = map(model_plant_cover, plant_types_cover),
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_models_forage_quality,
    command = {
      path <- here::here("data/models", glue::glue("forage_quality_{quality_metrics}.qs"))
      save_model_object(model_forage_quality, path)
      path
    },
    pattern = map(model_forage_quality, quality_metrics),
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_models_plant_bites,
    command = {
      path <- here::here("data/models", glue::glue("plant_bites_{plant_types_bites}.qs"))
      save_model_object(model_plant_bites, path)
      path
    },
    pattern = map(model_plant_bites, plant_types_bites),
    format = "file",
    repository = "local"
  ),

  tar_target(
    name = saved_models_bite_steps_ratio,
    command = {
      path <- here::here("data/models", glue::glue("bite_steps_ratio_{plant_types_ratio}.qs"))
      save_model_object(model_bite_steps_ratio, path)
      path
    },
    pattern = map(model_bite_steps_ratio, plant_types_ratio),
    format = "file",
    repository = "local"
  ),

  tar_target(
    name = saved_models_foraging_efficiency,
    command = {
      # Weight gain and steps
      save_model_object(model_weight_gain, here::here("data/models/weight_gain.qs"))
      if (!is.null(model_total_steps)) {
        save_model_object(model_total_steps, here::here("data/models/total_steps.qs"))
      }
      here::here("data/models")
    },
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_models_burrows,
    command = {
      if (!is.null(model_active_burrows)) {
        save_model_object(model_active_burrows, here::here("data/models/active_burrows.qs"))
      }
      if (!is.null(model_weight_gain_burrow_gam)) {
        save_model_object(model_weight_gain_burrow_gam, here::here("data/models/weight_gain_burrow_gam.qs"))
      }
      here::here("data/models")
    },
    format = "file",
    repository = "local"
  ),
  
  # ===========================================================
  # SAVE CSV SUMMARIES
  # ===========================================================
  
  tar_target(
    name = saved_summaries_diet_selection,
    command = {
      save_model_outputs(postprocessed_pika_feeding, here::here("data/clean/diet_selection/pika_feeding"))
      save_model_outputs(postprocessed_yak_grazing, here::here("data/clean/diet_selection/yak_grazing"))
      save_model_outputs(postprocessed_burrow_cover_gam, here::here("data/clean/diet_selection/burrow_cover_gam"))
      save_model_outputs(postprocessed_dung_cover, here::here("data/clean/diet_selection/dung_cover"))
      # Return the actual files created
      list.files(here::here("data/clean/diet_selection"), 
                 pattern = "\\.csv$", 
                 recursive = TRUE, 
                 full.names = TRUE)
    },
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_summaries_plant_cover,
    command = {
      dir <- here::here("data/clean/plant_cover", plant_types_cover)
      save_model_outputs(postprocessed_plant_cover, dir)
      dir
    },
    pattern = map(postprocessed_plant_cover, plant_types_cover),
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_summaries_forage_quality,
    command = {
      dir <- here::here("data/clean/forage_quality", quality_metrics)
      save_model_outputs(postprocessed_forage_quality, dir)
      dir
    },
    pattern = map(postprocessed_forage_quality, quality_metrics),
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_summaries_plant_bites,
    command = {
      dir <- here::here("data/clean/foraging_efficiency/plant_bites", plant_types_bites)
      save_model_outputs(postprocessed_plant_bites, dir)
      dir
    },
    pattern = map(postprocessed_plant_bites, plant_types_bites),
    format = "file",
    repository = "local"
  ),

  tar_target(
    name = saved_summaries_bite_steps_ratio,
    command = {
      dir <- here::here("data/clean/foraging_efficiency/bite_steps_ratio", plant_types_ratio)
      save_model_outputs(postprocessed_bite_steps_ratio, dir)
      dir
    },
    pattern = map(postprocessed_bite_steps_ratio, plant_types_ratio),
    format = "file",
    repository = "local"
  ),

  tar_target(
    name = saved_summaries_foraging_efficiency,
    command = {
      # Weight gain and steps
      save_model_outputs(postprocessed_weight_gain, here::here("data/clean/foraging_efficiency/weight_gain"))
      if (!is.null(postprocessed_total_steps)) {
        save_model_outputs(postprocessed_total_steps, here::here("data/clean/foraging_efficiency/total_steps"))
      }
      here::here("data/clean/foraging_efficiency")
    },
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = saved_summaries_burrows,
    command = {
      files <- character(0)
      
      if (!is.null(postprocessed_active_burrows)) {
        save_model_outputs(postprocessed_active_burrows, here::here("data/clean/additional_data/active_burrow_count"))
        new_files <- list.files(here::here("data/clean/additional_data/active_burrow_count"), 
                               pattern = "\\.csv$", full.names = TRUE)
        # Ensure new_files is a plain character vector
        new_files <- as.character(new_files)
        files <- c(files, new_files)
      }
      
      if (!is.null(postprocessed_weight_gain_burrow_gam)) {
        save_model_outputs(postprocessed_weight_gain_burrow_gam, here::here("data/clean/additional_data/weight_gain"))
        new_files <- list.files(here::here("data/clean/additional_data/weight_gain"), 
                               pattern = "\\.csv$", full.names = TRUE)
        new_files <- as.character(new_files)
        files <- c(files, new_files)
        
        # Also save the merged weight-burrow data for plotting
        if (!is.null(active_burrows) && !is.null(yak_weight_gain)) {
          burrow_agg <- active_burrows |>
            dplyr::group_by(block, year, month, pika_treatment, posion_plant_treatment) |>
            dplyr::summarise(active_burrows = mean(active_burrows, na.rm = TRUE), .groups = "drop") |>
            as.data.frame()
          
          burrow_agg$block <- as.character(burrow_agg$block)
          burrow_agg$year <- as.character(burrow_agg$year)
          burrow_agg$month <- as.character(burrow_agg$month)
          burrow_agg$pika_treatment <- as.character(burrow_agg$pika_treatment)
          burrow_agg$posion_plant_treatment <- as.character(burrow_agg$posion_plant_treatment)
          
          weight_agg <- yak_weight_gain |>
            dplyr::group_by(block, year, month, pika_treatment, posion_plant_treatment) |>
            dplyr::summarise(weight_gain_mean = mean(weight_gain, na.rm = TRUE), .groups = "drop") |>
            as.data.frame()

          weight_agg$block <- as.character(weight_agg$block)
          weight_agg$year <- as.character(weight_agg$year)
          weight_agg$month <- as.character(weight_agg$month)
          weight_agg$pika_treatment <- as.character(weight_agg$pika_treatment)
          weight_agg$posion_plant_treatment <- as.character(weight_agg$posion_plant_treatment)

          burrow_weight_raw <- merge(
            burrow_agg,
            weight_agg,
            by = c("block", "year", "month", "pika_treatment", "posion_plant_treatment"),
            all.x = TRUE
          )

          # Filter to match the GAM model: pika + S. chamaejasme treatment only
          # This is the biologically relevant comparison for the burrow-weight relationship
          burrow_weight_raw <- burrow_weight_raw |>
            dplyr::filter(
              active_burrows > 0,
              pika_treatment == "pika",
              posion_plant_treatment == "S. chamaejasme"
            )

          raw_file <- here::here("data/clean/additional_data/weight_gain_raw.csv")
          readr::write_csv(burrow_weight_raw, raw_file)
          files <- c(files, as.character(raw_file))

          # Generate predictions for the GAM model only if data exist
          if (!is.null(model_weight_gain_burrow_gam) && nrow(burrow_weight_raw) > 0) {
            pred_data <- tibble::tibble(
              active_burrows = seq(min(burrow_weight_raw$active_burrows, na.rm = TRUE),
                                  max(burrow_weight_raw$active_burrows, na.rm = TRUE),
                                  length.out = 100),
              # Add random effects as factors for prediction
              block = factor(burrow_weight_raw$block[1]),
              year = factor(burrow_weight_raw$year[1]),
              month = factor(burrow_weight_raw$month[1], levels = c("June", "July", "August"))
            )

            # Predict - ensure result is a vector, not a matrix
            pred_result <- predict(model_weight_gain_burrow_gam, newdata = pred_data, type = "response")
            pred_data$weight_gain_mean <- as.numeric(pred_result)

            # Convert to character for CSV
            pred_data <- pred_data |>
              dplyr::mutate(
                block = as.character(block),
                year = as.character(year),
                month = as.character(month)
              )

            pred_file <- here::here("data/clean/additional_data/weight_gain_modeled_dat.csv")
            readr::write_csv(pred_data, pred_file)
            files <- c(files, as.character(pred_file))
          }
        }
      }
      
      # Return files or NULL if no files created
      if (length(files) > 0) {
        # Ensure we return a plain unnamed character vector
        unname(as.character(files))
      } else {
        NULL
      }
    },
    format = "file",
    repository = "local"
  ),
  
  # ===========================================================
  # DIAGNOSTIC PLOTS
  # ===========================================================
  
  tar_target(
    name = diagnostics_diet_selection,
    command = {
      create_diagnostic_plot(model_pika_feeding, "pika_feeding", 
                            here::here("output/diagnostics/diet_selection/pika_feeding.png"))
      create_diagnostic_plot(model_yak_grazing, "yak_grazing",
                            here::here("output/diagnostics/diet_selection/yak_grazing.png"))
      here::here("output/diagnostics/diet_selection")
    },
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = diagnostics_plant_cover,
    command = {
      path <- here::here("output/diagnostics/plant_cover", glue::glue("{plant_types_cover}_diagnostics.png"))
      create_diagnostic_plot(model_plant_cover, plant_types_cover, path)
      path
    },
    pattern = map(model_plant_cover, plant_types_cover),
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = diagnostics_forage_quality,
    command = {
      path <- here::here("output/diagnostics/forage_quality", glue::glue("{quality_metrics}_diagnostics.png"))
      create_diagnostic_plot(model_forage_quality, quality_metrics, path)
      path
    },
    pattern = map(model_forage_quality, quality_metrics),
    format = "file",
    repository = "local"
  ),
  
  tar_target(
    name = diagnostics_foraging_efficiency,
    command = {
      # Plant bites
      path1 <- here::here("output/diagnostics/foraging_efficiency/plant_bites", glue::glue("{plant_types_bites}_diagnostics.png"))
      create_diagnostic_plot(model_plant_bites, plant_types_bites, path1)

      # Bite-steps ratio
      path2 <- here::here("output/diagnostics/foraging_efficiency/bite_steps_ratio", glue::glue("{plant_types_ratio}_diagnostics.png"))
      create_diagnostic_plot(model_bite_steps_ratio, plant_types_ratio, path2)

      # Weight gain and steps
      create_diagnostic_plot(model_weight_gain, "weight_gain",
                            here::here("output/diagnostics/foraging_efficiency/weight_gain.png"))
      if (!is.null(model_total_steps)) {
        create_diagnostic_plot(model_total_steps, "total_steps",
                              here::here("output/diagnostics/foraging_efficiency/total_steps.png"))
      }

      c(path1, path2, here::here("output/diagnostics/foraging_efficiency/weight_gain.png"))
    },
    pattern = map(model_plant_bites, plant_types_bites, model_bite_steps_ratio, plant_types_ratio),
    format = "file",
    repository = "local"
  ),
  
  # ===========================================================
  # SAVE CLEANED DATA AS CSV
  # (For Quarto documents to read)
  # ===========================================================
  
  tar_target(
    name = saved_clean_data,
    command = {
      # Create directories if they don't exist
      dir.create(here::here("data/clean/diet_selection"), recursive = TRUE, showWarnings = FALSE)
      dir.create(here::here("data/clean/foraging_efficiency"), recursive = TRUE, showWarnings = FALSE)
      dir.create(here::here("data/clean/plant_cover"), recursive = TRUE, showWarnings = FALSE)
      dir.create(here::here("data/clean/forage_quality"), recursive = TRUE, showWarnings = FALSE)
      dir.create(here::here("data/clean/additional_data"), recursive = TRUE, showWarnings = FALSE)
      
      # Diet selection data
      readr::write_csv(pika_feeding, here::here("data/clean/diet_selection/pika_feeding.csv"))
      readr::write_csv(yak_grazing, here::here("data/clean/diet_selection/yak_grazing.csv"))
      readr::write_csv(dung_burrow_by_plot, here::here("data/clean/diet_selection/dung_burrow_by_plot.csv"))
      
      # Experiment data
      readr::write_csv(yak_weight_gain, here::here("data/clean/foraging_efficiency/yak_weight_gain.csv"))
      readr::write_csv(plant_cover_by_treatment, here::here("data/clean/plant_cover/plant_cover_by_treatment.csv"))
      readr::write_csv(plant_forage_quality, here::here("data/clean/forage_quality/plant_forage_quality.csv"))
      readr::write_csv(yak_plant_bites, here::here("data/clean/foraging_efficiency/yak_plant_bites.csv"))
      readr::write_csv(yak_bite_steps_ratio, here::here("data/clean/foraging_efficiency/yak_bite_steps_ratio.csv"))
      
      # Active burrows data (if exists)
      if (!is.null(active_burrows) && nrow(active_burrows) > 0) {
        readr::write_csv(active_burrows, here::here("data/clean/additional_data/active_burrow_dat.csv"))
      }
      
      # Return list of created files
      files <- c(
        here::here("data/clean/diet_selection/pika_feeding.csv"),
        here::here("data/clean/diet_selection/yak_grazing.csv"),
        here::here("data/clean/diet_selection/dung_burrow_by_plot.csv"),
        here::here("data/clean/foraging_efficiency/yak_weight_gain.csv"),
        here::here("data/clean/plant_cover/plant_cover_by_treatment.csv"),
        here::here("data/clean/forage_quality/plant_forage_quality.csv"),
        here::here("data/clean/foraging_efficiency/yak_plant_bites.csv"),
        here::here("data/clean/foraging_efficiency/yak_bite_steps_ratio.csv")
      )
      
      if (!is.null(active_burrows) && nrow(active_burrows) > 0) {
        files <- c(files, here::here("data/clean/additional_data/active_burrow_dat.csv"))
      }
      
      files
    },
    format = "file",
    repository = "local"
  )
)
