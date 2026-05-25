# =============================================================
# Model Postprocessing Functions
# -------------------------------------------------------------
# Functions to extract EMMs, contrasts, diagnostics, and summaries
# =============================================================

# ===========================================================
# EMMEANS AND CONTRASTS
# ===========================================================

#' Extract estimated marginal means from a model
#' @param model Fitted model object (glmmTMB or gam)
#' @param specs Specification for emmeans (e.g., ~ treatment)
#' @param type Response type ("response" or "link")
#' @return Tibble of EMMs
extract_emms <- function(model, specs = ~ pika_treatment * posion_plant_treatment, 
                        type = "response") {
  emms <- emmeans::emmeans(model, specs = specs, type = type)
  tibble::as_tibble(emms)
}

#' Extract pairwise contrasts from a model
#' @param model Fitted model object
#' @param specs Specification for emmeans
#' @param adjust Adjustment method (default "tukey")
#' @param type Response type
#' @return Tibble of contrasts
extract_contrasts <- function(model, specs = ~ pika_treatment * posion_plant_treatment,
                             adjust = "tukey", type = "response") {
  emms <- emmeans::emmeans(model, specs = specs, type = type)
  contrasts <- pairs(emms, adjust = adjust)
  tibble::as_tibble(contrasts)
}

#' Extract compact letter display (grouping letters)
#' @param model Fitted model object
#' @param specs Specification for emmeans
#' @param adjust Adjustment method
#' @param letter_set Letter set to use (default: letters)
#' @param type Response type
#' @return Tibble with .group column containing letters
extract_cld <- function(model, specs = ~ pika_treatment * posion_plant_treatment,
                       adjust = "tukey", letter_set = letters, type = "response") {
  emms <- emmeans::emmeans(model, specs = specs, type = type)
  cld_result <- multcomp::cld(emms, adjust = adjust, Letters = letter_set, type = type)
  tibble::as_tibble(cld_result) |>
    dplyr::mutate(.group = stringr::str_trim(.group))
}

#' Complete postprocessing for a treatment model
#' Returns list with summary, emms, contrasts, and letters
#' @param model Fitted glmmTMB model
#' @return Named list with all postprocessed outputs
postprocess_treatment_model <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  
  # Handle adaptive models (extract model from list)
  if (is.list(model) && "model" %in% names(model)) {
    model <- model$model
  }
  
  if (is.null(model)) {
    return(NULL)
  }
  
  # Get family info using family() function
  fam <- tryCatch(family(model), error = function(e) NULL)
  
  if (!is.null(fam)) {
    family_info <- tibble::tibble(
      family = fam$family,
      link = fam$link
    )
  } else {
    family_info <- tibble::tibble(
      family = "unknown",
      link = "unknown"
    )
  }
  
  list(
    summary = broom.mixed::tidy(model),
    family = family_info,
    emms = extract_emms(model),
    contrasts = extract_contrasts(model),
    letters = extract_cld(model)
  )
}

#' Postprocess diet selection models (different specs)
postprocess_diet_model <- function(model, grouping_var = "plant_group") {
  if (is.null(model)) {
    return(NULL)
  }
  
  specs <- as.formula(paste0("~ ", grouping_var))
  
  # Get family info using family() function
  fam <- tryCatch(family(model), error = function(e) NULL)
  
  if (!is.null(fam)) {
    family_info <- tibble::tibble(
      family = fam$family,
      link = fam$link
    )
  } else {
    family_info <- tibble::tibble(
      family = "unknown",
      link = "unknown"
    )
  }
  
  list(
    summary = broom.mixed::tidy(model),
    family = family_info,
    emms = extract_emms(model, specs = specs),
    contrasts = extract_contrasts(model, specs = specs),
    letters = extract_cld(model, specs = specs)
  )
}

#' Postprocess GAM model
postprocess_gam <- function(gam_model) {
  if (is.null(gam_model)) {
    return(NULL)
  }
  
  list(
    summary = broom::tidy(gam_model),
    # Generate predictions for plotting (returns both marginal and conditional)
    predictions = generate_gam_predictions(gam_model)
  )
}

#' Postprocess linear model (lm)
postprocess_lm <- function(lm_model) {
  if (is.null(lm_model)) {
    return(NULL)
  }
  
  list(
    summary = broom::tidy(lm_model),
    # Generate predictions for plotting
    predictions = generate_lm_predictions(lm_model)
  )
}

#' Generate prediction grid for linear model plotting
generate_lm_predictions <- function(lm_model, n = 100) {
  # Extract model data
  model_data <- lm_model$model
  pred_var <- names(model_data)[2]  # First predictor after response
  
  # Create prediction grid
  pred_range <- range(model_data[[pred_var]], na.rm = TRUE)
  new_data <- tibble::tibble(
    !!pred_var := seq(pred_range[1], pred_range[2], length.out = n)
  )
  
  # Get predictions with SE
  preds <- predict(lm_model, newdata = new_data, se.fit = TRUE, interval = "confidence")
  
  # Handle different return formats
  if (is.list(preds) && "fit" %in% names(preds)) {
    # When se.fit = TRUE, predict returns a list
    if (is.matrix(preds$fit)) {
      # When interval is specified, fit is a matrix with fit, lwr, upr
      new_data$fit <- preds$fit[, "fit"]
      new_data$lower <- preds$fit[, "lwr"]
      new_data$upper <- preds$fit[, "upr"]
    } else {
      new_data$fit <- as.numeric(preds$fit)
      new_data$se <- as.numeric(preds$se.fit)
      new_data$lower <- new_data$fit - 1.96 * new_data$se
      new_data$upper <- new_data$fit + 1.96 * new_data$se
    }
  } else if (is.matrix(preds)) {
    # When interval is specified but se.fit = FALSE
    new_data$fit <- preds[, "fit"]
    new_data$lower <- preds[, "lwr"]
    new_data$upper <- preds[, "upr"]
  } else {
    # Simple numeric vector
    new_data$fit <- as.numeric(preds)
  }
  
  new_data
}

#' Generate prediction grid for GAM plotting
generate_gam_predictions <- function(gam_model, n = 100) {
  # Get the predictor variable name (first smooth term)
  smooth_terms <- mgcv::summary.gam(gam_model)$s.table
  if (nrow(smooth_terms) == 0) {
    return(NULL)
  }
  
  # Extract model data
  model_data <- gam_model$model
  pred_var <- names(model_data)[2]  # First predictor after response
  
  # Create prediction grid
  pred_range <- range(model_data[[pred_var]], na.rm = TRUE)
  new_data <- tibble::tibble(
    !!pred_var := seq(pred_range[1], pred_range[2], length.out = n)
  )
  
  # Add random effects at their first level (for GAMs with random effects)
  # Common random effects: plot, block, year, month
  random_effect_vars <- c("plot", "block", "year", "month")
  for (var in random_effect_vars) {
    if (var %in% names(model_data)) {
      new_data[[var]] <- model_data[[var]][1]
    }
  }
  
  # Conditional predictions (include random-effect offsets set to first observed level)
  preds_cond <- predict(gam_model, newdata = new_data, se.fit = TRUE, type = "response")

  fit_obj <- preds_cond$fit
  se_obj <- preds_cond$se.fit

  if (is.matrix(fit_obj)) {
    fit_df <- as.data.frame(fit_obj, stringsAsFactors = FALSE)
    names(fit_df) <- paste0("fit_", seq_len(ncol(fit_df)))
  } else {
    fit_df <- tibble::tibble(fit = as.numeric(fit_obj))
  }

  if (is.matrix(se_obj)) {
    se_df <- as.data.frame(se_obj, stringsAsFactors = FALSE)
    names(se_df) <- paste0("se_", seq_len(ncol(se_df)))
  } else {
    se_df <- tibble::tibble(se = as.numeric(se_obj))
  }

  conditional_out <- dplyr::bind_cols(new_data, fit_df, se_df)
  if ("fit" %in% names(conditional_out) && "se" %in% names(conditional_out)) {
    conditional_out$lower <- conditional_out$fit - 1.96 * conditional_out$se
    conditional_out$upper <- conditional_out$fit + 1.96 * conditional_out$se
  }

  # Marginal predictions matching the script: use gratia::smooth_estimates
  # which returns the smooth partial effect (no random effects). Add model
  # intercept to obtain the population-level response curve as in the script.
  marginal_out <- tryCatch({
    ests <- gratia::smooth_estimates(gam_model)
    intercept <- tryCatch(coef(gam_model)[1], error = function(e) 0)
    # If ests has a column with predictor name (e.g., 'active_burrows'), keep it.
    # gratia usually returns the smooth x values in a column named after the predictor.
    if (".estimate" %in% names(ests)) {
      ests <- ests |>
        dplyr::mutate(
          adj_est = .estimate + intercept,
          lower = adj_est - 1.96 * .se,
          upper = adj_est + 1.96 * .se
        )
    }
    ests
  }, error = function(e) {
    NULL
  })

  list(
    conditional = conditional_out,
    marginal = marginal_out
  )
}

# ===========================================================
# DIAGNOSTICS
# ===========================================================

#' Create DHARMa diagnostic plot for a model
#' @param model Fitted model object
#' @param model_name Name for the plot title
#' @param output_path Path to save PNG
#' @param n_sim Number of simulations for DHARMa
#' @return Logical indicating success
create_diagnostic_plot <- function(model, model_name, output_path, n_sim = 1000) {
  # Handle adaptive models
  if (is.list(model) && "model" %in% names(model)) {
    model <- model$model
  }
  
  if (is.null(model)) {
    message(glue::glue("[{model_name}] Skipped diagnostics (model is NULL)"))
    return(FALSE)
  }
  
  # Create directory if needed
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  tryCatch({
    png(
      filename = output_path,
      width = 1200, height = 600, res = 150
    )
    
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    
    resid <- DHARMa::simulateResiduals(model, n = n_sim)
    DHARMa::plotQQunif(resid)
    DHARMa::plotResiduals(resid)
    
    mtext(glue::glue("DHARMa Diagnostics – {model_name}"), outer = TRUE, cex = 1.2)
    
    dev.off()
    
    return(TRUE)
  }, error = function(e) {
    message(glue::glue("[{model_name}] Diagnostic plot failed: {e$message}"))
    if (dev.cur() > 1) dev.off()  # Close device if open
    return(FALSE)
  })
}

#' Create diagnostic plots for a list of models
#' @param model_list Named list of models
#' @param output_dir Directory to save plots
#' @param suffix Optional suffix for filenames
create_diagnostic_plots_batch <- function(model_list, output_dir, suffix = "") {
  purrr::iwalk(model_list, function(model, name) {
    filename <- if (suffix != "") {
      glue::glue("{name}_{suffix}_diagnostics.png")
    } else {
      glue::glue("{name}_diagnostics.png")
    }
    
    output_path <- file.path(output_dir, filename)
    create_diagnostic_plot(model, name, output_path)
  })
}

# ===========================================================
# SAVING FUNCTIONS
# ===========================================================

#' Save model postprocessing outputs to CSV files
#' @param postprocessed_list List from postprocess_*_model()
#' @param output_dir Directory to save CSVs
save_model_outputs <- function(postprocessed_list, output_dir) {
  if (is.null(postprocessed_list)) {
    message(glue::glue("Skipping save for {output_dir} (NULL input)"))
    return(FALSE)
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Helper to remove list columns before saving
  flatten_for_csv <- function(df) {
    if (is.null(df)) return(NULL)
    # Remove any list or matrix columns
    # If not a data.frame/tibble try to coerce
    if (!is.data.frame(df)) {
      df <- tryCatch(tibble::as_tibble(df), error = function(e) NULL)
      if (is.null(df)) return(NULL)
    }

    # For matrix columns, expand into separate columns; for list columns,
    # convert elements to strings so CSV can store them.
    for (col in names(df)) {
      colval <- df[[col]]
      if (is.matrix(colval)) {
        matdf <- as.data.frame(colval, stringsAsFactors = FALSE)
        names(matdf) <- paste0(col, "_", seq_len(ncol(matdf)))
        df[[col]] <- NULL
        df <- dplyr::bind_cols(df, matdf)
      } else if (is.list(colval)) {
        df[[col]] <- vapply(colval, function(x) {
          if (is.null(x)) return(NA_character_)
          if (is.atomic(x) && length(x) == 1) return(as.character(x))
          if (is.atomic(x)) return(paste(as.character(x), collapse = ";"))
          # Fallback: try to coerce other objects to character
          tryCatch(as.character(x), error = function(e) "<complex_value>")
        }, FUN.VALUE = "")
      }
    }

    # Final pass: coerce any remaining non-atomic or recursive columns to character
    non_atomic <- names(df)[vapply(df, function(x) !is.atomic(x) || is.recursive(x), logical(1))]
    if (length(non_atomic) > 0) {
      message(glue::glue("Coercing non-atomic columns to character for CSV: {paste(non_atomic, collapse = ", ")}"))
      for (col in non_atomic) {
        df[[col]] <- vapply(df[[col]], function(x) {
          if (is.null(x)) return(NA_character_)
          tryCatch(as.character(x), error = function(e) "<complex_value>")
        }, FUN.VALUE = "")
      }
    }

    return(df)
  }
  
  # Save each component as CSV
  # Helper to write CSV with fallback to .qs if CSV fails
  write_safe_csv <- function(df, path) {
    if (is.null(df)) return(FALSE)
    df2 <- flatten_for_csv(df)
    # Quick check for remaining list/matrix columns
    bad_cols <- names(df2)[vapply(df2, function(x) is.list(x) || is.matrix(x), logical(1))]
    if (length(bad_cols) > 0) {
      message(glue::glue("Skipping CSV write for {path}: list/matrix columns present: {paste(bad_cols, collapse=', ')}. Writing .qs instead."))
      tryCatch({
        qs2::qs_save(df2, sub("\\.csv$", ".qs", path))
      }, error = function(e) {
        message(glue::glue("Failed to save fallback .qs for {path}: {e$message}"))
      })
      return(FALSE)
    }

    tryCatch({
      readr::write_csv(df2, path)
      TRUE
    }, error = function(e) {
      message(glue::glue("Failed to write CSV {path}: {e$message}. Writing .qs fallback."))
      tryCatch({
        qs2::qs_save(df2, sub("\\.csv$", ".qs", path))
      }, error = function(e2) {
        message(glue::glue("Failed to save fallback .qs for {path}: {e2$message}"))
      })
      FALSE
    })
  }
  if (!is.null(postprocessed_list$summary)) {
    write_safe_csv(postprocessed_list$summary, file.path(output_dir, "model_summary.csv"))
  }
  
  if (!is.null(postprocessed_list$family)) {
    write_safe_csv(postprocessed_list$family, file.path(output_dir, "model_family.csv"))
  }
  
  if (!is.null(postprocessed_list$emms)) {
    write_safe_csv(postprocessed_list$emms, file.path(output_dir, "emms.csv"))
  }
  
  if (!is.null(postprocessed_list$contrasts)) {
    write_safe_csv(postprocessed_list$contrasts, file.path(output_dir, "posthoc_contrasts.csv"))
  }
  
  if (!is.null(postprocessed_list$letters)) {
    write_safe_csv(postprocessed_list$letters, file.path(output_dir, "posthoc_letters.csv"))
  }
  
  if (!is.null(postprocessed_list$predictions)) {
    preds <- postprocessed_list$predictions
    # If predictions is a list with marginal/conditional elements, write both
    if (is.list(preds) && ("marginal" %in% names(preds) || "conditional" %in% names(preds))) {
        if (!is.null(preds$marginal)) {
          write_safe_csv(preds$marginal, file.path(output_dir, "predictions_marginal.csv"))
          # Keep legacy filename for compatibility (predictions.csv -> marginal)
          write_safe_csv(preds$marginal, file.path(output_dir, "predictions.csv"))
        }
        if (!is.null(preds$conditional)) {
          write_safe_csv(preds$conditional, file.path(output_dir, "predictions_conditional.csv"))
        }
    } else {
      write_safe_csv(preds, file.path(output_dir, "predictions.csv"))
    }
  }
  
  return(TRUE)
}

#' Save a model object to .qs format
#' @param model Model object
#' @param output_path Path to save .qs file
#' @return The output_path if successful, NULL otherwise
save_model_object <- function(model, output_path) {
  # Handle adaptive models (list with $model component, but NOT actual model objects)
  # GAM/GLM/LM objects also have $model component (the model frame), so check class first
  if (is.list(model) && "model" %in% names(model) && 
      !inherits(model, c("gam", "glm", "lm", "glmmTMB"))) {
    model <- model$model
  }
  
  if (is.null(model)) {
    message(glue::glue("Skipping save for {output_path} (NULL model)"))
    return(NULL)
  }
  
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  qs2::qs_save(model, output_path)
  return(output_path)
}

#' Save a list of models with postprocessing and diagnostics
#' @param model_list Named list of models
#' @param base_output_dir Base directory for outputs
#' @param model_subdir Subdirectory for model objects
#' @param csv_subdir Subdirectory for CSV summaries
#' @param diagnostic_subdir Subdirectory for diagnostic plots
save_models_complete <- function(model_list, base_output_dir, 
                                model_subdir = "objects",
                                csv_subdir = "summaries",
                                diagnostic_subdir = "diagnostics") {
  
  purrr::iwalk(model_list, function(model, name) {
    # Save model object
    model_path <- file.path(base_output_dir, model_subdir, paste0(name, ".qs"))
    save_model_object(model, model_path)
    
    # Postprocess and save CSVs
    postprocessed <- postprocess_treatment_model(model)
    csv_dir <- file.path(base_output_dir, csv_subdir, name)
    save_model_outputs(postprocessed, csv_dir)
    
    # Create diagnostic plot
    diag_path <- file.path(base_output_dir, diagnostic_subdir, paste0(name, "_diagnostics.png"))
    create_diagnostic_plot(model, name, diag_path)
  })
}
