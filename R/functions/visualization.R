# =============================================================
# Visualization Helper Functions
# -------------------------------------------------------------
# Functions to create publication-ready plots from model outputs
# For use in Quarto reports
# =============================================================

#' Load model outputs from CSV files
#' @param model_name Name of model (e.g., "pika_feeding", "plant_cover/grasses")
#' @param base_dir Base directory (default: data/clean)
#' @return Named list with emms, contrasts, letters, summary
load_model_outputs <- function(model_name, base_dir = "data/clean") {
  model_dir <- here::here(base_dir, model_name)
  
  # Initialize outputs with NULL values
  outputs <- list(
    emms = NULL,
    contrasts = NULL,
    letters = NULL,
    summary = NULL,
    predictions = NULL
  )
  
  # Try to load each file
  if (file.exists(file.path(model_dir, "emms.csv"))) {
    outputs$emms <- readr::read_csv(file.path(model_dir, "emms.csv"), show_col_types = FALSE)
  }
  
  if (file.exists(file.path(model_dir, "posthoc_contrasts.csv"))) {
    outputs$contrasts <- readr::read_csv(file.path(model_dir, "posthoc_contrasts.csv"), show_col_types = FALSE)
  }
  
  if (file.exists(file.path(model_dir, "posthoc_letters.csv"))) {
    outputs$letters <- readr::read_csv(file.path(model_dir, "posthoc_letters.csv"), show_col_types = FALSE)
  }
  
  if (file.exists(file.path(model_dir, "model_summary.csv"))) {
    outputs$summary <- readr::read_csv(file.path(model_dir, "model_summary.csv"), show_col_types = FALSE)
  }
  
  if (file.exists(file.path(model_dir, "predictions.csv"))) {
    outputs$predictions <- readr::read_csv(file.path(model_dir, "predictions.csv"), show_col_types = FALSE)
  }
  
  return(outputs)
}

#' Load a model object from .qs file
#' @param model_name Name of model file (without .qs extension)
#' @param base_dir Base directory (default: data/models)
#' @return Model object
load_model_object <- function(model_name, base_dir = "data/models") {
  model_path <- here::here(base_dir, paste0(model_name, ".qs"))
  
  if (!file.exists(model_path)) {
    warning(glue::glue("Model file not found: {model_path}"))
    return(NULL)
  }
  
  qs2::qs_read(model_path)
}

#' Create treatment comparison plot
#' @param emms_data Tibble with EMMs
#' @param letters_data Tibble with compact letter display
#' @param raw_data Raw data for points (optional)
#' @param response_var Name of response variable in raw data
#' @param y_label Y-axis label
#' @param title Plot title
#' @param colors Named vector of colors for treatments
#' @return ggplot object
plot_treatment_comparison <- function(emms_data, letters_data, 
                                     raw_data = NULL, 
                                     response_var = NULL,
                                     y_label = "Response",
                                     title = NULL,
                                     colors = c("no pika" = "#ff8080", "pika" = "#2fc09b")) {
  
  # Create base plot with EMMs
  p <- ggplot2::ggplot(emms_data, 
                       ggplot2::aes(x = pika_treatment, 
                                   y = emmean, 
                                   fill = posion_plant_treatment)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.9), color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
      position = ggplot2::position_dodge(0.9),
      width = 0.2
    )
  
  # Add raw data points if provided
  if (!is.null(raw_data) && !is.null(response_var)) {
    p <- p + ggplot2::geom_jitter(
      data = raw_data,
      ggplot2::aes(y = .data[[response_var]]),
      position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
      alpha = 0.3, size = 1
    )
  }
  
  # Add letters if provided
  if (!is.null(letters_data)) {
    # Calculate y position for letters (above error bars)
    letters_data <- letters_data |>
      dplyr::mutate(
        y_pos = upper.CL * 1.05
      )
    
    p <- p + ggplot2::geom_text(
      data = letters_data,
      ggplot2::aes(label = .group, y = y_pos),
      position = ggplot2::position_dodge(0.9),
      vjust = 0
    )
  }
  
  # Styling
  p <- p +
    ggplot2::labs(y = y_label, x = "Pika Treatment", fill = "Plant Treatment", title = title) +
    ggpubr::theme_pubr() +
    ggplot2::theme(legend.position = "right")
  
  return(p)
}

#' Create diet selection plot (for plant groups)
#' @param emms_data Tibble with EMMs
#' @param letters_data Tibble with letters
#' @param raw_data Raw data (optional)
#' @param response_var Response variable name
#' @param y_label Y-axis label
#' @param title Plot title
#' @return ggplot object
plot_diet_selection <- function(emms_data, letters_data, 
                               raw_data = NULL, 
                               response_var = NULL,
                               y_label = "Frequency (%)",
                               title = NULL) {
  
  p <- ggplot2::ggplot(emms_data, 
                      ggplot2::aes(x = reorder(plant_group, emmean), y = emmean)) +
    ggplot2::geom_col(fill = "steelblue", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.2
    )
  
  # Add raw data if provided
  if (!is.null(raw_data) && !is.null(response_var)) {
    p <- p + ggplot2::geom_jitter(
      data = raw_data,
      ggplot2::aes(x = plant_group, y = .data[[response_var]]),
      width = 0.2, alpha = 0.3, size = 1
    )
  }
  
  # Add letters
  if (!is.null(letters_data)) {
    letters_data <- letters_data |>
      dplyr::mutate(y_pos = upper.CL * 1.05)
    
    p <- p + ggplot2::geom_text(
      data = letters_data,
      ggplot2::aes(x = reorder(plant_group, emmean), label = .group, y = y_pos),
      vjust = 0
    )
  }
  
  p <- p +
    ggplot2::labs(y = y_label, x = "", title = title) +
    ggpubr::theme_pubr()
  
  return(p)
}

#' Create GAM smooth plot
#' @param gam_predictions Tibble with fit, se, lower, upper
#' @param raw_data Raw data for points (optional)
#' @param x_var Name of x variable
#' @param y_var Name of y variable
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param title Plot title
#' @return ggplot object
plot_gam_smooth <- function(gam_predictions, raw_data = NULL,
                           x_var = NULL, y_var = NULL,
                           x_label = "Predictor",
                           y_label = "Response",
                           title = NULL) {
  
  # Get variable name from predictions (second column is usually the predictor)
  if (is.null(x_var)) {
    x_var <- names(gam_predictions)[1]
  }
  
  p <- ggplot2::ggplot(gam_predictions, ggplot2::aes(x = .data[[x_var]], y = fit)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue") +
    ggplot2::geom_line(color = "steelblue", size = 1.2)
  
  # Add raw data if provided
  if (!is.null(raw_data) && !is.null(y_var)) {
    p <- p + ggplot2::geom_point(
      data = raw_data,
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]),
      alpha = 0.5
    )
  }
  
  p <- p +
    ggplot2::labs(x = x_label, y = y_label, title = title) +
    ggpubr::theme_pubr()
  
  return(p)
}

#' Create multi-panel figure combining multiple plots
#' @param plot_list Named list of ggplot objects
#' @param ncol Number of columns
#' @param labels Panel labels (A, B, C, etc.)
#' @return Combined plot (patchwork object)
create_multipanel_figure <- function(plot_list, ncol = 2, labels = "AUTO") {
  patchwork::wrap_plots(plot_list, ncol = ncol) +
    patchwork::plot_annotation(tag_levels = labels)
}

#' Format model summary table for reports
#' @param summary_data Model summary tibble
#' @param caption Table caption
#' @param digits Number of decimal places
#' @return gt table object
format_model_table <- function(summary_data, caption = "Model Summary", digits = 3) {
  if (is.null(summary_data)) {
    warning("summary_data is NULL, returning empty table")
    return(gt::gt(tibble::tibble(Message = "No data available")))
  }
  
  summary_data |>
    dplyr::filter(effect == "fixed") |>
    dplyr::select(term, estimate, std.error, statistic, p.value) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits)),
      p.value = format.pval(p.value, digits = 3, eps = 0.001)
    ) |>
    gt::gt() |>
    gt::tab_header(title = caption) |>
    gt::cols_label(
      term = "Term",
      estimate = "Estimate",
      std.error = "SE",
      statistic = "z/t",
      p.value = "p-value"
    )
}

#' Format contrast table for reports
#' @param contrast_data Contrast tibble
#' @param caption Table caption
#' @param digits Number of decimal places
#' @return gt table object
format_contrast_table <- function(contrast_data, caption = "Pairwise Contrasts", digits = 3) {
  if (is.null(contrast_data)) {
    warning("contrast_data is NULL, returning empty table")
    return(gt::gt(tibble::tibble(Message = "No data available")))
  }

  # Helper to find the first matching column name from a set of candidates
  find_col <- function(cands, names_vec) {
    found <- intersect(cands, names_vec)
    if (length(found)) return(found[1])
    return(NA_character_)
  }

  nm <- names(contrast_data)
  # possible column name variants used across outputs
  contrast_col <- find_col(c("contrast", "Contrast"), nm)
  estimate_col <- find_col(c("estimate", "ratio", "odds.ratio", "odds_ratio"), nm)
  se_col <- find_col(c("SE", "se", "std.error", "Std..Error", "Std.Error"), nm)
  stat_col <- find_col(c("z.ratio", "t.ratio", "statistic", "z_ratio", "stat"), nm)
  p_col <- find_col(c("p.value", "p_value", "p", "P"), nm)

  # Create standardized columns (fill with NA if missing)
  df <- contrast_data
  df$.contrast <- if (!is.na(contrast_col)) contrast_data[[contrast_col]] else NA_character_
  df$.estimate <- if (!is.na(estimate_col)) contrast_data[[estimate_col]] else NA_real_
  df$.SE <- if (!is.na(se_col)) contrast_data[[se_col]] else NA_real_
  df$.stat <- if (!is.na(stat_col)) contrast_data[[stat_col]] else NA_real_
  df$.p <- if (!is.na(p_col)) contrast_data[[p_col]] else NA_real_

  out_df <- tibble::tibble(
    contrast = df$.contrast,
    estimate = df$.estimate,
    SE = df$.SE,
    stat = df$.stat,
    p.value = df$.p
  )

  # Round numeric columns and format p-values using base R to avoid NSE issues
  num_idx <- vapply(out_df, is.numeric, logical(1))
  out_df[num_idx] <- lapply(out_df[num_idx], function(x) round(x, digits))
  out_df$p.value <- format.pval(out_df$p.value, digits = 3, eps = 0.001)

  gt::gt(out_df) |>
    gt::tab_header(title = caption) |>
    gt::cols_label(
      contrast = "Contrast",
      estimate = "Estimate",
      SE = "SE",
      stat = "z/t",
      p.value = "p-value"
    )
}
