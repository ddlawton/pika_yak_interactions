# =============================================================
# Model Fitting Functions
# -------------------------------------------------------------
# Extracted from scripts 02-06
# Functions to fit GLMMs and GAMs for all analyses
# 
# REPRODUCIBILITY NOTE:
# Family selection for plant bites models is based on empirical
# testing and fixed per plant type (grasses=Gaussian, others=Tweedie).
# This ensures reproducible results across pipeline runs.
# Random seed is set in _targets.R for additional reproducibility.
# =============================================================

# ===========================================================
# STANDARD GLMM FORMULAS
# ===========================================================

#' Standard formula for experimental treatment models
#' @param response Character name of response variable
#' @return Formula object
formula_treatment_model <- function(response) {
  as.formula(paste0(
    response, 
    " ~ pika_treatment * posion_plant_treatment + (1 | block) + (1 | year) + (1 | month)"
  ))
}

#' Formula for diet selection (survey) models
#' @param response Character name of response variable
#' @param grouping_var Name of grouping variable (e.g., "plant_group")
#' @param random_effects Character vector of random effects
#' @return Formula object
formula_diet_model <- function(response, grouping_var = "plant_group", 
                               random_effects = c("plot", "month")) {
  re_terms <- paste0("(1 | ", random_effects, ")", collapse = " + ")
  as.formula(paste0(response, " ~ ", grouping_var, " + ", re_terms))
}

# ===========================================================
# ADAPTIVE MODEL FITTING FUNCTIONS
# ===========================================================

#' Fit GLMM with adaptive family selection (Gaussian → Tweedie fallback)
#' Useful for count data that may be overdispersed
#' @param formula Model formula
#' @param data Data frame
#' @param family_first First family to try (default gaussian)
#' @param family_fallback Fallback family if convergence fails (default tweedie)
#' @return List with model and warnings
fit_adaptive_glmm <- function(formula, data, 
                              family_first = gaussian(), 
                              family_fallback = tweedie(link = "log")) {
  warnings_list <- list()
  
  # Try first family
  model <- tryCatch({
    withCallingHandlers(
      glmmTMB::glmmTMB(
        formula = formula,
        family = family_first,
        data = data
      ),
      warning = function(w) {
        warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) {
    warnings_list[[length(warnings_list) + 1]] <<- paste("First family failed:", conditionMessage(e))
    NULL
  })

  # Check for convergence issues
  convergence_problems <- is.null(model) ||
    any(grepl("Model convergence problem", warnings_list)) ||
    any(grepl("non-positive-definite Hessian", warnings_list))
  
  # Try fallback family if needed
  if (convergence_problems) {
    warnings_list[[length(warnings_list) + 1]] <- "Trying fallback family due to convergence issues"
    
    model <- tryCatch({
      withCallingHandlers(
        glmmTMB::glmmTMB(
          formula = formula,
          family = family_fallback,
          data = data
        ),
        warning = function(w) {
          warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) {
      warnings_list[[length(warnings_list) + 1]] <<- paste("Fallback family failed:", conditionMessage(e))
      NULL
    })
  }

  list(
    model = model,
    warnings = warnings_list,
    family_used = if(!is.null(model)) family(model)$family else NA
  )
}

#' Fit both beta and Gaussian models for comparison
#' @param formula Formula object
#' @param data Data frame
#' @param response_beta Name of beta-transformed response variable
#' @param response_raw Name of raw response variable
#' @return List with both models and warnings
fit_beta_gaussian_pair <- function(formula, data, response_beta, response_raw) {
  warnings_list <- list(beta = list(), gauss = list())

  # Update formula for beta model
  formula_beta <- update(formula, paste0(response_beta, " ~ ."))
  formula_gauss <- update(formula, paste0(response_raw, " ~ ."))
  
  # Fit beta model
  beta_model <- tryCatch({
    withCallingHandlers(
      glmmTMB::glmmTMB(
        formula = formula_beta,
        family = glmmTMB::beta_family(),
        data = data
      ),
      warning = function(w) {
        warnings_list$beta[[length(warnings_list$beta) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) {
    warnings_list$beta[[length(warnings_list$beta) + 1]] <<- conditionMessage(e)
    NULL
  })

  # Fit Gaussian model
  gauss_model <- tryCatch({
    withCallingHandlers(
      glmmTMB::glmmTMB(
        formula = formula_gauss,
        family = gaussian(),
        data = data
      ),
      warning = function(w) {
        warnings_list$gauss[[length(warnings_list$gauss) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) {
    warnings_list$gauss[[length(warnings_list$gauss) + 1]] <<- conditionMessage(e)
    NULL
  })

  list(
    beta = beta_model,
    gauss = gauss_model,
    warnings = warnings_list
  )
}

# ===========================================================
# SPECIFIC MODEL FITTING FUNCTIONS
# ===========================================================

#' Fit pika feeding frequency model (Figure 2A)
fit_pika_feeding_model <- function(data) {
  formula <- formula_diet_model("feeding_clipping_freq", "plant_group", c("plot", "month"))
  glmmTMB::glmmTMB(
    formula = formula,
    family = gaussian(),
    data = data
  )
}

#' Fit yak grazing frequency model (Figure 2B)
fit_yak_grazing_model <- function(data) {
  formula <- formula_diet_model("grazing_freq", "plant_group", c("transect", "month"))
  glmmTMB::glmmTMB(
    formula = formula,
    family = gaussian(),
    data = data
  )
}

#' Fit GAM for burrow density vs. S. chamaejasme cover (Figure 2C)
fit_burrow_cover_gam <- function(data) {
  mgcv::gam(
    s_chamaejasme_cover_percent ~ s(active_pika_burrow_no_100m2, k = 8, bs = 'ts') + s(plot, bs = 're'),
    data = data,
    family = gaussian(),
    select = TRUE
  )
}

#' Fit linear model for dung vs. S. chamaejasme cover (Figure 2D)
fit_dung_cover_model <- function(data) {
  lm(yak_dung_no_100m2 ~ s_chamaejasme_cover_percent, data = data)
}

#' Fit plant cover model (one per plant type)
#' @param data Data frame with cover data for one plant type
#' @param use_beta Logical, whether to use beta-transformed response
#' @return glmmTMB model object
fit_plant_cover_model <- function(data, use_beta = TRUE) {
  response <- if (use_beta) "cover_beta" else "cover"
  formula <- formula_treatment_model(response)
  
  glmmTMB::glmmTMB(
    formula = formula,
    family = if (use_beta) glmmTMB::beta_family() else gaussian(),
    data = data
  )
}

#' Fit forage quality model (one per quality metric)
fit_forage_quality_model <- function(data, use_beta = TRUE) {
  response <- if (use_beta) "forage_quality_beta" else "forage_quality"
  
  # Check if month has sufficient levels (ee_percent has all NA months)
  has_month <- "month" %in% names(data) && 
    length(unique(na.omit(data$month))) > 1
  
  if (has_month) {
    formula <- formula_treatment_model(response)
  } else {
    # Skip month random effect if insufficient data
    formula <- as.formula(paste0(
      response, " ~ pika_treatment * posion_plant_treatment + (1 | block) + (1 | year)"
    ))
  }
  
  glmmTMB::glmmTMB(
    formula = formula,
    family = if (use_beta) glmmTMB::beta_family() else gaussian(),
    data = data
  )
}

#' Fit plant bites model with explicit family per plant type
#' Based on empirical testing, different plant types require different families:
#' - Grasses: Gaussian (converges well)
#' - Sedges: Tweedie (Gaussian has convergence issues)
#' - Forbs: Tweedie (Gaussian has convergence issues)
fit_plant_bites_model <- function(data) {
  formula <- formula_treatment_model("forage_efficiency")
  
  # Determine plant type from data to select appropriate family
  plant_type <- unique(data$plant)
  
  if (length(plant_type) != 1) {
    stop("Data should contain only one plant type")
  }
  
  # Select family based on empirical testing results
  family_to_use <- if (plant_type == "grasses") {
    gaussian()
  } else {
    tweedie(link = "log")
  }
  
  # Fit model with specified family
  model <- glmmTMB::glmmTMB(
    formula = formula,
    family = family_to_use,
    data = data
  )
  
  # Return in same structure as adaptive models for compatibility
  list(
    model = model,
    warnings = list(),
    family_used = family(model)$family
  )
}

#' Fit bite-to-step ratio model (beta family)
fit_bite_steps_model <- function(data, use_beta = TRUE) {
  response <- if (use_beta) "bites_steps_ratio_beta" else "bites_steps_ratio"
  # This dataset doesn't have year, only block and month
  formula <- as.formula(paste0(
    response, " ~ pika_treatment * posion_plant_treatment + (1 | block) + (1 | month)"
  ))
  
  glmmTMB::glmmTMB(
    formula = formula,
    family = if (use_beta) glmmTMB::beta_family() else gaussian(),
    data = data
  )
}

#' Fit weight gain model
fit_weight_gain_model <- function(data) {
  formula <- formula_treatment_model("weight_gain")
  glmmTMB::glmmTMB(
    formula = formula,
    family = gaussian(),
    data = data
  )
}

#' Fit total steps model
fit_total_steps_model <- function(data) {
  formula <- formula_treatment_model("total_steps")
  glmmTMB::glmmTMB(
    formula = formula,
    family = gaussian(),
    data = data
  )
}

#' Fit active burrows model (treatment effect)
fit_active_burrows_model <- function(data) {
  glmmTMB::glmmTMB(
    active_burrows ~ pika_treatment * posion_plant_treatment + (1 | block) + (1 | month),
    family = gaussian(),
    data = data
  )
}

#' Fit GAM for weight gain vs. burrow density (Figure 5)
#' Only uses pika + S. chamaejasme treatment plots
fit_weight_gain_burrow_gam <- function(data) {
  # Filter to pika + poison treatment and ensure factors are properly set
  data_filtered <- data |>
    dplyr::filter(
      pika_treatment == "pika",
      posion_plant_treatment == "S. chamaejasme"
    ) |>
    dplyr::mutate(
      block = factor(block),
      year = factor(year),
      month = factor(month, levels = c("June", "July", "August"))
    )
  
  # Match the standalone script: use select = TRUE and same smooth specification
  mgcv::gam(
    weight_gain_mean ~
      s(active_burrows, bs = 'tp', k = 15) +
      s(block, bs = 're') + s(year, bs = 're') + s(month, bs = 're'),
    select = TRUE,
    data = data_filtered
  )
}

# ===========================================================
# MODEL SPLITTING HELPERS
# ===========================================================

#' Split data by grouping variable for separate models
#' @param data Data frame
#' @param group_var Name of grouping variable
#' @return Named list of data frames
split_by_group <- function(data, group_var = "plant") {
  data |>
    dplyr::ungroup() |>
    dplyr::group_split(.data[[group_var]]) |>
    purrr::set_names(
      data |> 
        dplyr::group_by(.data[[group_var]]) |> 
        dplyr::group_keys() |> 
        dplyr::pull(.data[[group_var]])
    )
}
