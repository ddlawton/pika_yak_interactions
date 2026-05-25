# =============================================================
# Test Script: Model Family Detection
# -------------------------------------------------------------
# Purpose: Load all model objects and verify their families
# =============================================================

library(tidyverse)
library(qs2)
library(here)
library(glue)
library(glmmTMB)  # Needed for family.glmmTMB method

here::i_am("README.md")

# =============================================================
# HELPER FUNCTIONS
# =============================================================

# Get family information from a model
get_model_family <- function(model, model_name) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Model:", model_name, "\n")
  cat(rep("=", 60), "\n", sep = "")
  
  if (is.null(model)) {
    cat("ERROR: Model is NULL\n")
    return(list(
      name = model_name,
      class = "NULL",
      family = NA_character_,
      tweedie_power = NA_real_,
      random_effects = NA_character_
    ))
  }
  
  # Check if it's an adaptive model (list with $model)
  # Note: GAM/GLM/LM objects also have $model (the model frame), so check those first
  is_adaptive <- FALSE
  
  # Special handling for GAM/LM models saved as lists (before checking adaptive)
  if (is.list(model) && "formula" %in% names(model) && "coefficients" %in% names(model)) {
    cat("Note: Detected model saved as list - attempting to restore class\n")
    
    # Check for GAM-specific components
    if ("smooth" %in% names(model) && "sp" %in% names(model)) {
      cat("Restoring as GAM model\n")
      class(model) <- c("gam", "glm", "lm")
    } else if ("family" %in% names(model) && "qr" %in% names(model)) {
      cat("Restoring as GLM model\n")
      class(model) <- c("glm", "lm")
    } else if ("qr" %in% names(model) && "terms" %in% names(model)) {
      cat("Restoring as LM model\n")
      class(model) <- "lm"
    }
  }
  
  # Now check if it's truly an adaptive model (list with $model that's NOT a restored GAM/LM)
  if (is.list(model) && "model" %in% names(model) && 
      !inherits(model, c("gam", "glm", "lm", "glmmTMB"))) {
    is_adaptive <- TRUE
    cat("Note: This is an adaptive model (list structure)\n")
    cat("List names:", paste(names(model), collapse = ", "), "\n")
    actual_model <- model$model
  } else {
    actual_model <- model
  }
  
  if (is.null(actual_model)) {
    cat("ERROR: Extracted model is NULL\n")
    return(list(
      name = model_name,
      class = "NULL (from list)",
      family = NA_character_,
      tweedie_power = NA_real_,
      random_effects = NA_character_
    ))
  }
  
  # Print class information
  model_class <- class(actual_model)
  cat("Class:", paste(model_class, collapse = ", "), "\n")
  
  # Get family
  fam <- tryCatch({
    if (inherits(actual_model, "glmmTMB")) {
      # Extract family from glmmTMB model object directly
      fam_obj <- actual_model$modelInfo$family
      if (!is.null(fam_obj)) {
        list(
          family = fam_obj$family,
          link = fam_obj$link
        )
      } else {
        NULL
      }
    } else {
      family(actual_model)
    }
  }, error = function(e) {
    cat("WARNING: Could not extract family:", e$message, "\n")
    NULL
  })
  
  if (!is.null(fam)) {
    cat("Family name:", fam$family, "\n")
    cat("Link function:", fam$link, "\n")
    
    # Check for Tweedie power parameter
    tweedie_power <- NA_real_
    if (grepl("tweedie", fam$family, ignore.case = TRUE)) {
      cat("Tweedie model detected - attempting to extract power parameter\n")
      
      # Try multiple methods
      tweedie_power <- tryCatch({
        # Method 1: from family object
        if (!is.null(fam$par)) {
          cat("  Method 1 (fam$par):", fam$par, "\n")
          return(fam$par)
        }
        # Method 2: from modelInfo
        if (inherits(actual_model, "glmmTMB") && !is.null(actual_model$modelInfo$family$par)) {
          cat("  Method 2 (modelInfo$family$par):", actual_model$modelInfo$family$par, "\n")
          return(actual_model$modelInfo$family$par)
        }
        # Method 3: from fit parameters
        if (inherits(actual_model, "glmmTMB") && !is.null(actual_model$fit$par)) {
          disp_par <- actual_model$fit$par
          if (length(disp_par) >= 2) {
            cat("  Method 3 (fit$par[2]):", disp_par[2], "\n")
            return(disp_par[2])
          }
        }
        cat("  WARNING: Could not extract Tweedie power parameter\n")
        NA_real_
      }, error = function(e) {
        cat("  ERROR extracting power:", e$message, "\n")
        NA_real_
      })
    }
  } else {
    cat("No family information available\n")
    if (inherits(actual_model, "lm")) {
      cat("Note: lm objects use Gaussian family by default\n")
      fam <- list(family = "gaussian", link = "identity")
    }
  }
  
  # Get random effects
  random_effects <- ""
  if (inherits(actual_model, "gam")) {
    cat("GAM model - checking for random effects\n")
    formula_obj <- formula(actual_model)
    terms_char <- as.character(formula_obj)[3]
    cat("Formula RHS:", terms_char, "\n")
    
    # Check smooth summary
    smooth_summ <- summary(actual_model)$s.table
    if (!is.null(smooth_summ) && nrow(smooth_summ) > 0) {
      cat("Smooth terms:\n")
      print(rownames(smooth_summ))
      
      # Look for random effects (bs='re')
      re_terms <- rownames(smooth_summ)[grepl(",re\\)", rownames(smooth_summ))]
      if (length(re_terms) > 0) {
        re_vars <- gsub("s\\(([^,)]+).*\\)", "\\1", re_terms)
        random_effects <- paste(re_vars, collapse = ", ")
        cat("Random effects detected:", random_effects, "\n")
      }
    }
  } else if (inherits(actual_model, "glmmTMB")) {
    cat("glmmTMB model - checking for random effects\n")
    re_struct <- actual_model$modelInfo$reTrms
    if (!is.null(re_struct) && length(re_struct$cond$cnms) > 0) {
      re_names <- names(re_struct$cond$cnms)
      random_effects <- paste(re_names, collapse = ", ")
      cat("Random effects:", random_effects, "\n")
    }
  } else if (inherits(actual_model, "lm") && !inherits(actual_model, "glmmTMB")) {
    cat("LM model - no random effects\n")
  }
  
  # Print formula
  form <- tryCatch({
    formula(actual_model)
  }, error = function(e) NULL)
  
  if (!is.null(form)) {
    cat("Formula:", deparse(form), "\n")
  }
  
  list(
    name = model_name,
    class = paste(model_class, collapse = "/"),
    family = if(!is.null(fam)) fam$family else NA_character_,
    link = if(!is.null(fam)) fam$link else NA_character_,
    tweedie_power = if(exists("tweedie_power")) tweedie_power else NA_real_,
    random_effects = random_effects,
    is_adaptive = is_adaptive
  )
}

# =============================================================
# LOAD ALL MODELS
# =============================================================

model_files <- list(
  # Diet selection models
  pika_feeding = here("data/models/pika_feeding.qs"),
  yak_grazing = here("data/models/yak_grazing.qs"),
  burrow_cover_gam = here("data/models/burrow_cover_gam.qs"),
  dung_cover = here("data/models/dung_cover.qs"),
  
  # Weight gain
  weight_gain = here("data/models/weight_gain.qs"),
  weight_gain_burrow_gam = here("data/models/weight_gain_burrow_gam.qs"),
  
  # Plant cover
  plant_cover_grasses = here("data/models/plant_cover_grasses.qs"),
  plant_cover_sedges = here("data/models/plant_cover_sedges.qs"),
  plant_cover_forbs = here("data/models/plant_cover_forbs.qs"),
  plant_cover_s_chamaejasme = here("data/models/plant_cover_s_chamaejasme.qs"),
  active_burrows = here("data/models/active_burrows.qs"),
  
  # Forage quality
  forage_quality_cp = here("data/models/forage_quality_cp.qs"),
  forage_quality_adf = here("data/models/forage_quality_adf.qs"),
  forage_quality_ndf = here("data/models/forage_quality_ndf.qs"),
  
  # Plant bites
  plant_bites_sedges = here("data/models/plant_bites_sedges.qs"),
  plant_bites_grasses = here("data/models/plant_bites_grasses.qs"),
  plant_bites_forbs = here("data/models/plant_bites_forbs.qs"),
  
  # Bite steps ratio
  bite_steps_sedges = here("data/models/bite_steps_ratio_sedges.qs"),
  bite_steps_grasses = here("data/models/bite_steps_ratio_grasses.qs"),
  bite_steps_forbs = here("data/models/bite_steps_ratio_forbs.qs")
)

cat("\n")
cat("#############################################################\n")
cat("#                  MODEL FAMILY TESTING                     #\n")
cat("#############################################################\n")
cat("\nChecking", length(model_files), "model files...\n")

# Load and test each model
results_list <- list()

for (i in seq_along(model_files)) {
  model_path <- model_files[[i]]
  model_name <- names(model_files)[i]
  
  if (!file.exists(model_path)) {
    cat("\n", rep("=", 60), "\n", sep = "")
    cat("Model:", model_name, "\n")
    cat(rep("=", 60), "\n", sep = "")
    cat("ERROR: File not found:", model_path, "\n")
    
    results_list[[i]] <- list(
      name = model_name,
      class = "FILE_NOT_FOUND",
      family = NA_character_,
      tweedie_power = NA_real_,
      random_effects = NA_character_
    )
    next
  }
  
  model <- tryCatch({
    qs2::qs_read(model_path)
  }, error = function(e) {
    cat("\n", rep("=", 60), "\n", sep = "")
    cat("Model:", model_name, "\n")
    cat(rep("=", 60), "\n", sep = "")
    cat("ERROR loading model:", e$message, "\n")
    NULL
  })
  
  results_list[[i]] <- get_model_family(model, model_name)
}

# =============================================================
# SUMMARY TABLE
# =============================================================

cat("\n\n")
cat("#############################################################\n")
cat("#                    SUMMARY TABLE                          #\n")
cat("#############################################################\n\n")

results_df <- bind_rows(results_list)

# Print summary table
results_df |>
  select(name, class, family, link, tweedie_power, random_effects, is_adaptive) |>
  mutate(
    tweedie_power = if_else(is.na(tweedie_power), "", as.character(round(tweedie_power, 3))),
    is_adaptive = if_else(is.na(is_adaptive), FALSE, is_adaptive)
  ) |>
  print(n = Inf, width = Inf)

# Save results
output_file <- here("output/model_family_test_results.csv")
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write_csv(results_df, output_file)

cat("\n\nResults saved to:", output_file, "\n")

# Count by family
cat("\n\nSummary by family:\n")
results_df |>
  filter(!is.na(family)) |>
  count(family, name = "count") |>
  arrange(desc(count)) |>
  print()

# Identify issues
issues <- results_df |>
  filter(is.na(family) | class %in% c("NULL", "FILE_NOT_FOUND", "data.frame"))

if (nrow(issues) > 0) {
  cat("\n\nWARNING: Issues found with", nrow(issues), "models:\n")
  print(issues)
} else {
  cat("\n\nAll models loaded successfully!\n")
}
