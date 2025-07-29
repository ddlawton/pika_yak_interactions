fit_candidate_models <- function(response,
  data,
  family = gaussian(),
  ziformula = ~0,
  dispformula = ~1,
  REML = FALSE,
  ...) {
stopifnot(is.character(response), length(response) == 1)

# Build the five formulas you want to compare
f_full <- sprintf("%s ~ pika_treatment * posion_plant_treatment + (1|block) + (1|block:yak:month)", response)
f_bm   <- sprintf("%s ~ pika_treatment * posion_plant_treatment + (1 | block) + (1 | month)", response)
f_b    <- sprintf("%s ~ pika_treatment * posion_plant_treatment + (1 | block)", response)
f_m    <- sprintf("%s ~ pika_treatment * posion_plant_treatment + (1 | month)", response)
f_fix  <- sprintf("%s ~ pika_treatment * posion_plant_treatment", response)

formulas <- list(
mod_full = as.formula(f_full),
mod_bm   = as.formula(f_bm),
mod_b    = as.formula(f_b),
mod_m    = as.formula(f_m),
mod_fix  = as.formula(f_fix)
)

# safe wrapper so the function doesn't die if one model fails to converge
fit_one <- function(formula) {
glmmTMB::glmmTMB(
formula     = formula,
data        = data,
family      = family,
ziformula   = ziformula,
dispformula = dispformula,
REML        = REML,
...
)
}

models <- purrr::imap(formulas, ~{
tryCatch(
fit_one(.x),
error = function(e) {
warning(sprintf("Model %s failed: %s", .y, e$message))
NULL
}
)
})

# Drop failed fits (if any)
models <- models[!vapply(models, is.null, logical(1))]

# Model selection table
tab <- purrr::imap_dfr(models, ~{
ll  <- stats::logLik(.x)
tibble::tibble(
model   = .y,
npar    = attr(ll, "df"),
logLik  = as.numeric(ll),
AIC     = AIC(.x),
BIC     = BIC(.x)
)
}) |>
dplyr::mutate(
deltaAIC = AIC - min(AIC,na.rm = TRUE),
deltaBIC = BIC - min(BIC,na.rm = TRUE)
) |>
dplyr::arrange(AIC)

list(models = models, table = tab)
}

#' Extract EMMs and compact-letter display (CLD) from a glmmTMB model
#'
#' @param model  A fitted glmmTMB model.
#' @param spec   A valid `emmeans` specification string or list, e.g.
#'               "pika_treatment * posion_plant_treatment".
#' @param adjust P-value adjustment method passed to `cld()` (default "tukey").
#' @param ...    Passed to `emmeans::emmeans()`.
#'
#' @return A list with:
#'   - emm:   the emmeans object
#'   - cld:   the CLD (letters) data.frame
#'   - table: tidy tibble of the CLD (means, SE, CIs, letters)
extract_emms <- function(model, spec, adjust = "tukey", ...) {
  emm <- emmeans::emmeans(model, specs = spec, ...)
  cld_df <- multcomp::cld(emm, adjust = adjust, Letters = letters, sort = FALSE) |>
    as.data.frame() |>
    # Standardize letter column name
    dplyr::rename(letters = .group) |>
    # Clean whitespace in letters
    dplyr::mutate(letters = stringr::str_squish(letters))
  list(
    emm   = emm,
    cld   = cld_df,
    table = tibble::as_tibble(cld_df)
  )
}


plot_emms <- function(data, emms_letters, yvar, ylab, y_letter_override = NULL) {
  dodge_width <- 0.75
  y_letter_override <- y_letter_override %||% (if (yvar == "weight_gain") 0.55 else 42)

  data |>
    left_join(emms_letters, by = c("pika_treatment", "posion_plant_treatment")) |>
    ggplot(aes(x = pika_treatment, y = .data[[yvar]], color = posion_plant_treatment)) +
    geom_jitter(
      size = 1, alpha = 0.4,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width)
    ) +
    geom_point(
      data = emms_letters,
      aes(x = pika_treatment, y = emmean, fill = posion_plant_treatment),
      position = position_dodge(width = dodge_width),
      alpha = 1, size = 4, pch = 21, color = "black"
    ) +
    geom_text(
      data = emms_letters,
      aes(x = pika_treatment, y = y_letter_override, label = letters, group = posion_plant_treatment),
      position = position_dodge(width = dodge_width),
      size = 5, show.legend = FALSE, color = "black"
    ) +
    MetBrewer::scale_fill_met_d(name = "Lakota") +
    MetBrewer::scale_color_met_d(name = "Lakota") +
    labs(y = ylab, x = "", color = "Poison plant treatment", fill = "Poison plant treatment") +
    theme_pubr(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank())
}


plot_random_effects <- function(model, data, grouping_var, response) {
  re_list <- ranef(model)$cond
  if (!grouping_var %in% names(re_list)) {
    stop("`grouping_var` must be one of: ", paste(names(re_list), collapse = ", "))
  }

  re_tbl <- re_list[[grouping_var]] |> 
    as.data.frame() |> 
    rownames_to_column(grouping_var) |> 
    rename(ranef_intercept = `(Intercept)`)

  intercept <- as.numeric(fixef(model)$cond["(Intercept)"])
  re_tbl <- re_tbl |> mutate(conditional_mean = intercept + ranef_intercept)

  # make join keys characters to be safe
  data2 <- data
  data2[[grouping_var]] <- as.character(data2[[grouping_var]])
  re_tbl[[grouping_var]] <- as.character(re_tbl[[grouping_var]])

  joined <- left_join(data2, re_tbl, by = grouping_var)

  ggplot(joined, aes(x = .data[[grouping_var]], y = .data[[response]])) +
    geom_jitter(width = 0.2, height = 0, size = 0.5, pch = 21) +
    geom_point(aes(y = conditional_mean), color = "blue", size = 2) +
    xlab(grouping_var) + ylab(response) +
    facet_grid(pika_treatment ~ posion_plant_treatment) +
    theme_pubr()
}

