# =============================================================
# Data Preprocessing Functions
# -------------------------------------------------------------
# Extracted from 01_data_management.R
# Functions to read raw Excel files and create clean tibbles
# =============================================================

# ---- Utility Function to Extract Month from Column Names ----
extract_month <- function(name, june_pattern, july_pattern, aug_pattern) {
  dplyr::case_when(
    stringr::str_detect(name, june_pattern) ~ "June",
    stringr::str_detect(name, july_pattern) ~ "July",
    stringr::str_detect(name, aug_pattern) ~ "August",
    TRUE ~ NA_character_
  )
}

# ---- Utility Function for Beta Transformation ----
#' Apply beta transformation to proportion data for beta regression
#' @param x Vector of proportions (0-1 or 0-100)
#' @param is_percent Logical, whether x is in percentage scale (default TRUE)
#' @return Beta-transformed values that avoid 0 and 1
beta_transform <- function(x, is_percent = TRUE) {
  if (is_percent) {
    x <- x / 100
  }
  n <- length(x)
  (x * (n - 1) + 0.5) / n
}

# ===========================================================
# RAW DATA READING FUNCTIONS
# ===========================================================

#' Read pika feeding frequency data (Figure 2A)
read_pika_feeding <- function(raw_file_path) {
  readxl::read_excel(raw_file_path, skip = 1, sheet = "Fig. 2") |>
    dplyr::select(Plot...2:...6) |>
    tidyr::pivot_longer(
      cols = c(`Feeding/clipping frequency (%)`, ...5, ...6),
      names_to = "name", 
      values_to = "feeding_clipping_freq"
    ) |>
    dplyr::mutate(
      month = dplyr::case_when(
        name == "...5" ~ "July",
        name == "...6" ~ "August",
        name == "Feeding/clipping frequency (%)" ~ "June"
      ),
      animal = "pika",
      plot = factor(Plot...2),
      feeding_clipping_freq_beta = beta_transform(feeding_clipping_freq, is_percent = TRUE)
    ) |>
    dplyr::select(plot, plant_group = `Plant groups...3`, month, animal, 
                  feeding_clipping_freq, feeding_clipping_freq_beta)
}

#' Read yak grazing frequency data (Figure 2B)
read_yak_grazing <- function(raw_file_path) {
  readxl::read_excel(raw_file_path, skip = 1, sheet = "Fig. 2") |>
    dplyr::select(Transect:...14) |>
    tidyr::pivot_longer(
      cols = c(`Grazing frequency (%)`, ...13, ...14),
      names_to = "name", 
      values_to = "grazing_freq"
    ) |>
    dplyr::mutate(
      month = dplyr::case_when(
        name == "...13" ~ "July",
        name == "...14" ~ "August",
        name == "Grazing frequency (%)" ~ "June"
      ),
      animal = "yak",
      transect = factor(Transect),
      grazing_freq = tidyr::replace_na(grazing_freq, 0)
    ) |>
    dplyr::select(transect, plant_group = `Plant groups...11`, month, animal, 
                  grazing_freq) |>
    dplyr::filter(plant_group != 'S. chamaejasme') |>
    dplyr::mutate(
      grazing_freq_beta = beta_transform(grazing_freq, is_percent = TRUE)
    )
}

#' Read dung and burrow density data (Figure 2C/D)
read_dung_burrow <- function(raw_file_path) {
  readxl::read_excel(raw_file_path, skip = 1, sheet = "Fig. 2") |>
    dplyr::select(Plot...18:`Yak  dung(no./100m2)`) |>
    janitor::clean_names() |>
    dplyr::rename(plot = plot_18) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      plot = factor(plot),
      s_chamaejasme_cover_percent_beta = beta_transform(s_chamaejasme_cover_percent, is_percent = TRUE)
    )
}

#' Read yak weight gain data (Figure 3A)
read_yak_weight_gain <- function(raw_file_path) {
  df <- readxl::read_excel(raw_file_path, skip = 2, sheet = "Fig. 3a") |>
    janitor::remove_empty() |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = yak_1_6:yak_2_11, 
      names_to = "yak", 
      values_to = "weight_gain"
    ) |>
    dplyr::mutate(
      month = dplyr::case_when(
        yak %in% c("yak_1_6", "yak_2_7") ~ "June",
        yak %in% c("yak_1_8", "yak_2_9") ~ "July",
        yak %in% c("yak_1_10", "yak_2_11") ~ "August"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("block", "year")), factor)
    )
  
  # Apply yak ID refinement (from script 04)
  df |>
    dplyr::mutate(
      yak = gsub("_[^_]*$", '', yak),
      treatment_code = paste0(
        ifelse(pika_treatment == "pika", "P", "A"),
        ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
      ),
      yak = paste0("B", block, "_", treatment_code, "_", yak)
    ) |>
    dplyr::select(-treatment_code)
}

#' Read plant cover data (Figure 3B/C and S1)
read_plant_cover <- function(raw_file_path) {
  readxl::read_excel(raw_file_path, skip = 3, sheet = "Fig. 3b,c and Fig. S1") |>
    janitor::remove_empty() |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = s_chamaejasme_percent_5:forbs_percent_16,
      names_to = "plant", 
      values_to = "cover"
    ) |>
    dplyr::mutate(
      month = extract_month(plant, "_5$|_6$|_7$|_8$", "_9$|_10$|_11$|_12$", "_13$|_14$|_15$|_16$"),
      plant = gsub("_percent_[0-9]+$", "", plant)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("block", "year")), factor)
    ) |>
    dplyr::group_by(plant) |>
    dplyr::mutate(
      cover_beta = beta_transform(cover, is_percent = TRUE)
    ) |>
    dplyr::ungroup()
}

#' Read plant forage quality data (Figure 3D and S2)
read_forage_quality <- function(raw_file_path) {
  readxl::read_excel(raw_file_path, skip = 2, sheet = "Fig. 3d and Fig. S2") |>
    janitor::remove_empty() |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = cp_percent_6:ndf_percent_15,
      names_to = "plant", 
      values_to = "forage_quality"
    ) |>
    dplyr::mutate(
      month = extract_month(plant, "_6$|_7$|_8$|_9$", "_10$|_11$|_12$|_13$", "_14$|_15$|_16$|_17$"),
      plant = gsub("_percent_[0-9]+$", "", plant)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("block", "year")), factor)
    ) |>
    dplyr::group_by(plant) |>
    dplyr::mutate(
      forage_quality_beta = beta_transform(forage_quality, is_percent = TRUE)
    ) |>
    dplyr::ungroup()
}

#' Read plant bites data (Figure 4A-C and S3A)
read_plant_bites <- function(raw_file_path) {
  df <- readxl::read_excel(raw_file_path, skip = 3, sheet = "Fig.4a-c and Fig. S3a") |>
    janitor::remove_empty() |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = sedges_6:forbs_23,
      names_to = "plant", 
      values_to = "forage_efficiency"
    ) |>
    dplyr::mutate(
      num = as.numeric(stringr::str_extract(plant, "\\d+$")),
      yak = dplyr::case_when(
        num %in% c(6:8, 12:14, 18:20) ~ "yak_1",
        num %in% c(9:11, 15:17, 21:23) ~ "yak_2",
        TRUE ~ NA_character_
      ),
      month = dplyr::case_when(
        num %in% 6:11 ~ "June",
        num %in% 12:17 ~ "July",
        num %in% 18:23 ~ "August"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("block", "year")), factor)
    ) |>
    dplyr::select(-num)
  
  # Apply yak ID refinement (from script 04)
  df |>
    dplyr::mutate(
      plant = gsub("_[^_]*$", '', plant),
      treatment_code = paste0(
        ifelse(pika_treatment == "pika", "P", "A"),
        ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
      ),
      yak = paste0("B", block, "_", treatment_code, "_", yak)
    ) |>
    dplyr::select(-treatment_code)
}

#' Read bite-to-step ratio data (Figure 4E/F and S3B)
read_bite_steps_ratio <- function(raw_file_path) {
  df <- readxl::read_excel(raw_file_path, skip = 2, sheet = "Fig.4d-f and Fig.S3b") |>
    janitor::remove_empty() |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = sedges_5:forbs_22,
      names_to = "plant", 
      values_to = "bites_steps_ratio"
    ) |>
    dplyr::mutate(
      num = as.numeric(stringr::str_extract(plant, "\\d+$")),
      yak_num = dplyr::case_when(
        num %in% c(5:7, 11:13, 17:19) ~ "yak_1",
        num %in% c(8:10, 14:16, 20:22) ~ "yak_2"
      ),
      month = dplyr::case_when(
        num %in% 5:10 ~ "June",
        num %in% 11:16 ~ "July",
        num %in% 17:22 ~ "August"
      ),
      plant = gsub("_[0-9]+$", "", plant)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("block", "year")), factor)
    ) |>
    dplyr::select(-num)
  
  # Apply beta transformation by plant group
  df |>
    dplyr::group_by(plant) |>
    dplyr::mutate(
      bites_steps_ratio_beta = beta_transform(bites_steps_ratio, is_percent = FALSE)
    ) |>
    dplyr::mutate(
      treatment_code = paste0(
        ifelse(pika_treatment == "pika", "P", "A"),
        ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
      ),
      yak = paste0("B", block, "_", treatment_code, "_", yak_num)
    ) |>
    dplyr::select(-c(treatment_code, yak_num)) |>
    dplyr::ungroup()
}

#' Read total steps data (optional - appears in some versions)
read_total_steps <- function(raw_file_path) {
  # This function may need adjustment based on actual data structure
  # Placeholder for now - check if this sheet exists
  tryCatch({
    df <- readxl::read_excel(raw_file_path, sheet = "Total Steps") |>
      janitor::clean_names() |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("block", "year")), factor)
      )
    
    # Apply yak ID refinement
    df |>
      dplyr::mutate(
        yak = gsub("_[^_]*$", '', yak),
        treatment_code = paste0(
          ifelse(pika_treatment == "pika", "P", "A"),
          ifelse(posion_plant_treatment == "S. chamaejasme", "P", "A")
        ),
        yak = paste0("B", block, "_", treatment_code, "_", yak)
      ) |>
      dplyr::select(-treatment_code)
  }, error = function(e) {
    message("Total steps sheet not found or error reading it")
    return(NULL)
  })
}

#' Read initial plant condition data
read_initial_plant_condition <- function(initial_file_path) {
  readxl::read_excel(initial_file_path, skip = 4, sheet = 1) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("block", "plot")), factor)
    )
}

#' Read active burrow data (for Figure 5 analysis)
read_active_burrows <- function(raw_file_path) {
  tryCatch({
    # Read from burrow data.xlsx - skip first 2 rows to get to actual data
    # Columns at skip=2 are: Block, Pika treatment, Posion plant treatment, ...4 (empty), Year,
    # then yak weight columns (6-14), then empty cols, then ...16-18 (duplicates of Block/treatment/year),
    # then columns 19-21 are active_burrows/ha for June/July/August
    # Column 22 is Average, columns 25-26 are summary stats
    df <- readxl::read_excel(raw_file_path, skip = 2) |>
      # Select only the relevant columns: treatments, year, and active burrows/ha
      dplyr::select(
        block = Block,
        pika_treatment = `Pika treatment`,
        posion_plant_treatment = `Posion plant treatment`,
        year = Year,
        june = 19,      # active burrows/ha for June
        july = 20,      # active burrows/ha for July  
        august = 21     # active burrows/ha for August
      ) |>
      # Remove rows with NA block (summary rows at bottom)
      dplyr::filter(!is.na(block)) |>
      # Pivot to long format with month as a variable
      tidyr::pivot_longer(
        cols = c(june, july, august),
        names_to = "month",
        values_to = "active_burrows"
      ) |>
      # Clean up month names
      dplyr::mutate(
        month = dplyr::case_when(
          month == "june" ~ "June",
          month == "july" ~ "July",
          month == "august" ~ "August"
        ),
        # Convert to factors
        block = factor(block),
        pika_treatment = factor(pika_treatment),
        posion_plant_treatment = factor(posion_plant_treatment),
        year = factor(year),
        month = factor(month, levels = c("June", "July", "August"))
      ) |>
      # Remove any rows with missing burrow counts
      dplyr::filter(!is.na(active_burrows))
    
    return(df)
  }, error = function(e) {
    message("Active burrows data not found or error reading: ", e$message)
    return(NULL)
  })
}
