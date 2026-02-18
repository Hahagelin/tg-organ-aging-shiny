# Organ-age Results Explorer
# Interactive Shiny application for exploring organ-age analysis results
# Refactored with bslib for modern, professional appearance

library(shiny)
library(bslib)

# ============================================================================
# CONFIGURATION & PATHS
# ============================================================================

.get_app_dir <- function() {
  ofile <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  normalizePath(getwd())
}

APP_DIR <- .get_app_dir()
RESULTS_ROOT <- normalizePath(file.path(APP_DIR, "www/plots"), mustWork = FALSE)
NOTES_ROOT <- normalizePath(file.path(APP_DIR, "../docs/plot_notes"), mustWork = FALSE)

# ============================================================================
# DISPLAY NAME FORMATTING
# ============================================================================

format_display_name <- function(name) {
  if (is.null(name) || length(name) == 0) return(name)
  
  formatted <- gsub("_", " ", name)
  formatted <- gsub("\\b(\\w)", "\\U\\1", formatted, perl = TRUE)
  
  special_cases <- list(
    bmi = "BMI", whr = "Waist-to-hip ratio", hba1c = "HbA1c", ldl = "LDL", hdl = "HDL",
    crp = "CRP", mz = "MZ", dz = "DZ",
    edyrs = "Education years", "fi_10" = "Frailty index (10%)", "fi 10" = "Frailty index (10%)",
    exercise_how_often = "Exercise frequency",
    atrial_fibrillation = "Atrial Fibrillation",
    ischemic_heart_disease = "Ischemic Heart Disease",
    cerebrovascular_disease = "Cerebrovascular Disease",
    heart_failure = "Heart Failure", prostate_cancer = "Prostate Cancer",
    end_of_healthspan = "End of Healthspan", healthspan = "Healthspan",
    "5_years" = "5 Years", "10_years" = "10 Years", "15_years" = "15 Years",
    combined = "Combined", unadjusted = "Unadjusted", adjusted = "Adjusted",
    male = "Male", female = "Female",
    all_pairs = "All Pairs", mz_twins = "MZ Twins", mz_pairs = "MZ Twins",
    dz_twins = "DZ Twins",
    smoking = "Smoking", eversmok = "Smoking (ever)", alcohol = "Alcohol", glucose = "Glucose",
    diabetes = "Diabetes", hypertension = "Hypertension", dementia = "Dementia",
    parkinsons = "Parkinson's Disease", melanoma = "Melanoma",
    cardiovascular = "Cardiovascular", neuropsychiatric = "Neuropsychiatric",
    digestive = "Digestive", endocrine = "Endocrine", malignancy = "Malignancy",
    musculoskeletal = "Musculoskeletal", neurosensorial = "Neurosensorial",
    respiratory = "Respiratory", urological = "Urological"
  )
  
  if (name %in% names(special_cases)) return(special_cases[[name]])
  
  for (key in names(special_cases)) {
    pattern <- paste0("\\b", gsub("_", " ", key), "\\b")
    formatted <- gsub(pattern, special_cases[[key]], formatted, ignore.case = TRUE)
  }
  
  formatted
}

create_display_choices <- function(values) {
  if (length(values) == 0) return(character(0))
  display_names <- sapply(values, format_display_name)
  setNames(values, display_names)
}

get_covariate_set_display_name <- function(analysis_set) {
  mapping <- c(
    "core" = "Multivariable",
    "base" = "Minimally adjusted",
    "extended" = "Multivariable + eGFR (cr-cys)"
  )
  if (analysis_set %in% names(mapping)) {
    return(mapping[analysis_set])
  }
  analysis_set
}

# ============================================================================
# FILE DISCOVERY: COX ANALYSES
# ============================================================================

find_heatmap_svg <- function(analysis_type, analysis_set, subcategory = NULL, scope, within_pair_type = NULL) {
  if (analysis_type == "cox") {
    if (scope == "main") {
      dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, "heatmap", "main")
      filename <- "mortality.svg"
      target <- file.path(dir_path, filename)
      if (file.exists(target)) return(normalizePath(target))
      return(NULL)
    } else if (scope == "within_pair") {
      dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, "heatmap", "within_pair")
      filename <- if (!is.null(within_pair_type)) {
        if (within_pair_type == "mz_twins") "mz_twins.svg"
        else if (within_pair_type == "all_pairs") "all_pairs.svg"
        else paste0(within_pair_type, ".svg")
      } else {
        "all_pairs.svg"
      }
      target <- file.path(dir_path, filename)
      if (file.exists(target)) return(normalizePath(target))
      return(NULL)
    }
    return(NULL)
  } else {
    dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, subcategory, "heatmap", scope)
    filename <- if (scope == "within_pair" && !is.null(within_pair_type)) {
      paste0(within_pair_type, ".svg")
    } else {
      "mortality.svg"
    }
    target <- file.path(dir_path, filename)
    if (file.exists(target)) return(normalizePath(target))
    NULL
  }
}

find_forest_table_svg <- function(analysis_type, analysis_set, subcategory = NULL, outcome, scope, within_pair_type = NULL) {
  if (analysis_type == "cox") {
    if (scope == "main") {
      dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, "forest_table")
      filename <- paste0(outcome, ".svg")
    } else {
      dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, "forest_table", "within_pair")
      filename <- paste0(outcome, "_", within_pair_type, ".svg")
    }
  } else {
    if (scope == "main") {
      dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, subcategory, "forest_table")
      filename <- paste0(outcome, ".svg")
    } else {
      dir_path <- file.path(RESULTS_ROOT, analysis_type, analysis_set, subcategory, "forest_table", "within_pair")
      filename <- paste0(outcome, "_", within_pair_type, ".svg")
    }
  }
  target <- file.path(dir_path, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

discover_outcomes <- function(analysis_type, analysis_set, subcategory = NULL) {
  if (is.null(analysis_type) || is.null(analysis_set)) return(character(0))
  if (analysis_type != "cox" && is.null(subcategory)) return(character(0))
  
  forest_dir <- if (identical(analysis_type, "cox")) {
    file.path(RESULTS_ROOT, analysis_type, analysis_set, "forest_table")
  } else {
    file.path(RESULTS_ROOT, analysis_type, analysis_set, subcategory, "forest_table")
  }
  if (!dir.exists(forest_dir)) return(character(0))
  svg_files <- list.files(forest_dir, pattern = "\\.svg$", full.names = FALSE)
  if (length(svg_files) == 0) return(character(0))
  outcomes <- gsub("\\.svg$", "", svg_files)
  outcomes <- outcomes[!grepl("_(all_pairs|mz_twins)$", outcomes)]
  if (length(outcomes) == 0) return(character(0))
  sort(outcomes)
}

# ============================================================================
# FILE DISCOVERY: BASELINE CURRENT HEALTH
# ============================================================================

discover_mortality_heatmap_variants <- function(analysis_set) {
  heatmap_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "heatmap")
  if (!dir.exists(heatmap_dir)) return(character(0))
  pattern <- "mortality_organ_age_heatmap_(.+)\\.svg$"
  svg_files <- list.files(heatmap_dir, pattern = pattern, full.names = FALSE)
  sort(gsub(pattern, "\\1", svg_files))
}

find_mortality_heatmap_svg <- function(analysis_set, variant) {
  if (is.null(variant)) return(NULL)
  filename <- paste0("mortality_organ_age_heatmap_", variant, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "heatmap", filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

discover_current_health_forest_files <- function(analysis_set) {
  forest_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "forest_table")
  if (!dir.exists(forest_dir)) return(character(0))
  pattern <- "forest_(.+)\\.svg$"
  svg_files <- list.files(forest_dir, pattern = pattern, full.names = FALSE)
  # Exclude within-pair files (they have _all_pairs or _mz_pairs suffix)
  svg_files <- svg_files[!grepl("_(all_pairs|mz_pairs)\\.svg$", svg_files)]
  sort(gsub(pattern, "\\1", svg_files))
}

find_current_health_forest_svg <- function(analysis_set, exposure) {
  if (is.null(exposure)) return(NULL)
  filename <- paste0("forest_", exposure, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "forest_table", filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

# ============================================================================
# FILE DISCOVERY: BASELINE CURRENT HEALTH - WITHIN-PAIR CONTINUOUS OUTCOMES
# ============================================================================

discover_within_pair_heatmap_variants <- function(analysis_set, pair_type = "all_pairs") {
  heatmap_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "within_pair", "heatmap")
  if (!dir.exists(heatmap_dir)) return(character(0))
  pair_prefix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") "mz_pairs" else "all_pairs"
  pattern <- paste0("^", pair_prefix, "_(.+)\\.svg$")
  svg_files <- list.files(heatmap_dir, pattern = pattern, full.names = FALSE)
  sort(gsub(pattern, "\\1", svg_files))
}

find_within_pair_heatmap_svg <- function(analysis_set, variant, pair_type = "all_pairs") {
  if (is.null(variant)) return(NULL)
  pair_prefix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") "mz_pairs" else "all_pairs"
  filename <- paste0(pair_prefix, "_", variant, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "within_pair", "heatmap", filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

discover_within_pair_forest_files <- function(analysis_set, pair_type = "all_pairs") {
  exposures <- character(0)
  
  # Check within_pair directory
  forest_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "within_pair", "forest_table")
  if (dir.exists(forest_dir)) {
    pair_suffix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") "mz_pairs" else "all_pairs"
    pattern <- paste0("^forest_(.+)_", pair_suffix, "\\.svg$")
    svg_files <- list.files(forest_dir, pattern = pattern, full.names = FALSE)
    exposures <- gsub(pattern, "\\1", svg_files)
  }
  
  # Also check discordant directory for variables that don't have within_pair files
  discordant_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "discordant", "forest_table")
  if (dir.exists(discordant_dir)) {
    discordant_suffix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") "mz" else "all"
    discordant_pattern <- paste0("^forest_(.+)_", discordant_suffix, "\\.svg$")
    discordant_files <- list.files(discordant_dir, pattern = discordant_pattern, full.names = FALSE)
    discordant_exposures <- gsub(discordant_pattern, "\\1", discordant_files)
    
    # Map discordant variable names to match main variable names
    name_mapping <- c("smoking" = "eversmok", "exercise" = "exercise_how_often")
    for (i in seq_along(discordant_exposures)) {
      if (discordant_exposures[i] %in% names(name_mapping)) {
        discordant_exposures[i] <- name_mapping[discordant_exposures[i]]
      }
    }
    
    # Combine with existing exposures, removing duplicates
    exposures <- unique(c(exposures, discordant_exposures))
  }
  
  sort(exposures)
}

find_within_pair_forest_svg <- function(analysis_set, exposure, pair_type = "all_pairs") {
  if (is.null(exposure)) return(NULL)
  pair_suffix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") "mz_pairs" else "all_pairs"
  filename <- paste0("forest_", exposure, "_", pair_suffix, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "within_pair", "forest_table", filename)
  if (file.exists(target)) return(normalizePath(target))
  
  # Also check discordant directory for variables that don't have within_pair files
  # Reverse mapping: eversmok -> smoking, exercise_how_often -> exercise
  reverse_mapping <- c("eversmok" = "smoking", "exercise_how_often" = "exercise")
  discordant_exposure <- if (exposure %in% names(reverse_mapping)) {
    reverse_mapping[exposure]
  } else {
    exposure
  }
  
  discordant_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "discordant", "forest_table")
  if (dir.exists(discordant_dir)) {
    discordant_suffix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") "mz" else "all"
    discordant_filename <- paste0("forest_", discordant_exposure, "_", discordant_suffix, ".svg")
    discordant_target <- file.path(discordant_dir, discordant_filename)
    if (file.exists(discordant_target)) return(normalizePath(discordant_target))
    
    # Also try mz_pairs variant if looking for mz_twins
    if (pair_type == "mz_twins") {
      discordant_filename_alt <- paste0("forest_", discordant_exposure, "_mz_pairs.svg")
      discordant_target_alt <- file.path(discordant_dir, discordant_filename_alt)
      if (file.exists(discordant_target_alt)) return(normalizePath(discordant_target_alt))
    }
  }
  
  NULL
}

# ============================================================================
# FILE DISCOVERY: BASELINE DISCORDANT & SEX-STRATIFIED
# ============================================================================

discover_discordant_forest_files <- function(analysis_set) {
  forest_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "discordant", "forest_table")
  if (!dir.exists(forest_dir)) return(list(exposure = character(0), pair_type = character(0)))
  pattern <- "forest_(.+)_(.+)\\.svg$"
  svg_files <- list.files(forest_dir, pattern = pattern, full.names = FALSE)
  matches <- regmatches(svg_files, regexec(pattern, svg_files))
  exposures <- unique(sapply(matches, function(x) x[2]))
  pair_types <- unique(sapply(matches, function(x) x[3]))
  exposures <- exposures[!grepl("_", exposures)]
  pair_types <- pair_types[!pair_types %in% c("focused")]
  pair_types <- gsub("^mz_pairs$", "mz_twins", pair_types)
  list(exposure = sort(exposures), pair_type = sort(pair_types))
}

find_discordant_forest_svg <- function(analysis_set, exposure, pair_type) {
  if (is.null(exposure) || is.null(pair_type)) return(NULL)
  pair_type_file <- if (pair_type == "mz_twins") "mz_twins" else if (pair_type == "mz_pairs") "mz_pairs" else pair_type
  filename <- paste0("forest_", exposure, "_", pair_type_file, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "discordant", "forest_table", filename)
  if (file.exists(target)) return(normalizePath(target))
  if (pair_type == "mz_twins") {
    filename_alt <- paste0("forest_", exposure, "_mz_pairs.svg")
    target_alt <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "discordant", "forest_table", filename_alt)
    if (file.exists(target_alt)) return(normalizePath(target_alt))
  }
  NULL
}

discover_sex_stratified_files <- function(analysis_set, viz_type) {
  sex_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "sex_stratified", viz_type)
  if (!dir.exists(sex_dir)) return(list(exposure = character(0), sex = character(0)))
  if (viz_type == "heatmap") {
    pattern <- "(male|female)_(.+)\\.svg$"
  } else {
    pattern <- "forest_(male|female)_(.+)\\.svg$"
  }
  svg_files <- list.files(sex_dir, pattern = pattern, full.names = FALSE)
  matches <- regmatches(svg_files, regexec(pattern, svg_files))
  sexes <- unique(sapply(matches, function(x) x[2]))
  exposures <- unique(sapply(matches, function(x) x[3]))
  list(exposure = sort(exposures), sex = sort(sexes))
}

find_sex_stratified_svg <- function(analysis_set, viz_type, exposure, sex) {
  if (is.null(exposure) || is.null(sex)) return(NULL)
  filename <- if (viz_type == "heatmap") {
    paste0(sex, "_", exposure, ".svg")
  } else {
    paste0("forest_", sex, "_", exposure, ".svg")
  }
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "current_health", "sex_stratified", viz_type, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

# ============================================================================
# FILE DISCOVERY: BASELINE PREVALENT DISEASE
# ============================================================================

discover_prevalent_disease_time_intervals <- function(analysis_set) {
  prevalent_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "main")
  if (!dir.exists(prevalent_dir)) return(character(0))
  time_intervals <- list.dirs(prevalent_dir, full.names = FALSE, recursive = FALSE)
  time_intervals <- time_intervals[!grepl("^\\.", time_intervals)]
  time_intervals <- time_intervals[time_intervals != "panel"]
  sort(time_intervals)
}

discover_prevalent_disease_diseases <- function(analysis_set, time_interval) {
  if (is.null(time_interval)) return(character(0))
  prevalent_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "main", time_interval)
  if (!dir.exists(prevalent_dir)) return(character(0))
  pattern <- "prevalent_forest_(.+)\\.svg$"
  svg_files <- list.files(prevalent_dir, pattern = pattern, full.names = FALSE)
  sort(gsub(pattern, "\\1", svg_files))
}

find_prevalent_disease_heatmap_svg <- function(analysis_set, time_interval) {
  if (is.null(time_interval)) return(NULL)
  filename <- "mortality_organ_age_heatmap_combined.svg"
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "main", time_interval, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

find_prevalent_disease_forest_svg <- function(analysis_set, time_interval, disease) {
  if (is.null(time_interval) || is.null(disease)) return(NULL)
  filename <- paste0("prevalent_forest_", disease, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "main", time_interval, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

discover_prevalent_disease_within_time_intervals <- function(analysis_set) {
  within_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "within")
  if (!dir.exists(within_dir)) return(character(0))
  time_intervals <- list.dirs(within_dir, full.names = FALSE, recursive = FALSE)
  time_intervals <- time_intervals[!grepl("^\\.", time_intervals)]
  time_intervals <- time_intervals[time_intervals != "panel"]
  sort(time_intervals)
}

discover_prevalent_disease_within_diseases <- function(analysis_set, time_interval, pair_type = NULL) {
  if (is.null(time_interval)) return(character(0))
  within_dir <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "within", time_interval)
  if (!dir.exists(within_dir)) return(character(0))
  
  if (is.null(pair_type)) {
    pattern <- "forest_(all_pairs|mz_pairs)_(.+)\\.svg$"
    svg_files <- list.files(within_dir, pattern = pattern, full.names = FALSE)
    diseases <- gsub(pattern, "\\2", svg_files)
  } else {
    prefix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") {
      "forest_mz_pairs_"
    } else {
      "forest_all_pairs_"
    }
    pattern <- paste0("^", prefix, "(.+)\\.svg$")
    svg_files <- list.files(within_dir, pattern = pattern, full.names = FALSE)
    diseases <- gsub(pattern, "\\1", svg_files)
  }
  sort(unique(diseases))
}

find_prevalent_disease_within_heatmap_svg <- function(analysis_set, time_interval, pair_type = "all_pairs") {
  if (is.null(time_interval)) return(NULL)
  filename <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") {
    "mz_pairs_heatmap.svg"
  } else {
    "all_pairs_heatmap.svg"
  }
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "within", time_interval, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

find_prevalent_disease_within_forest_svg <- function(analysis_set, time_interval, disease, pair_type = "all_pairs") {
  if (is.null(time_interval) || is.null(disease)) return(NULL)
  prefix <- if (pair_type == "mz_twins" || pair_type == "mz_pairs") {
    "forest_mz_pairs_"
  } else {
    "forest_all_pairs_"
  }
  filename <- paste0(prefix, disease, ".svg")
  target <- file.path(RESULTS_ROOT, "baseline", analysis_set, "prevalent_disease", "within", time_interval, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

# ============================================================================
# FILE DISCOVERY: ORGAN MODELS (TOP 10 PROTEINS)
# ============================================================================

discover_organ_model_organs <- function() {
  organ_dir <- file.path(RESULTS_ROOT, "organ_age_distribution", "organ_models")
  if (!dir.exists(organ_dir)) return(character(0))
  svg_files <- list.files(organ_dir, pattern = "_top_proteins\\.svg$", full.names = FALSE)
  if (!length(svg_files)) return(character(0))
  organs <- gsub("_top_proteins\\.svg$", "", svg_files)
  sort(organs)
}

find_organ_model_svg <- function(organ) {
  if (is.null(organ)) return(NULL)
  base_dir <- file.path(RESULTS_ROOT, "organ_age_distribution", "organ_models")
  
  filename <- paste0(organ, "_top_proteins.svg")
  target <- file.path(base_dir, filename)
  
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

# ============================================================================
# FILE DISCOVERY: MULTIMORBIDITY
# ============================================================================

discover_multimorbidity_categories <- function() {
  c("Incident accumulation of disease (AG)" = "ag",
    "Baseline multimorbidities (NB)" = "nb")
}

discover_multimorbidity_svgs <- function(analysis_set, category, scope = NULL) {
  if (is.null(scope)) return(character(0))
  dir_path <- if (identical(scope, "main")) {
    file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "main")
  } else {
    file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "within_pair")
  }
  if (!dir.exists(dir_path)) return(character(0))
  svgs <- list.files(dir_path, pattern = "\\.svg$", full.names = FALSE)
  sort(svgs)
}

resolve_mm_filename <- function(analysis_set, category, scope, within_pair_type) {
  files <- discover_multimorbidity_svgs(analysis_set, category, scope)
  if (!length(files)) return(NULL)
  if (identical(scope, "main")) {
    pref <- grep(paste0("^", category, "_full_main_.*_heatmap\\.svg$"), files, value = TRUE)
    return(if (length(pref)) pref[1] else files[1])
  }
  if (identical(within_pair_type, "mz_twins")) {
    pref <- grep(paste0("^", category, "_full_withinpair_mz_.*_heatmap\\.svg$"), files, value = TRUE)
    return(if (length(pref)) pref[1] else files[1])
  } else {
    pref <- grep(paste0("^", category, "_full_withinpair_.*_heatmap\\.svg$"), files, value = TRUE)
    pref <- setdiff(pref, grep("withinpair_mz_", pref, value = TRUE))
    return(if (length(pref)) pref[1] else files[1])
  }
}

find_multimorbidity_svg <- function(analysis_set, category, filename, scope) {
  if (is.null(filename) || is.null(scope)) return(NULL)
  dir_path <- if (identical(scope, "main")) {
    file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "main")
  } else {
    file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "within_pair")
  }
  target <- file.path(dir_path, filename)
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

discover_mm_outcomes <- function(analysis_set, category, scope) {
  dir_path <- if (identical(scope, "main")) {
    file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "main")
  } else {
    file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "within_pair")
  }
  if (!dir.exists(dir_path)) return(character(0))
  files <- list.files(dir_path, pattern = "\\.svg$", full.names = FALSE)
  pattern <- paste0("^", category, "_forest_(.+?)(?:_(?:mz_twins|all_pairs))?\\.svg$")
  outcomes <- sub(pattern, "\\1", grep(pattern, files, value = TRUE))
  sort(unique(outcomes))
}

find_mm_forest_svg <- function(analysis_set, category, outcome, scope, within_pair_type = NULL) {
  if (is.null(outcome) || is.null(scope)) return(NULL)
  if (identical(scope, "main")) {
    filename <- paste0(category, "_forest_", outcome, ".svg")
    target <- file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "main", filename)
  } else {
    suffix <- if (identical(within_pair_type, "mz_twins")) "mz_twins" else "all_pairs"
    filename <- paste0(category, "_forest_", outcome, "_", suffix, ".svg")
    target <- file.path(RESULTS_ROOT, "multimorbidity", analysis_set, category, "within_pair", filename)
  }
  if (file.exists(target)) return(normalizePath(target))
  NULL
}

# ============================================================================
# NOTES SYSTEM
# ============================================================================

read_text_file <- function(path) {
  tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"), error = function(e) NULL)
}

render_md_html <- function(text) {
  if (is.null(text)) return(NULL)
  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(commonmark::markdown_html(text))
  }
  if (requireNamespace("markdown", quietly = TRUE)) {
    tf_in <- tempfile(fileext = ".md")
    tf_out <- tempfile(fileext = ".html")
    writeLines(text, tf_in)
    markdown::markdownToHTML(file = tf_in, output = tf_out, options = c("smartypants"))
    return(read_text_file(tf_out))
  }
  paste0("<pre>", htmltools::htmlEscape(text), "</pre>")
}

read_note_if_exists <- function(rel_path) {
  if (!dir.exists(NOTES_ROOT)) return(NULL)
  target <- file.path(NOTES_ROOT, rel_path)
  if (file.exists(target)) return(read_text_file(target))
  NULL
}

build_note_candidates <- function(input) {
  candidates <- character(0)
  candidates <- c(candidates, file.path("analysis_set", paste0(input$analysis_set, ".md")))
  candidates <- c(candidates, file.path("analysis_type", paste0(input$analysis_type, ".md")))

  if (identical(input$analysis_type, "baseline")) {
    if (identical(input$subcategory, "current_health")) {
      if (identical(input$viz_type, "heatmap")) {
        if (!is.null(input$mortality_heatmap_variant)) {
          candidates <- c(candidates, file.path("baseline", "current_health", "heatmap", paste0(input$mortality_heatmap_variant, ".md")))
        }
        candidates <- c(candidates, file.path("baseline", "current_health", "heatmap", "_general.md"))
      } else if (identical(input$viz_type, "forest_table")) {
        if (!is.null(input$current_health_exposure)) {
          candidates <- c(candidates, file.path("baseline", "current_health", "forest_table", paste0(input$current_health_exposure, ".md")))
        }
        candidates <- c(candidates, file.path("baseline", "current_health", "forest_table", "_general.md"))
      } else if (identical(input$viz_type, "discordant")) {
        if (!is.null(input$discordant_exposure) && !is.null(input$discordant_pair_type)) {
          candidates <- c(candidates, file.path("baseline", "current_health", "discordant", paste0(input$discordant_exposure, "__", input$discordant_pair_type, ".md")))
        }
        if (!is.null(input$discordant_exposure)) {
          candidates <- c(candidates, file.path("baseline", "current_health", "discordant", paste0(input$discordant_exposure, ".md")))
        }
        candidates <- c(candidates, file.path("baseline", "current_health", "discordant", "_general.md"))
      }
    } else if (identical(input$subcategory, "prevalent_disease")) {
      if (identical(input$viz_type, "heatmap")) {
        if (!is.null(input$prevalent_disease_time_interval)) {
          candidates <- c(candidates, file.path("baseline", "prevalent_disease", "heatmap", paste0(input$prevalent_disease_time_interval, ".md")))
        }
        candidates <- c(candidates, file.path("baseline", "prevalent_disease", "heatmap", "_general.md"))
      } else if (identical(input$viz_type, "forest_table")) {
        if (!is.null(input$prevalent_disease_time_interval) && !is.null(input$prevalent_disease_disease)) {
          candidates <- c(candidates, file.path("baseline", "prevalent_disease", "forest_table", paste0(input$prevalent_disease_time_interval, "__", input$prevalent_disease_disease, ".md")))
        }
        candidates <- c(candidates, file.path("baseline", "prevalent_disease", "forest_table", "_general.md"))
      }
    }
  } else if (identical(input$analysis_type, "cox")) {
    if (identical(input$viz_type, "heatmap")) {
      candidates <- c(candidates, file.path("cox", "heatmap", "_general.md"))
    } else if (identical(input$viz_type, "forest_table")) {
      if (!is.null(input$outcome)) {
        candidates <- c(candidates, file.path("cox", "forest_table", paste0(input$outcome, ".md")))
      }
      candidates <- c(candidates, file.path("cox", "forest_table", "_general.md"))
    }
  }
  unique(candidates)
}

file_facts <- function(path) {
  inf <- file.info(path)
  md5 <- tryCatch(utils::md5sum(path)[[1]], error = function(e) NA_character_)
  list(path = normalizePath(path), size = inf$size, mtime = inf$mtime, md5 = md5)
}

# ============================================================================
# THEME CONFIGURATION
# ============================================================================

app_theme <- bs_theme(
  version = 5,
  preset = "cosmo",
  primary = "#3498db",
  secondary = "#95a5a6",
  success = "#27ae60",
  info = "#17a2b8",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_collection("Helvetica Neue", "Helvetica", "Arial", "sans-serif"),
  heading_font = font_collection("Helvetica Neue", "Helvetica", "Arial", "sans-serif"),
  font_scale = 0.95,
  "border-radius" = "0.5rem",
  "card-border-radius" = "0.75rem"
) |>
  bs_add_rules(
    "
    .navbar-brand {
      font-weight: 600;
      font-size: 1rem;
      text-align: center;
      width: 100%;
      margin: 0 auto;
    }
    .card-header {
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      border-bottom: 3px solid #3498db;
      font-weight: 600;
      color: #2c3e50;
    }
    .btn-primary {
      background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
      border-color: #2980b9;
      box-shadow: 0 2px 4px rgba(52, 152, 219, 0.3);
    }
    .btn-primary:hover {
      background: linear-gradient(135deg, #2980b9 0%, #21618c 100%);
      border-color: #21618c;
      box-shadow: 0 4px 8px rgba(52, 152, 219, 0.4);
      transform: translateY(-1px);
    }
    .nav-link.active {
      background: linear-gradient(135deg, #3498db 0%, #2980b9 100%) !important;
      color: white !important;
      font-weight: 600;
    }
    .card {
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      transition: box-shadow 0.3s ease;
    }
    .card:hover {
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }
    .sidebar {
      background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%);
    }
    
    /* Pill tab styling with outlines */
    .nav-pills .nav-link {
      border: 1.5px solid #3498db;
      color: #3498db;
      background-color: transparent;
      transition: all 0.2s ease;
      margin-right: 0.25rem;
      padding: 0.375rem 0.75rem;
    }
    
    .nav-pills .nav-link:hover {
      background-color: rgba(52, 152, 219, 0.1);
      border-color: #3498db;
    }
    
    .nav-pills .nav-link.active {
      background-color: #3498db;
      border-color: #3498db;
      color: white;
    }
    
    /* Compact spacing for option cards */
    .dynamic-card-body .control-label {
      margin-bottom: 0.25rem;
      font-size: 0.95rem;
      font-weight: 600;
    }
    
    .dynamic-card-body .radio {
      margin-bottom: 0.25rem;
      font-size: 0.95rem;
    }
    
    .dynamic-card-body .form-group {
      margin-bottom: 0.5rem;
    }
    
    .dynamic-card-body label {
      font-size: 0.95rem;
    }
    
    .dynamic-card-body .radio label {
      font-size: 0.95rem;
    }
    
    /* Consistent tight spacing between sidebar cards */
    .sidebar .card {
      margin-bottom: 0.75rem;
    }
    
    /* Indent sub-options for hierarchy */
    .within-pair-suboptions {
      margin-left: 1.5rem;
      margin-top: 0.5rem;
    }
    
    /* Reduce space after navset pills */
    .nav-pills {
      margin-bottom: 0.5rem;
    }
    
    /* Fix card tabs being cut off */
    .card .nav-tabs {
      overflow: visible;
      flex-wrap: wrap;
    }
    
    .card .nav-tabs .nav-link {
      white-space: normal;
      overflow: visible;
    }
    
    .card-header {
      overflow: visible;
    }
    
    .bslib-card .card-header {
      overflow: visible;
    }
    "
  )

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- page_sidebar(
  theme = app_theme,
  title = "Organ-specific proteomic aging predicts multimorbidity in twins",
  
  
  tags$head(
    tags$style(HTML("
      .selectize-dropdown {
        z-index: 10000 !important;
      }
      .sidebar {
        overflow-y: auto;
        overflow-x: visible;
      }
      .dynamic-card {
        transition: min-height 0.3s ease-in-out;
        overflow: visible !important;
      }
      .dynamic-card-body {
        overflow: visible !important;
        transition: min-height 0.3s ease-in-out;
      }
      .navbar-brand {
        text-align: center !important;
        width: 100% !important;
        display: block !important;
        font-size: 1rem !important;
      }
      .navbar {
        display: flex;
        justify-content: center;
      }
      #covariate_set_card {
        overflow: visible !important;
      }
      #covariate_set_card .card-body {
        overflow: visible !important;
      }
      #covariate_set_card .selectize-dropdown {
        z-index: 10001 !important;
      }
      /* Compact spacing for intro boxes and tabs */
      .navset-card-tab {
        margin-bottom: 0.5rem !important;
      }
      .card-header {
        padding: 0.5rem 0.75rem !important;
      }
      .bslib-card .card-header {
        padding: 0.5rem 0.75rem !important;
      }
      .navset-card-tab .nav-tabs {
        display: flex;
        flex-wrap: wrap;
        gap: 0.25rem 0.5rem;
      }
      .navset-card-tab .nav-tabs .nav-link {
        white-space: normal;
      }
      .navset-card-tab .nav-item {
        flex: 0 0 auto;
      }
      .navset-card-tab .nav-tabs {
        overflow-x: auto;
        overflow-y: visible;
      }
      .navset-tab .nav-tabs,
      .navset-pill .nav-tabs,
      .navset-card-tab .nav-tabs {
        flex-wrap: wrap;
        overflow-x: auto;
        overflow-y: visible;
      }
      .navset-tab .nav-item,
      .navset-pill .nav-item,
      .navset-card-tab .nav-item {
        flex: 0 0 auto;
      }
      .heatmap-gallery-grid {
        display: grid;
        grid-template-columns: 1fr;
        gap: 1rem;
      }
      .heatmap-gallery-body {
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .heatmap-gallery-img {
        width: 100%;
        height: auto;
        max-height: 28vh;
        object-fit: contain;
      }
      @media (min-width: 1200px) {
        .heatmap-gallery-grid {
          grid-template-columns: repeat(3, 1fr);
        }
        .heatmap-gallery-img {
          max-height: 55vh;
        }
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Store original heights
        var originalHeights = {};
        
        // Function to adjust card height based on dropdown state
        function adjustCardHeight(card, isOpen) {
          var $card = $(card);
          var $cardBody = $card.find('.card-body');
          var cardId = $card.attr('id') || 'card_' + Math.random().toString(36).substr(2, 9);
          
          if (isOpen) {
            // Store original height if not stored
            if (!originalHeights[cardId]) {
              originalHeights[cardId] = $cardBody.height();
            }
            
            // Calculate needed height: base height + space for dropdown
            var baseHeight = originalHeights[cardId] || $cardBody.height();
            var dropdownHeight = 250; // Space for dropdown options
            var newHeight = baseHeight + dropdownHeight;
            
            $cardBody.css('min-height', newHeight + 'px');
            $card.addClass('expanded');
          } else {
            // Reset to original height when closed
            if (originalHeights[cardId]) {
              $cardBody.css('min-height', originalHeights[cardId] + 'px');
            } else {
              $cardBody.css('min-height', '');
            }
            $card.removeClass('expanded');
          }
        }
        
        // Listen for selectize open/close events
        $(document).on('selectize:open', function(e) {
          var $card = $(e.target).closest('.dynamic-card');
          if ($card.length) {
            adjustCardHeight($card[0], true);
          }
        });
        
        $(document).on('selectize:close', function(e) {
          var $card = $(e.target).closest('.dynamic-card');
          if ($card.length) {
            // Small delay to ensure smooth transition
            setTimeout(function() {
              adjustCardHeight($card[0], false);
            }, 150);
          }
        });
        
        // Handle dynamically rendered selectInputs (from uiOutput)
        $(document).on('DOMNodeInserted', function(e) {
          if ($(e.target).find('.selectize-control').length) {
            var $card = $(e.target).closest('.dynamic-card');
            if ($card.length && !$card.hasClass('selectize-initialized')) {
              $card.addClass('selectize-initialized');
            }
          }
        });
      });
    "))
  ),
  
  sidebar = sidebar(
    width = 350,
    
    card(
      full_screen = FALSE,
      id = "analysis_config_card",
      class = "dynamic-card",
      card_header("Navigation"),
      card_body(
        class = "dynamic-card-body",
        style = "padding: 1rem;",
        tags$style(HTML("
          #analysis_type .radio:first-child {
            margin-bottom: 0.5rem;
          }
          .analysis_type_label {
            display: block;
            margin-top: 0.5rem;
            margin-bottom: 0.25rem;
            margin-left: 20px;
            font-weight: 600;
            font-size: 0.875rem;
            color: #212529;
          }
        ")),
        div(
          id = "analysis_type_container",
          radioButtons(
            "analysis_type",
            NULL,
            choices = c(
              "Study overview" = "landing",
              "Clock compositions" = "clock_proteins",
              "Baseline health" = "baseline",
              "Incident disease & mortality" = "cox",
              "Multimorbidity" = "multimorbidity"
            ),
            selected = "landing"
          ),
          tags$script(HTML("
            $(document).ready(function() {
              var $radios = $('#analysis_type .radio');
              if ($radios.length) {
                var resultsLabel = $('<div class=\"analysis_type_label\">Organ age associations</div>');
                if ($radios.length > 2) {
                  $radios.eq(2).before(resultsLabel);
                }
              }
            });
          "))
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.analysis_type == 'clock_proteins'",
      card(
        class = "dynamic-card",
        card_header("Clock protein options"),
        card_body(
          class = "dynamic-card-body",
          style = "padding: 0.75rem;",
          tags$label(class = "control-label mb-2 fw-bold", "View"),
          radioButtons(
            "clock_view_mode",
            NULL,
            choices = c("Gallery" = "gallery", "Focused" = "focused"),
            selected = "gallery"
          ),
          conditionalPanel(
            condition = "input.clock_view_mode == 'focused'",
            tags$label(class = "control-label mb-2 fw-bold", "Organ clock"),
            uiOutput("clock_focus_organ_selector")
          )
        )
      )
    ),
    
    tags$div(style = "display:none;",
             selectInput("subcategory", NULL,
                        choices = c("current_health", "prevalent_disease"),
                        selected = "current_health")),
    
    tags$div(style = "display:none;",
             selectInput("viz_type", NULL,
                        choices = c("heatmap", "forest_table", "discordant"),
                        selected = "heatmap")),
    
    conditionalPanel(
      condition = "input.analysis_type == 'cox'",
      card(
        class = "dynamic-card",
        card_header("Incident Disease Options"),
        card_body(
          class = "dynamic-card-body",
          style = "padding: 0.75rem;",
          tags$label(class = "control-label mb-2 fw-bold", "Visualization"),
          navset_pill(
            id = "cox_viz_tabs",
            nav_panel("Heatmap", value = "heatmap"),
            nav_panel("Forest table", value = "forest_table")
          ),
          conditionalPanel(
            condition = "input.cox_viz_tabs == 'forest_table' || (input.cox_viz_tabs == 'heatmap' && input.cox_heatmap_view != 'gallery')",
            tags$label(class = "control-label mb-2 fw-bold", "Scope"),
            radioButtons(
              "cox_scope",
              NULL,
              choices = c("Cohort-level" = "main", "Within-pair" = "within_pair"),
              selected = "main"
            )
          ),
          conditionalPanel(
            condition = "input.cox_scope == 'within_pair' && (input.cox_viz_tabs == 'forest_table' || (input.cox_viz_tabs == 'heatmap' && input.cox_heatmap_view != 'gallery'))",
            div(
              class = "within-pair-suboptions",
              radioButtons(
                "cox_within_pair_type",
                NULL,
                choices = c("All twin pairs" = "all_pairs", "Monozygotic twins" = "mz_twins"),
                selected = "all_pairs"
              )
            )
          ),
          conditionalPanel(
            condition = "input.cox_viz_tabs == 'forest_table'",
            uiOutput("outcome_selector")
          ),
          conditionalPanel(
            condition = "input.cox_viz_tabs == 'heatmap'",
            tags$label(class = "control-label mb-2 fw-bold", "View"),
            radioButtons(
              "cox_heatmap_view",
              NULL,
              choices = c("Single heatmap" = "single", "Gallery" = "gallery"),
              selected = "single"
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.analysis_type == 'multimorbidity'",
      card(
        class = "dynamic-card",
        card_header("Multimorbidity Options"),
        card_body(
          class = "dynamic-card-body",
          style = "padding: 0.75rem;",
          tags$label(class = "control-label mb-2 fw-bold", "Visualization"),
          navset_pill(
            id = "mm_viz_tabs",
            nav_panel("Heatmap", value = "heatmap"),
            nav_panel("Forest table", value = "forest_table")
          ),
          conditionalPanel(
            condition = "input.mm_viz_tabs == 'forest_table' || (input.mm_viz_tabs == 'heatmap' && input.mm_heatmap_view != 'gallery')",
            tags$label(class = "control-label mb-2 fw-bold", "Scope"),
            radioButtons(
              "mm_scope",
              NULL,
              choices = c("Cohort-level" = "main", "Within-pair" = "within_pair"),
              selected = "main"
            )
          ),
          conditionalPanel(
            condition = "input.mm_scope == 'within_pair' && (input.mm_viz_tabs == 'forest_table' || (input.mm_viz_tabs == 'heatmap' && input.mm_heatmap_view != 'gallery'))",
            div(
              class = "within-pair-suboptions",
              radioButtons(
                "mm_within_pair_type",
                NULL,
                choices = c("All twin pairs" = "all_pairs", "Monozygotic twins" = "mz_twins"),
                selected = "all_pairs"
              )
            )
          ),
          conditionalPanel(
            condition = "input.mm_viz_tabs == 'forest_table'",
            uiOutput("mm_outcome_selector")
          ),
          conditionalPanel(
            condition = "input.mm_viz_tabs == 'heatmap'",
            tags$label(class = "control-label mb-2 fw-bold", "View"),
            radioButtons(
              "mm_heatmap_view",
              NULL,
              choices = c("Single heatmap" = "single", "Gallery" = "gallery"),
              selected = "single"
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.analysis_type == 'baseline' && input.subcategory == 'current_health'",
      card(
        class = "dynamic-card",
        card_header("Baseline Health Options"),
        card_body(
          class = "dynamic-card-body",
          style = "padding: 0.75rem;",
          tags$label(class = "control-label mb-2 fw-bold", "Visualization"),
          navset_pill(
            id = "baseline_ch_viz_tabs",
            nav_panel("Heatmap", value = "heatmap"),
            nav_panel("Forest table", value = "forest_table")
          ),
          conditionalPanel(
            condition = "input.baseline_ch_viz_tabs == 'heatmap' && (input.baseline_ch_heatmap_view != 'gallery') && input.baseline_ch_scope == 'main'",
            uiOutput("mortality_heatmap_selector")
          ),
          conditionalPanel(
            condition = "input.baseline_ch_viz_tabs == 'heatmap' && (input.baseline_ch_heatmap_view != 'gallery') && input.baseline_ch_scope == 'within_pair'",
            uiOutput("within_pair_heatmap_selector")
          ),
          conditionalPanel(
            condition = "input.baseline_ch_viz_tabs == 'forest_table'",
            uiOutput("current_health_exposure_selector")
          ),
          conditionalPanel(
            condition = "input.baseline_ch_viz_tabs == 'forest_table' || (input.baseline_ch_viz_tabs == 'heatmap' && input.baseline_ch_heatmap_view != 'gallery')",
            tags$label(class = "control-label mb-2 fw-bold", "Scope"),
            radioButtons(
              "baseline_ch_scope",
              NULL,
              choices = c("Cohort-level" = "main", "Within-pair" = "within_pair"),
              selected = "main"
            )
          ),
          conditionalPanel(
            condition = "input.baseline_ch_scope == 'within_pair' && (input.baseline_ch_viz_tabs == 'forest_table' || (input.baseline_ch_viz_tabs == 'heatmap' && input.baseline_ch_heatmap_view != 'gallery'))",
            div(
              class = "within-pair-suboptions",
              radioButtons(
                "baseline_ch_within_pair_type",
                NULL,
                choices = c("All twin pairs" = "all_pairs", "Monozygotic twins" = "mz_twins"),
                selected = "all_pairs"
              )
            )
          ),
          conditionalPanel(
            condition = "input.baseline_ch_viz_tabs == 'heatmap'",
            tags$label(class = "control-label mb-2 fw-bold", "View"),
            radioButtons(
              "baseline_ch_heatmap_view",
              NULL,
              choices = c("Single heatmap" = "single", "Gallery" = "gallery"),
              selected = "single"
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.analysis_type == 'baseline' && input.subcategory == 'prevalent_disease'",
      card(
        class = "dynamic-card",
        card_header("Prior Disease Options"),
        card_body(
          class = "dynamic-card-body",
          style = "padding: 0.75rem;",
          tags$label(class = "control-label mb-2 fw-bold", "Visualization"),
          navset_pill(
            id = "baseline_pd_viz_tabs",
            nav_panel("Heatmap", value = "heatmap"),
            nav_panel("Forest table", value = "forest_table")
          ),
          conditionalPanel(
            condition = "input.baseline_pd_viz_tabs == 'forest_table' || (input.baseline_pd_viz_tabs == 'heatmap' && input.baseline_pd_heatmap_view != 'gallery')",
            tags$label(class = "control-label mb-2 fw-bold", "Scope"),
            radioButtons(
              "prevalent_disease_scope",
              NULL,
              choices = c("Cohort-level" = "main", "Within-pair" = "within"),
              selected = "main"
            )
          ),
          conditionalPanel(
            condition = "input.prevalent_disease_scope == 'within' && (input.baseline_pd_viz_tabs == 'forest_table' || (input.baseline_pd_viz_tabs == 'heatmap' && input.baseline_pd_heatmap_view != 'gallery'))",
            div(
              class = "within-pair-suboptions",
              radioButtons(
                "prevalent_within_pair_type",
                NULL,
                choices = c("All twin pairs" = "all_pairs", "Monozygotic twins" = "mz_twins"),
                selected = "all_pairs"
              )
            )
          ),
          uiOutput("prevalent_disease_time_selector"),
          conditionalPanel(
            condition = "input.baseline_pd_viz_tabs == 'forest_table'",
            uiOutput("prevalent_disease_forest_selector")
          ),
          conditionalPanel(
            condition = "input.baseline_pd_viz_tabs == 'heatmap'",
            tags$label(class = "control-label mb-2 fw-bold", "View"),
            radioButtons(
              "baseline_pd_heatmap_view",
              NULL,
              choices = c("Single heatmap" = "single", "Gallery" = "gallery"),
              selected = "single"
            )
          )
        )
      )
    ),
    
    card(
      id = "covariate_set_card",
      card_header("Covariate set"),
      card_body(
        style = "padding: 0.75rem; overflow: visible; position: relative; z-index: 1;",
        selectInput(
          "analysis_set",
          NULL,
          choices = c(
            "Multivariable" = "core",
            "Minimally adjusted" = "base",
            "Multivariable + eGFR" = "extended"
          ),
          selected = "core",
          width = "100%"
        )
      )
    ),
    
  ),
  
  uiOutput("main_content")
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Tab synchronization logic
  observeEvent(input$baseline_domain_tabs, ignoreInit = TRUE, {
    updateSelectInput(session, "subcategory", selected = input$baseline_domain_tabs)
  })
  
  observeEvent(input$subcategory, ignoreInit = TRUE, {
    if (!identical(input$baseline_domain_tabs, input$subcategory)) {
      updateTabsetPanel(session, "baseline_domain_tabs", selected = input$subcategory)
    }
  })
  
  observeEvent(input$mm_category_tabs, ignoreInit = TRUE, {
    if (!is.null(input$mm_category_tabs)) {
      # This will be used to determine which multimorbidity category to show
    }
  })
  
  # Synchronize visualization pill tabs with viz_type
  observeEvent(input$cox_viz_tabs, {
    updateSelectInput(session, "viz_type", selected = input$cox_viz_tabs)
  }, ignoreInit = TRUE)
  
  observeEvent(input$mm_viz_tabs, {
    updateSelectInput(session, "viz_type", selected = input$mm_viz_tabs)
  }, ignoreInit = TRUE)
  
  observeEvent(input$baseline_ch_viz_tabs, {
    updateSelectInput(session, "viz_type", selected = input$baseline_ch_viz_tabs)
  }, ignoreInit = TRUE)
  
  observeEvent(input$baseline_pd_viz_tabs, {
    updateSelectInput(session, "viz_type", selected = input$baseline_pd_viz_tabs)
  }, ignoreInit = TRUE)
  
  observeEvent(input$go_to_landing, {
    updateSelectInput(session, "analysis_type", selected = "landing")
  })
  
  
  observeEvent(input$prevalent_within_pair_type, {
    if (!is.null(input$prevalent_within_pair_type) && 
        input$prevalent_within_pair_type == "mz_twins" &&
        input$analysis_type == "baseline" &&
        input$subcategory == "prevalent_disease") {
      showNotification(
        "Switching to MZ pairs: Only diseases with sufficient cases will be available.",
        type = "warning",
        duration = 5
      )
    }
  }, ignoreInit = TRUE)
  
  # Update within-pair heatmap selector when pair type changes
  observeEvent(input$baseline_ch_within_pair_type, {
    if (input$analysis_type == "baseline" && 
        input$subcategory == "current_health" && 
        input$baseline_ch_viz_tabs == "heatmap" &&
        input$baseline_ch_scope == "within_pair") {
      # Trigger re-render of within_pair_heatmap_selector
      # The selector will automatically update because it's reactive to input$baseline_ch_within_pair_type
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$analysis_type, {
    if (identical(input$analysis_type, "multimorbidity")) {
      if (is.null(input$mm_category_tabs) || !input$mm_category_tabs %in% c("ag", "nb")) {
        updateTabsetPanel(session, "mm_category_tabs", selected = "ag")
      }
      if (is.null(input$mm_scope) || !input$mm_scope %in% c("main", "within_pair")) {
        updateRadioButtons(session, "mm_scope", selected = "main")
      }
      if (is.null(input$mm_within_pair_type) || !input$mm_within_pair_type %in% c("all_pairs", "mz_twins")) {
        updateRadioButtons(session, "mm_within_pair_type", selected = "all_pairs")
      }
      if (is.null(input$mm_viz_tabs) || !input$mm_viz_tabs %in% c("heatmap", "forest_table")) {
        updateNavsetPanel(session, "mm_viz_tabs", selected = "heatmap")
      }
    }
  }, ignoreInit = FALSE, priority = 10)
  
  # Dynamic UI: Selectors
  output$outcome_selector <- renderUI({
    if (is.null(input$analysis_type) || is.null(input$analysis_set)) return(NULL)
    if (input$analysis_type == "baseline" && is.null(input$subcategory)) return(NULL)
    
    subcat <- if (input$analysis_type == "baseline") input$subcategory else NULL
    outcomes <- discover_outcomes(input$analysis_type, input$analysis_set, subcat)
    if (!length(outcomes)) {
      return(div(class = "alert alert-info", "No outcomes found for this analysis set."))
    }
    selectInput("outcome", "Outcome", choices = create_display_choices(outcomes), selected = outcomes[1])
  })
  
  output$mortality_heatmap_selector <- renderUI({
    variants <- discover_mortality_heatmap_variants(input$analysis_set)
    if (!length(variants)) {
      return(div(class = "alert alert-info", "No heatmap variants found."))
    }
    # Try to preserve selection from within_pair if it exists, otherwise use current or first
    selected <- if (!is.null(input$within_pair_heatmap_variant) && 
                     input$within_pair_heatmap_variant %in% variants) {
      input$within_pair_heatmap_variant
    } else if (!is.null(input$mortality_heatmap_variant) && 
               input$mortality_heatmap_variant %in% variants) {
      input$mortality_heatmap_variant
    } else {
      variants[1]
    }
    selectInput("mortality_heatmap_variant", "Markers",
               choices = create_display_choices(variants), selected = selected)
  })
  
  output$within_pair_heatmap_selector <- renderUI({
    pair_type <- if (is.null(input$baseline_ch_within_pair_type)) "all_pairs" else input$baseline_ch_within_pair_type
    variants <- discover_within_pair_heatmap_variants(input$analysis_set, pair_type)
    if (!length(variants)) {
      return(div(class = "alert alert-info", "No within-pair heatmap variants found."))
    }
    # Try to preserve selection from main scope if it exists, otherwise use current or first
    selected <- if (!is.null(input$mortality_heatmap_variant) && 
                     input$mortality_heatmap_variant %in% variants) {
      input$mortality_heatmap_variant
    } else if (!is.null(input$within_pair_heatmap_variant) && 
               input$within_pair_heatmap_variant %in% variants) {
      input$within_pair_heatmap_variant
    } else {
      variants[1]
    }
    selectInput("within_pair_heatmap_variant", "Markers",
               choices = create_display_choices(variants), selected = selected)
  })
  
  output$current_health_exposure_selector <- renderUI({
    scope <- if (is.null(input$baseline_ch_scope)) "main" else input$baseline_ch_scope
    
    if (scope == "within_pair" && input$baseline_ch_viz_tabs == "forest_table") {
      pair_type <- if (is.null(input$baseline_ch_within_pair_type)) "all_pairs" else input$baseline_ch_within_pair_type
      exposures <- discover_within_pair_forest_files(input$analysis_set, pair_type)
    } else {
      exposures <- discover_current_health_forest_files(input$analysis_set)
    }
    
    if (!length(exposures)) {
      return(div(class = "alert alert-info", "No exposures found."))
    }
    selectInput("current_health_exposure", "Exposure",
               choices = create_display_choices(exposures), selected = exposures[1])
  })
  
  # Sex-stratified selector (removed from UI but kept for future re-implementation)
  # output$sex_stratified_sex_selector <- renderUI({
  #   sex_files <- discover_sex_stratified_files(input$analysis_set,
  #                                              if (input$viz_type == "heatmap") "heatmap" else "forest_table")
  #   if (!length(sex_files$sex)) {
  #     return(div(class = "alert alert-info", "No sex-stratified files found."))
  #   }
  #   selectInput("sex_stratified_sex", "Sex",
  #              choices = create_display_choices(sex_files$sex), selected = sex_files$sex[1])
  # })
  
  output$prevalent_disease_time_selector <- renderUI({
    time_intervals <- if (input$prevalent_disease_scope == "main") {
      discover_prevalent_disease_time_intervals(input$analysis_set)
    } else {
      discover_prevalent_disease_within_time_intervals(input$analysis_set)
    }
    if (!length(time_intervals)) {
      return(div(class = "alert alert-info", "No time intervals found."))
    }
    selectInput("prevalent_disease_time_interval", "Look-back window",
               choices = create_display_choices(time_intervals), selected = time_intervals[1])
  })
  
  output$prevalent_disease_forest_selector <- renderUI({
    if (is.null(input$prevalent_disease_time_interval)) return(NULL)
    diseases <- if (input$prevalent_disease_scope == "main") {
      discover_prevalent_disease_diseases(input$analysis_set, input$prevalent_disease_time_interval)
    } else {
      pair_type <- if (is.null(input$prevalent_within_pair_type)) "all_pairs" else input$prevalent_within_pair_type
      discover_prevalent_disease_within_diseases(input$analysis_set, input$prevalent_disease_time_interval, pair_type)
    }
    
    if (!length(diseases)) {
      msg <- if (input$prevalent_disease_scope == "within" && 
                 !is.null(input$prevalent_within_pair_type) && 
                 input$prevalent_within_pair_type == "mz_twins") {
        "No diseases available for MZ pairs analysis (insufficient cases)."
      } else {
        "No diseases found."
      }
      return(div(class = "alert alert-warning", msg))
    }
    
    selected_disease <- input$prevalent_disease_disease
    if (!is.null(selected_disease) && selected_disease %in% diseases) {
      selected <- selected_disease
    } else {
      selected <- diseases[1]
    }
    
    tagList(
      if (input$prevalent_disease_scope == "within" && 
          !is.null(input$prevalent_within_pair_type) && 
          input$prevalent_within_pair_type == "mz_twins") {
        div(class = "alert alert-info mb-2", 
            tags$small("Note: Only diseases with sufficient MZ twin cases are shown."))
      },
      selectInput("prevalent_disease_disease", "Disease",
                 choices = create_display_choices(diseases), selected = selected)
    )
  })
  
  output$mm_outcome_selector <- renderUI({
    mm_cat <- if (!is.null(input$mm_category_tabs)) input$mm_category_tabs else "ag"
    scope <- if (is.null(input$mm_scope)) "main" else input$mm_scope
    outcomes <- discover_mm_outcomes(input$analysis_set, mm_cat, scope)
    if (!length(outcomes)) {
      return(div(class = "alert alert-info", "No outcomes found."))
    }
    selectInput("mm_outcome", "Outcome", choices = create_display_choices(outcomes), selected = outcomes[1])
  })
  
  # Visualization: Heatmaps
  current_heatmap_path <- reactive({
    svg_path <- NULL
    
    if (input$analysis_type == "cox") {
      if (is.null(input$cox_scope)) return(NULL)
      scope <- input$cox_scope
      within_pair_type <- if (scope == "within_pair") input$cox_within_pair_type else NULL
      svg_path <- find_heatmap_svg(input$analysis_type, input$analysis_set, NULL, scope, within_pair_type)
    
    } else if (input$analysis_type == "multimorbidity") {
      mm_cat <- if (!is.null(input$mm_category_tabs)) input$mm_category_tabs else "ag"
      scope <- if (is.null(input$mm_scope)) "main" else input$mm_scope
      within_type <- if (scope == "within_pair") {
        if (is.null(input$mm_within_pair_type)) "all_pairs" else input$mm_within_pair_type
      } else {
        NULL
      }
      filename <- resolve_mm_filename(input$analysis_set, mm_cat, scope, within_type)
      if (!is.null(filename)) {
        svg_path <- find_multimorbidity_svg(input$analysis_set, mm_cat, filename, scope)
      }
      
    } else if (input$analysis_type == "baseline" && input$subcategory == "current_health" && input$viz_type == "heatmap") {
      scope <- if (is.null(input$baseline_ch_scope)) "main" else input$baseline_ch_scope
      
      # Sex-stratified option removed (can be re-implemented later)
      # if (isTRUE(input$sex_stratified_toggle)) {
      #   if (is.null(input$current_health_exposure) || is.null(input$sex_stratified_sex)) return(NULL)
      #   svg_path <- find_sex_stratified_svg(input$analysis_set, "heatmap", input$current_health_exposure, input$sex_stratified_sex)
      # } else 
      
      if (scope == "within_pair") {
        if (is.null(input$within_pair_heatmap_variant)) return(NULL)
        pair_type <- if (is.null(input$baseline_ch_within_pair_type)) "all_pairs" else input$baseline_ch_within_pair_type
        svg_path <- find_within_pair_heatmap_svg(input$analysis_set, input$within_pair_heatmap_variant, pair_type)
      } else {
        if (is.null(input$mortality_heatmap_variant)) return(NULL)
        svg_path <- find_mortality_heatmap_svg(input$analysis_set, input$mortality_heatmap_variant)
      }
      
    } else if (input$analysis_type == "baseline" && input$subcategory == "prevalent_disease" && input$viz_type == "heatmap") {
      if (is.null(input$prevalent_disease_time_interval)) return(NULL)
      if (input$prevalent_disease_scope == "main") {
        svg_path <- find_prevalent_disease_heatmap_svg(input$analysis_set, input$prevalent_disease_time_interval)
      } else {
        pair_type <- if (is.null(input$prevalent_within_pair_type)) "all_pairs" else input$prevalent_within_pair_type
        svg_path <- find_prevalent_disease_within_heatmap_svg(input$analysis_set, input$prevalent_disease_time_interval, pair_type)
      }
    }
    
    svg_path
  })
  
  output$heatmap_ui <- renderUI({
    if (input$analysis_type == "cox" && input$cox_viz_tabs == "heatmap" &&
        !is.null(input$cox_heatmap_view) && input$cox_heatmap_view == "gallery") {
      gallery_specs <- list(
        list(label = "Cohort", scope = "main", within = NULL),
        list(label = "Within all twin pairs", scope = "within_pair", within = "all_pairs"),
        list(label = "Within monozygotic twin pairs", scope = "within_pair", within = "mz_twins")
      )
      
      cards <- lapply(gallery_specs, function(spec) {
        svg_path <- find_heatmap_svg("cox", input$analysis_set, NULL, spec$scope, spec$within)
        if (is.null(svg_path)) {
          return(
            card(
              card_header(spec$label),
              card_body(
                div(class = "alert alert-warning", "Heatmap not available.")
              )
            )
          )
        }
        rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
        card(
          card_header(spec$label),
          card_body(
            class = "heatmap-gallery-body",
            tags$img(src = rel_path, class = "heatmap-gallery-img")
          )
        )
      })
      
      return(div(class = "heatmap-gallery-grid", cards))
    }
    
    if (input$analysis_type == "multimorbidity" && input$mm_viz_tabs == "heatmap" &&
        !is.null(input$mm_heatmap_view) && input$mm_heatmap_view == "gallery") {
      mm_cat <- if (!is.null(input$mm_category_tabs)) input$mm_category_tabs else "ag"
      gallery_specs <- list(
        list(label = "Cohort", scope = "main", within = NULL),
        list(label = "Within all twin pairs", scope = "within_pair", within = "all_pairs"),
        list(label = "Within monozygotic twin pairs", scope = "within_pair", within = "mz_twins")
      )
      
      cards <- lapply(gallery_specs, function(spec) {
        filename <- resolve_mm_filename(input$analysis_set, mm_cat, spec$scope, spec$within)
        svg_path <- if (!is.null(filename)) {
          find_multimorbidity_svg(input$analysis_set, mm_cat, filename, spec$scope)
        } else {
          NULL
        }
        if (is.null(svg_path)) {
          return(
            card(
              card_header(spec$label),
              card_body(
                div(class = "alert alert-warning", "Heatmap not available.")
              )
            )
          )
        }
        rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
        card(
          card_header(spec$label),
          card_body(
            class = "heatmap-gallery-body",
            tags$img(src = rel_path, class = "heatmap-gallery-img")
          )
        )
      })
      
      return(div(class = "heatmap-gallery-grid", cards))
    }
    
    if (input$analysis_type == "baseline" && input$subcategory == "current_health" &&
        input$baseline_ch_viz_tabs == "heatmap" &&
        !is.null(input$baseline_ch_heatmap_view) && input$baseline_ch_heatmap_view == "gallery") {
      if (is.null(input$mortality_heatmap_variant)) {
        return(div(class = "alert alert-info", "Select a heatmap variant to view the gallery."))
      }
      variant <- input$mortality_heatmap_variant
      gallery_specs <- list(
        list(label = "Cohort", scope = "main", within = NULL),
        list(label = "Within all twin pairs", scope = "within_pair", within = "all_pairs"),
        list(label = "Within monozygotic twin pairs", scope = "within_pair", within = "mz_twins")
      )
      
      cards <- lapply(gallery_specs, function(spec) {
        svg_path <- if (spec$scope == "main") {
          find_mortality_heatmap_svg(input$analysis_set, variant)
        } else {
          find_within_pair_heatmap_svg(input$analysis_set, variant, spec$within)
        }
        if (is.null(svg_path)) {
          return(
            card(
              card_header(spec$label),
              card_body(
                div(class = "alert alert-warning", "Heatmap not available.")
              )
            )
          )
        }
        rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
        card(
          card_header(spec$label),
          card_body(
            class = "heatmap-gallery-body",
            tags$img(src = rel_path, class = "heatmap-gallery-img")
          )
        )
      })
      
      return(div(class = "heatmap-gallery-grid", cards))
    }
    
    if (input$analysis_type == "baseline" && input$subcategory == "prevalent_disease" &&
        input$baseline_pd_viz_tabs == "heatmap" &&
        !is.null(input$baseline_pd_heatmap_view) && input$baseline_pd_heatmap_view == "gallery") {
      if (is.null(input$prevalent_disease_time_interval)) {
        return(div(class = "alert alert-info", "Select a time window to view the gallery."))
      }
      time_interval <- input$prevalent_disease_time_interval
      gallery_specs <- list(
        list(label = "Cohort", scope = "main", within = NULL),
        list(label = "Within all twin pairs", scope = "within", within = "all_pairs"),
        list(label = "Within monozygotic twin pairs", scope = "within", within = "mz_twins")
      )
      
      cards <- lapply(gallery_specs, function(spec) {
        svg_path <- if (spec$scope == "main") {
          find_prevalent_disease_heatmap_svg(input$analysis_set, time_interval)
        } else {
          find_prevalent_disease_within_heatmap_svg(input$analysis_set, time_interval, spec$within)
        }
        if (is.null(svg_path)) {
          return(
            card(
              card_header(spec$label),
              card_body(
                div(class = "alert alert-warning", "Heatmap not available.")
              )
            )
          )
        }
        rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
        card(
          card_header(spec$label),
          card_body(
            class = "heatmap-gallery-body",
            tags$img(src = rel_path, class = "heatmap-gallery-img")
          )
        )
      })
      
      return(div(class = "heatmap-gallery-grid", cards))
    }
    
    svg_path <- current_heatmap_path()
    if (is.null(svg_path)) {
      msg <- if (input$analysis_type == "baseline" && 
                 input$subcategory == "prevalent_disease" &&
                 input$prevalent_disease_scope == "within" &&
                 !is.null(input$prevalent_within_pair_type) &&
                 input$prevalent_within_pair_type == "mz_twins") {
        "Heatmap not available for MZ pairs (insufficient cases for this time interval)."
      } else {
        "Heatmap not available."
      }
      return(div(class = "alert alert-warning", style = "margin: 2rem;", msg))
    }
    
    # Convert absolute path to relative path from www directory
    # Since files are in www/plots, extract path relative to www/
    rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
    tags$img(src = rel_path, style = "width: 100%; height: auto; max-height: 600px;")
  })
  
  output$baseline_health_heatmap_caption <- renderUI({
    if (!(input$analysis_type == "baseline" && 
          input$subcategory == "current_health" && 
          input$baseline_ch_viz_tabs == "heatmap")) {
      return(NULL)
    }
    
    scope <- if (is.null(input$baseline_ch_scope)) "main" else input$baseline_ch_scope
    
    variant <- if (scope == "within_pair") {
      if (is.null(input$within_pair_heatmap_variant)) return(NULL)
      input$within_pair_heatmap_variant
    } else {
      if (is.null(input$mortality_heatmap_variant)) return(NULL)
      input$mortality_heatmap_variant
    }
    
    if (is.null(variant) || is.null(input$analysis_set)) return(NULL)
    
    covariate_set_name <- get_covariate_set_display_name(input$analysis_set)
    
    if (!is.null(input$baseline_ch_heatmap_view) && input$baseline_ch_heatmap_view == "gallery") {
      return(
        div(
          class = "text-muted small mt-3",
          style = "line-height: 1.6;",
          tags$p(
            class = "mb-0",
            "Gallery showing cohort, all twin pairs, and monozygotic twin pairs heatmaps ",
            "for ", tags$strong(format_display_name(variant)), " markers. ",
            "Each row represents an outcome, each column an organ age. ",
            "Effect sizes are standardized and reflect a 1 SD increase in organ age. ",
            "Non-significant associations at FDR < 0.05 are marked by crosses. ",
            "Models use the ", tags$strong(covariate_set_name), " covariate set."
          )
        )
      )
    }
    
    scope_text <- if (scope == "main") {
      "Cohort analysis"
    } else if (scope == "within_pair") {
      pair_type <- if (is.null(input$baseline_ch_within_pair_type)) "all_pairs" else input$baseline_ch_within_pair_type
      if (pair_type == "mz_twins") {
        "Within monozygotic twin pairs"
      } else {
        "Within all twin pairs"
      }
    } else {
      ""
    }
    
    variant_display <- format_display_name(variant)
    
    div(
      class = "text-muted small mt-3",
      style = "line-height: 1.6;",
      tags$p(
        class = "mb-0",
        "Heatmap showing associations between each organ age and ", 
        tags$strong(variant_display), 
        " markers from linear (continuous outcome) and logistic (binary outcome) regression. ",
        "Each row represents an outcome, each column an organ age. ",
        "Effect sizes are standardized and reflect a 1 SD increase in organ age. ",
        "Non-significant associations at FDR < 0.05 are marked by crosses. ",
        "Models use the ", tags$strong(covariate_set_name), " covariate set. ",
        scope_text, "."
      )
    )
  })
  
  output$prevalent_disease_heatmap_caption <- renderUI({
    if (!(input$analysis_type == "baseline" && 
          input$subcategory == "prevalent_disease" && 
          input$baseline_pd_viz_tabs == "heatmap")) {
      return(NULL)
    }
    
    if (is.null(input$prevalent_disease_time_interval) || is.null(input$analysis_set)) return(NULL)
    
    scope <- if (is.null(input$prevalent_disease_scope)) "main" else input$prevalent_disease_scope
    
    covariate_set_name <- get_covariate_set_display_name(input$analysis_set)
    
    time_window_display <- format_display_name(input$prevalent_disease_time_interval)
    
    if (!is.null(input$baseline_pd_heatmap_view) && input$baseline_pd_heatmap_view == "gallery") {
      return(
        div(
          class = "text-muted small mt-3",
          style = "line-height: 1.6;",
          tags$p(
            class = "mb-0",
            "Gallery showing cohort, all twin pairs, and monozygotic twin pairs heatmaps ",
            "for prior disease at baseline. ",
            "Each row represents a disease, each column an organ age. ",
            "Effect sizes are displayed as log(OR) and reflect a 1 SD increase in organ age. ",
            "Non-significant associations at FDR < 0.05 are marked by crosses. ",
            "Models use the ", tags$strong(covariate_set_name), " covariate set. ",
            "Look-back window: ", tags$strong(time_window_display), "."
          )
        )
      )
    }
    
    scope_text <- if (scope == "main") {
      "Cohort analysis"
    } else if (scope == "within") {
      pair_type <- if (is.null(input$prevalent_within_pair_type)) "all_pairs" else input$prevalent_within_pair_type
      if (pair_type == "mz_twins") {
        "Within monozygotic twin pairs"
      } else {
        "Within all twin pairs"
      }
    } else {
      ""
    }
    
    div(
      class = "text-muted small mt-3",
      style = "line-height: 1.6;",
      tags$p(
        class = "mb-0",
        "Heatmap showing associations between each organ age and prior disease at baseline using logistic regression. ",
        "Each row represents a disease, each column an organ age. ",
        "Effect sizes are displayed as log(OR) and reflect a 1 SD increase in organ age. ",
        "Non-significant associations at FDR < 0.05 are marked by crosses. ",
        "Models use the ", tags$strong(covariate_set_name), " covariate set. ",
        "Look-back window: ", tags$strong(time_window_display), ". ",
        scope_text, "."
      )
    )
  })
  
  output$cox_heatmap_caption <- renderUI({
    if (!(input$analysis_type == "cox" && input$cox_viz_tabs == "heatmap")) {
      return(NULL)
    }
    
    if (is.null(input$analysis_set)) return(NULL)
    
    scope <- if (is.null(input$cox_scope)) "main" else input$cox_scope
    
    covariate_set_name <- get_covariate_set_display_name(input$analysis_set)
    
    if (!is.null(input$cox_heatmap_view) && input$cox_heatmap_view == "gallery") {
      return(
        div(
          class = "text-muted small mt-3",
          style = "line-height: 1.6;",
          tags$p(
            class = "mb-0",
            "Gallery showing cohort, all twin pairs, and monozygotic twin pairs heatmaps ",
            "from Cox proportional hazards models. ",
            "Colors indicate log hazard ratios per 1 SD higher organ age, adjusted for the ",
            tags$strong(covariate_set_name), " covariate set. ",
            "Non-significant associations at FDR < 0.05 are marked by crosses."
          )
        )
      )
    }
    
    scope_text <- if (scope == "main") {
      "Cohort analysis"
    } else if (scope == "within_pair") {
      pair_type <- if (is.null(input$cox_within_pair_type)) "all_pairs" else input$cox_within_pair_type
      if (pair_type == "mz_twins") {
        "Within monozygotic twin pairs"
      } else {
        "Within all twin pairs"
      }
    } else {
      ""
    }
    
    div(
      class = "text-muted small mt-3",
      style = "line-height: 1.6;",
      tags$p(
        class = "mb-0",
        "Heatmap showing associations between each organ age and risk of incident major diseases ",
        "from Cox proportional hazards models. ",
        "Each row represents an outcome, each column an organ age. ",
        "Colors indicate log hazard ratios per 1 SD higher organ age, adjusted for the ", 
        tags$strong(covariate_set_name), " covariate set. ",
        "Non-significant associations at FDR < 0.05 are marked by crosses. ",
        scope_text, "."
      )
    )
  })
  
  output$multimorbidity_heatmap_caption <- renderUI({
    if (!(input$analysis_type == "multimorbidity" && input$mm_viz_tabs == "heatmap")) {
      return(NULL)
    }
    
    if (is.null(input$analysis_set) || is.null(input$mm_category_tabs)) return(NULL)
    
    mm_cat <- input$mm_category_tabs
    scope <- if (is.null(input$mm_scope)) "main" else input$mm_scope
    
    covariate_set_name <- get_covariate_set_display_name(input$analysis_set)
    
    if (!is.null(input$mm_heatmap_view) && input$mm_heatmap_view == "gallery") {
      return(
        div(
          class = "text-muted small mt-3",
          style = "line-height: 1.6;",
          tags$p(
            class = "mb-0",
            "Gallery showing cohort, all twin pairs, and monozygotic twin pairs heatmaps ",
            "for multimorbidity within disease groupings. ",
            "Each row represents a disease grouping, each column an organ age. ",
            "Effect sizes are expressed as log incidence rate ratio (IRR) or log hazard ratio (HR) per 1 SD increase in organ age. ",
            "Non-significant associations at FDR < 0.05 are marked by crosses. ",
            "Models use the ", tags$strong(covariate_set_name), " covariate set."
          )
        )
      )
    }
    
    scope_text <- if (scope == "main") {
      "Cohort analysis"
    } else if (scope == "within_pair") {
      pair_type <- if (is.null(input$mm_within_pair_type)) "all_pairs" else input$mm_within_pair_type
      if (pair_type == "mz_twins") {
        "Within monozygotic twin pairs"
      } else {
        "Within all twin pairs"
      }
    } else {
      ""
    }
    
    if (mm_cat == "nb") {
      div(
        class = "text-muted small mt-3",
        style = "line-height: 1.6;",
        tags$p(
          class = "mb-0",
          "Heatmap showing associations between organ ages and baseline multimorbidity within major disease groupings. ",
          "Each row represents a disease grouping, each column an organ age. ",
          "Effect sizes are expressed as log incidence rate ratio (IRR) per 1 SD increase in organ age. ",
          "IRRs were estimated from count-based regression models and reflect the relative change in multimorbidity burden associated with higher organ age. ",
          "Models use the ", tags$strong(covariate_set_name), " covariate set. ",
          "Non-significant associations at FDR < 0.05 are marked by crosses. ",
          scope_text, "."
        )
      )
    } else {
      div(
        class = "text-muted small mt-3",
        style = "line-height: 1.6;",
        tags$p(
          class = "mb-0",
          "Heatmap showing associations between organ ages and the accumulation of diseases within disease groupings. ",
          "Each row represents a disease grouping, each column an organ age. ",
          "Colors indicate log hazard ratio for accelerated diagnosis accumulation per 1 SD increase in organ age, ",
          "adjusted for the ", tags$strong(covariate_set_name), " covariate set. ",
          "Non-significant associations at FDR < 0.05 are marked by crosses. ",
          scope_text, "."
        )
      )
    }
  })
  
  output$download_heatmap <- downloadHandler(
    filename = function() {
      if (input$analysis_type == "multimorbidity") {
        mm_cat <- if (!is.null(input$mm_category_tabs)) input$mm_category_tabs else "ag"
        paste0("mm_heatmap_", mm_cat, "_", Sys.Date(), ".svg")
      } else {
        paste0("heatmap_", input$analysis_type, "_", Sys.Date(), ".svg")
      }
    },
    content = function(file) {
      svg_path <- current_heatmap_path()
      if (!is.null(svg_path) && file.exists(svg_path)) {
        file.copy(svg_path, file)
      }
    }
  )
  
  
  # Current forest SVG path (reactive)
  current_forest_path <- reactive({
    if (input$analysis_type == "baseline" && input$subcategory == "current_health" && input$viz_type == "forest_table") {
      scope <- if (is.null(input$baseline_ch_scope)) "main" else input$baseline_ch_scope
      
      # Sex-stratified option removed (can be re-implemented later)
      # if (isTRUE(input$sex_stratified_toggle)) {
      #   if (is.null(input$current_health_exposure) || is.null(input$sex_stratified_sex)) return(NULL)
      #   find_sex_stratified_svg(input$analysis_set, "forest_table", input$current_health_exposure, input$sex_stratified_sex)
      # } else 
      
      if (scope == "within_pair") {
        if (is.null(input$current_health_exposure)) return(NULL)
        pair_type <- if (is.null(input$baseline_ch_within_pair_type)) "all_pairs" else input$baseline_ch_within_pair_type
        # Try within-pair continuous outcomes first, then discordant
        forest_path <- find_within_pair_forest_svg(input$analysis_set, input$current_health_exposure, pair_type)
        if (is.null(forest_path)) {
          forest_path <- find_discordant_forest_svg(input$analysis_set, input$current_health_exposure, pair_type)
        }
        forest_path
      } else {
        if (is.null(input$current_health_exposure)) return(NULL)
        find_current_health_forest_svg(input$analysis_set, input$current_health_exposure)
      }
      
    } else if (input$analysis_type == "baseline" && input$subcategory == "prevalent_disease" && input$viz_type == "forest_table") {
      if (is.null(input$prevalent_disease_time_interval) || is.null(input$prevalent_disease_disease)) return(NULL)
      if (input$prevalent_disease_scope == "main") {
        find_prevalent_disease_forest_svg(input$analysis_set, input$prevalent_disease_time_interval, input$prevalent_disease_disease)
      } else {
        pair_type <- if (is.null(input$prevalent_within_pair_type)) "all_pairs" else input$prevalent_within_pair_type
        find_prevalent_disease_within_forest_svg(input$analysis_set, input$prevalent_disease_time_interval, input$prevalent_disease_disease, pair_type)
      }
      
    } else if (input$analysis_type == "cox" && input$viz_type == "forest_table") {
      if (is.null(input$outcome)) return(NULL)
      scope <- if (is.null(input$cox_scope)) "main" else input$cox_scope
      within_pair_type <- if (scope == "within_pair") input$cox_within_pair_type else NULL
      find_forest_table_svg(input$analysis_type, input$analysis_set, NULL, input$outcome, scope, within_pair_type)
    
    } else if (input$analysis_type == "multimorbidity" && input$viz_type == "forest_table") {
      mm_cat <- if (!is.null(input$mm_category_tabs)) input$mm_category_tabs else "ag"
      if (is.null(input$mm_outcome)) return(NULL)
      scope <- if (is.null(input$mm_scope)) "main" else input$mm_scope
      within_type <- if (scope == "within_pair") {
        if (is.null(input$mm_within_pair_type)) "all_pairs" else input$mm_within_pair_type
      } else {
        NULL
      }
      find_mm_forest_svg(input$analysis_set, mm_cat, input$mm_outcome, scope, within_type)
    
    } else {
      NULL
    }
  })
  
  # Visualization: Forest tables
  output$forest_ui <- renderUI({
    svg_path <- current_forest_path()
    if (is.null(svg_path)) {
      msg <- if (input$analysis_type == "baseline" && 
                 input$subcategory == "prevalent_disease" &&
                 input$prevalent_disease_scope == "within" &&
                 !is.null(input$prevalent_within_pair_type) &&
                 input$prevalent_within_pair_type == "mz_twins") {
        "Forest table not available for MZ pairs (insufficient cases for this disease)."
      } else {
        "Forest table not available."
      }
      return(div(class = "alert alert-warning", style = "margin: 2rem;", msg))
    }
    
    # Convert absolute path to relative path from www directory
    # Since files are in www/plots, extract path relative to www/
    rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
    tags$img(src = rel_path, style = "width: 100%; height: auto; max-height: 600px;")
  })
  
  # Forest table metadata
  output$forest_metadata <- renderUI({
    svg_path <- current_forest_path()
    if (is.null(svg_path) || !file.exists(svg_path)) return(NULL)
    facts <- file_facts(svg_path)
    tags$span(class = "text-muted small",
             paste0("Last updated: ", format(facts$mtime, "%Y-%m-%d %H:%M:%S")))
  })
  
  # Forest table notes
  output$forest_notes <- renderUI({
    svg_path <- current_forest_path()
    if (is.null(svg_path)) return(NULL)
    
    note_html <- NULL
    for (rel in build_note_candidates(input)) {
      txt <- read_note_if_exists(rel)
      if (!is.null(txt)) {
        note_html <- render_md_html(txt)
        break
      }
    }
    
    if (!is.null(note_html)) {
      accordion(
        accordion_panel(
          "Notes",
          HTML(note_html)
        )
      )
    }
  })
  
  output$download_forest <- downloadHandler(
    filename = function() {
      if (input$analysis_type == "multimorbidity") {
        mm_cat <- if (!is.null(input$mm_category_tabs)) input$mm_category_tabs else "ag"
        paste0("mm_forest_", mm_cat, "_", input$mm_outcome, "_", Sys.Date(), ".svg")
      } else {
        paste0("forest_", input$analysis_type, "_", Sys.Date(), ".svg")
      }
    },
    content = function(file) {
      svg_path <- current_forest_path()
      if (!is.null(svg_path) && file.exists(svg_path)) {
        file.copy(svg_path, file)
      }
    }
  )
  
  output$clock_focus_organ_selector <- renderUI({
    organs <- discover_organ_model_organs()
    if (!length(organs)) {
      return(div(class = "alert alert-info", "No organ models found."))
    }
    
    preferred <- "Brain"
    selected <- input$clock_focus_organ
    if (is.null(selected) || !selected %in% organs) {
      selected <- if (preferred %in% organs) preferred else organs[1]
    }
    
    selectInput(
      "clock_focus_organ",
      NULL,
      choices = create_display_choices(organs),
      selected = selected
    )
  })
  
  output$organ_model_ui <- renderUI({
    organs <- discover_organ_model_organs()
    if (!length(organs)) {
      return(div(class = "alert alert-info", style = "margin: 2rem;", "No organ models found."))
    }
    
    mode <- if (is.null(input$clock_view_mode)) "gallery" else input$clock_view_mode
    
    if (identical(mode, "focused")) {
      if (is.null(input$clock_focus_organ)) {
        return(div(class = "alert alert-info", style = "margin: 2rem;", "Select an organ in the sidebar to view its top proteins."))
      }
      svg_path <- find_organ_model_svg(input$clock_focus_organ)
      if (is.null(svg_path)) {
        return(div(class = "alert alert-info", style = "margin: 2rem;", "Organ model plot not available for this organ."))
      }
      rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
      return(tags$img(src = rel_path, style = "width: 100%; height: auto; max-height: 600px;"))
    }
    
    cards <- lapply(organs, function(org) {
      svg_path <- find_organ_model_svg(org)
      if (is.null(svg_path)) return(NULL)
      rel_path <- sub(".*www/", "", gsub("\\\\", "/", svg_path))
      card(
        card_body(
          tags$img(src = rel_path, style = "width: 100%; height: auto;")
        )
      )
    })
    
    cards <- Filter(Negate(is.null), cards)
    if (!length(cards)) {
      return(div(class = "alert alert-info", style = "margin: 2rem;", "Organ model plots not available."))
    }
    
    do.call(layout_column_wrap, c(list(width = 1/2), cards))
  })
  
  output$download_clock_proteins <- downloadHandler(
    filename = function() {
      "supplementary-data-1-clocks-proteins.xlsx"
    },
    content = function(file) {
      src <- file.path(RESULTS_ROOT, "supplementary-data-1-clocks-proteins.xlsx")
      if (file.exists(src)) {
        file.copy(src, file)
      }
    }
  )
  
  
  # Find roadmap image - check multiple possible locations
  roadmap_image_found <- reactive({
    possible_paths <- c(
      file.path(APP_DIR, "www/plots/s1_graphical_abstract.png"),
      file.path(APP_DIR, "www/s1_graphical_abstract.png"),
      file.path(APP_DIR, "../results/05_publication_results/s1_graphical_abstract.png"),
      file.path(APP_DIR, "www/roadmap.png"),
      file.path(APP_DIR, "roadmap.png"),
      file.path(APP_DIR, "../docs/roadmap.png"),
      file.path(APP_DIR, "../roadmap.png")
    )
    for (path in possible_paths) {
      if (file.exists(path)) {
        # If not in www/plots folder, copy it there so Shiny can serve it
        if (!grepl("www/plots", path)) {
          plots_dir <- file.path(APP_DIR, "www/plots")
          if (!dir.exists(plots_dir)) dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
          target_path <- file.path(plots_dir, "s1_graphical_abstract.png")
          if (!file.exists(target_path) || file.mtime(path) > file.mtime(target_path)) {
            file.copy(path, target_path, overwrite = TRUE)
          }
        }
        return(TRUE)
      }
    }
    FALSE
  })
  
  # Main content - landing page or analysis views
  output$main_content <- renderUI({
    if (is.null(input$analysis_type) || input$analysis_type == "landing" || input$analysis_type == "") {
      # Landing page
      has_roadmap <- roadmap_image_found()
      
      layout_columns(
        col_widths = c(12, 6, 6),
        
        card(
          class = "bg-light",
          card_body(
            tags$h4(class = "fw-bold mb-2", "Organ-specific proteomic aging predicts multimorbidity in twins"),
            tags$p(
              class = "mb-1",
              tags$span("Hampus Hagelin", tags$sup("1"), ", "),
              tags$span("Robert Karlsson", tags$sup("1"), ", "),
              tags$span("Patrik Magnusson", tags$sup("1"), ", "),
              tags$span("Jochen M Schwenk", tags$sup("2"), ", "),
              tags$span("Sara Hägg", tags$sup("1"))
            ),
            tags$p(
              class = "small text-muted mb-1",
              tags$sup("1"), "Department of Medical Epidemiology and Biostatistics, Karolinska Institutet, Stockholm, Sweden. ",
              tags$sup("2"), "KTH Royal Institute of Technology, Stockholm, Sweden."
            ),
            tags$p(
              class = "small text-muted mb-2",
              tags$strong("DOI:"), " 10.XXXX/placeholder"
            ),
            div(
              class = "p-3 rounded",
              style = "background-color: #e7f3ff; border-left: 4px solid var(--bs-primary);",
              tags$strong("Get started:"),
              " Select an analysis type in the sidebar to explore organ-specific aging associations ",
              "across diseases and health outcomes."
            )
          )
        ),
        
        if (has_roadmap) {
          card(
            card_header(class = "fw-bold", "Study roadmap"),
            card_body(
              tags$img(
                src = "plots/s1_graphical_abstract.png",
                style = "width: 100%; height: auto;",
                alt = "Study roadmap showing the flow from blood sampling to organ age estimation and health outcomes"
              ),
              p(
                class = "mt-2 mb-0 small text-muted",
                "Overview of the study design linking circulating proteins to organ-specific aging clocks ",
                "and downstream health outcomes, including co-twin control analyses."
              )
            )
          )
        } else {
          card(
            card_header(class = "fw-bold", "Study roadmap"),
            card_body(
              p(class = "text-muted small mb-0", 
                "Roadmap image not found. Expected location: ",
                file.path(APP_DIR, "www/plots/s1_graphical_abstract.png"))
            )
          )
        },
        
        tagList(
          card(
            card_header(class = "fw-bold", "Covariate sets"),
            card_body(
              tags$ul(
                class = "list-unstyled mb-0",
                tags$li(
                  class = "mb-3 pb-3 border-bottom",
                  tags$strong("Multivariable (primary)"),
                  tags$p(
                    class = "mb-0 mt-1 small",
                    "Age, sex, years of education, smoking status (ever vs. never), ",
                    "body mass index (BMI), glycated hemoglobin (HbA1c), total-to-HDL cholesterol ratio, ",
                    "and C-reactive protein (CRP)."
                  )
                ),
                tags$li(
                  class = "mb-3 pb-3 border-bottom",
                  tags$strong("Minimally adjusted"),
                  tags$p(class = "mb-0 mt-1 small", "Age and sex.")
                ),
                tags$li(
                  class = "mb-0",
                  tags$strong("Multivariable + eGFR"),
                  tags$p(
                    class = "mb-0 mt-1 small",
                    "All multivariable covariates plus estimated glomerular ",
                    "filtration rate based on creatinine and cystatin C (eGFRcr-cys). ",
                    "Analysed separately due to data availability in a subset of participants."
                  )
                )
              )
            )
          ),
          if (has_roadmap) {
            card(
              class = "bg-light mt-3",
              card_header(class = "fw-bold", "How to use this app"),
              card_body(
                tags$ol(
                  class = "mb-0",
                  tags$li("Select a ", tags$strong("covariate set"), " to define the adjustment strategy used in the analyses."),
                  tags$li("Choose an ", tags$strong("analysis type"), " to explore baseline health, incident disease, or multimorbidity."),
                  tags$li("Use the ", tags$strong("heatmaps"), " to obtain a cohort analysis of associations across organs and outcomes."),
                  tags$li("Switch to the ", tags$strong("forest tables"), " to inspect individual estimates and confidence intervals in detail."),
                  tags$li("Explore ", tags$strong("clock composition"), " to see the proteins that contribute most strongly to each organ-specific aging clock.")
                )
              )
            )
          } else {
            card(
              class = "bg-light mt-3",
              card_header(class = "fw-bold", "How to use this app"),
              card_body(
                tags$ol(
                  class = "mb-0",
                  tags$li("Select a ", tags$strong("covariate set"), " to define the adjustment strategy used in the analyses."),
                  tags$li("Choose an ", tags$strong("analysis type"), " to explore baseline health, incident disease, or multimorbidity."),
                  tags$li("Use the ", tags$strong("heatmaps"), " to obtain a cohort analysis of associations across organs and outcomes."),
                  tags$li("Switch to the ", tags$strong("forest tables"), " to inspect individual estimates and confidence intervals in detail."),
                  tags$li("Explore ", tags$strong("clock composition"), " to see the proteins that contribute most strongly to each organ-specific aging clock.")
                )
              )
            )
          }
        )
      )
    } else {
      # Analysis content
      tagList(
        conditionalPanel(
          condition = "input.analysis_type == 'baseline'",
          uiOutput("baseline_intro_text"),
          navset_card_tab(
            id = "baseline_domain_tabs",
            nav_panel("Baseline health", value = "current_health"),
            nav_panel("Prior disease", value = "prevalent_disease")
          ),
          uiOutput("baseline_category_text")
        ),
        
        conditionalPanel(
          condition = "input.analysis_type == 'clock_proteins'",
          tagList(
            downloadButton("download_clock_proteins", "Download clock protein data (xlsx)", class = "btn-sm btn-primary mb-2"),
            uiOutput("clock_proteins_intro_text"),
            card(
              card_header("Top 10 proteins by organ"),
              card_body(
                uiOutput("organ_model_ui")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.analysis_type == 'cox'",
          uiOutput("cox_intro_text")
        ),
        
        conditionalPanel(
          condition = "input.analysis_type == 'multimorbidity'",
          uiOutput("multimorbidity_intro_text"),
          navset_card_tab(
            id = "mm_category_tabs",
            nav_panel("Incident accumulation", value = "ag"),
            nav_panel("Prevalent multimorbidities", value = "nb")
          ),
          uiOutput("mm_category_text")
        ),
        
        conditionalPanel(
          condition = "(input.cox_viz_tabs == 'heatmap' && input.analysis_type == 'cox') || (input.mm_viz_tabs == 'heatmap' && input.analysis_type == 'multimorbidity') || (input.baseline_ch_viz_tabs == 'heatmap' && input.analysis_type == 'baseline' && input.subcategory == 'current_health') || (input.baseline_pd_viz_tabs == 'heatmap' && input.analysis_type == 'baseline' && input.subcategory == 'prevalent_disease')",
          card(
            card_header("Heatmap Visualization"),
            card_body(
              uiOutput("heatmap_ui"),
              conditionalPanel(
                condition = "input.analysis_type == 'baseline' && input.subcategory == 'current_health' && input.baseline_ch_viz_tabs == 'heatmap'",
                uiOutput("baseline_health_heatmap_caption")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'baseline' && input.subcategory == 'prevalent_disease' && input.baseline_pd_viz_tabs == 'heatmap'",
                uiOutput("prevalent_disease_heatmap_caption")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'cox' && input.cox_viz_tabs == 'heatmap'",
                uiOutput("cox_heatmap_caption")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'multimorbidity' && input.mm_viz_tabs == 'heatmap'",
                uiOutput("multimorbidity_heatmap_caption")
              )
            ),
            card_footer(
              class = "text-end",
              downloadButton("download_heatmap", "Download SVG", class = "btn-sm btn-primary")
            )
          )
        ),
        
        conditionalPanel(
          condition = "(input.cox_viz_tabs == 'forest_table' && input.analysis_type == 'cox') || (input.mm_viz_tabs == 'forest_table' && input.analysis_type == 'multimorbidity') || (input.baseline_ch_viz_tabs == 'forest_table' && input.analysis_type == 'baseline' && input.subcategory == 'current_health') || (input.baseline_pd_viz_tabs == 'forest_table' && input.analysis_type == 'baseline' && input.subcategory == 'prevalent_disease')",
          card(
            card_header("Forest Table"),
            card_body(
              uiOutput("forest_ui")
            ),
            card_footer(
              class = "d-flex justify-content-between align-items-center",
              uiOutput("forest_metadata"),
              downloadButton("download_forest", "Download SVG", class = "btn-sm btn-primary")
            ),
            uiOutput("forest_notes")
          )
        )
      )
    }
  })
  
  # Explanatory text for each analysis type
  output$cox_intro_text <- renderUI({
    div(
      class = "alert alert-info mb-2",
      style = "background-color: #e7f3ff; border-left: 4px solid #3498db; border-radius: 0.5rem; padding: 0.5rem 0.75rem;",
      tags$strong("Incident age-related disease"),
      tags$p(
        class = "mb-0 mt-1",
        style = "font-size: 0.9rem;",
        "Survival analysis (Cox) examining how organ-age estimates predict incident disease onset and mortality, and healthspan."
      )
    )
  })
  
  output$baseline_intro_text <- renderUI({
    div(
      class = "alert alert-info mb-2",
      style = "background-color: #e7f3ff; border-left: 4px solid #3498db; border-radius: 0.5rem; padding: 0.5rem 0.75rem;",
      tags$strong("Baseline"),
      tags$p(
        class = "mb-0 mt-1",
        style = "font-size: 0.9rem;",
        "Explore associations between organ-age measures and health outcomes at baseline. ",
        "Results show how each organ's biological age relates to baseline health status and prevalence of prior diseases."
      )
    )
  })
  
  output$clock_proteins_intro_text <- renderUI({
    div(
      class = "alert alert-info mb-2",
      style = "background-color: #e7f3ff; border-left: 4px solid #3498db; border-radius: 0.5rem; padding: 0.5rem 0.75rem;",
      tags$strong("Clock proteins"),
      tags$p(
        class = "mb-0 mt-1",
        style = "font-size: 0.9rem;",
        "Overview of proteins contributing most strongly to the organ-specific aging clocks. ",
        "Browse, in a two-column gallery, the top 10 clock proteins for each organ. ",
        "Proteins are selected by absolute coefficient magnitude, then ranked from highest to lowest."
      )
    )
  })
  
  output$baseline_category_text <- renderUI({
    req(input$subcategory)
    
    if (input$subcategory == "current_health") {
      div(
        class = "alert alert-light mb-2 mt-1",
        style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; border-radius: 0.5rem; padding: 0.4rem 0.75rem;",
        tags$p(
          class = "mb-0",
          style = "font-size: 0.9rem;",
          tags$strong("Baseline health: "),
          "Associations between organ ages and health metrics measured at baseline, ",
          "including lifestyle, clinical biomarkers, and other markers of aging."
        )
      )
    } else if (input$subcategory == "prevalent_disease") {
      div(
        class = "alert alert-light mb-2 mt-1",
        style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; border-radius: 0.5rem; padding: 0.4rem 0.75rem;",
        tags$p(
          class = "mb-0",
          style = "font-size: 0.9rem;",
          tags$strong("Prior disease: "),
          "Associations between organ ages and prior diagnoses of age-related diseases."
        )
      )
    } else {
      NULL
    }
  })
  
  output$multimorbidity_intro_text <- renderUI({
    div(
      class = "alert alert-info mb-2",
      style = "background-color: #e7f3ff; border-left: 4px solid #3498db; border-radius: 0.5rem; padding: 0.5rem 0.75rem;",
      tags$strong("Multimorbidity analyses"),
      tags$p(
        class = "mb-0 mt-1",
        style = "font-size: 0.9rem;",
        "Examine how organ-age measures predict multimorbidity patterns. ",
        "Analyses include both incident accumulation of diseases over time and baseline multimorbidity count in multiple disease groupings."
      )
    )
  })
  
  output$mm_category_text <- renderUI({
    req(input$mm_category_tabs)
    
    if (input$mm_category_tabs == "ag") {
      div(
        class = "alert alert-light mb-2 mt-1",
        style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; border-radius: 0.5rem; padding: 0.4rem 0.75rem;",
        tags$p(
          class = "mb-0",
          style = "font-size: 0.9rem;",
          tags$strong("Incident accumulation: "),
          "Prospective analysis of disease accumulation over follow-up. ",
          "Results show how organ age predicts the rate of developing diseases in various disease groupings."
        )
      )
    } else {
      div(
        class = "alert alert-light mb-2 mt-1",
        style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; border-radius: 0.5rem; padding: 0.4rem 0.75rem;",
        tags$p(
          class = "mb-0",
          style = "font-size: 0.9rem;",
          tags$strong("Prevalent multimorbidities: "),
          "Cross-sectional analysis of existing disease burden. ",
          "Results show associations between organ age and the number of prevalent conditions at baseline within various disease groupings."
        )
      )
    }
  })
}

shinyApp(ui, server)
