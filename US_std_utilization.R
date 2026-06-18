###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# COHORT INPUT FILE
# ---------------------------------------------------------------------------
# Supported formats:
# - .csv
# - .rds
# - .xlsx
#
# Required columns:
# - DATE_VAR: index date
# - AGE_VAR: age in years
# - SEX_VAR: sex category or sex indicator
# - EXPOSURE_VAR: initial P2Y12 inhibitor group
COHORT_FILE  <- "<COHORT_COVARIATE_FILE>"
COHORT_SHEET <- "<COHORT_COVARIATE_SHEET>"  # use 1 for first sheet, or NULL for csv/rds

DATE_VAR     <- "index_date"
AGE_VAR      <- "age_years"
SEX_VAR      <- "sex"
EXPOSURE_VAR <- "initial_p2y12i"

# ---------------------------------------------------------------------------
# STANDARD POPULATION INPUT FILE
# ---------------------------------------------------------------------------
# Required columns:
# - age_cat
# - sex
# - standard_population_weight
STD_FILE       <- "<STANDARD_POPULATION_EXCEL_FILE>"
STD_SHEET      <- "<STANDARD_POPULATION_SHEET>"
STD_WEIGHT_COL <- "standard_population_weight"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
# The output workbook contains:
# - standardized_counts
# - collapse_log
# - period_check
# - weight_check
# - strata_weights
# - age_group_map
OUTPUT_FILE <- "<STANDARDIZED_UTILIZATION_OUTPUT_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# STUDY PERIOD
# ---------------------------------------------------------------------------
START_YEAR <- 2011
END_YEAR   <- 2024

# ---------------------------------------------------------------------------
# STANDARDIZATION STRATA
# ---------------------------------------------------------------------------
AGE_LEVELS <- c("18-44", "45-64", "65-85", ">85")
SEX_LEVELS <- c("M", "F")


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "readxl",
  "writexl",
  "lubridate",
  "tools"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(readxl)
library(writexl)
library(lubridate)
library(tools)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

read_tabular_file <- function(file, sheet = NULL) {
  extension <- tolower(tools::file_ext(file))
  
  if (extension == "csv") {
    return(readr::read_csv(file, show_col_types = FALSE))
  }
  
  if (extension == "rds") {
    return(readRDS(file))
  }
  
  if (extension %in% c("xlsx", "xls")) {
    if (is.null(sheet) || identical(sheet, "") || identical(sheet, "<COHORT_COVARIATE_SHEET>")) {
      sheet <- 1
    }
    
    return(readxl::read_excel(file, sheet = sheet))
  }
  
  stop("Unsupported input file type: .", extension)
}

normalize_age_cat <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\u2013|\u2014", "-", x)
  x <- gsub("\\s+", "", x)
  
  dplyr::case_when(
    x == "18-44" ~ "18-44",
    x == "45-64" ~ "45-64",
    x == "65-85" ~ "65-85",
    x %in% c(">85", "85>", "85+", ">85+", "86+") ~ ">85",
    TRUE ~ x
  )
}

normalize_sex <- function(x) {
  x <- trimws(as.character(x))
  x_up <- toupper(x)
  
  dplyr::case_when(
    x_up %in% c("0", "M", "MALE", "MEN", "MAN") ~ "M",
    x_up %in% c("1", "F", "FEMALE", "WOMEN", "WOMAN") ~ "F",
    TRUE ~ NA_character_
  )
}

age_group_label <- function(age_values) {
  age_values <- AGE_LEVELS[AGE_LEVELS %in% unique(as.character(age_values))]
  
  age_min <- c("18-44" = 18, "45-64" = 45, "65-85" = 65, ">85" = 86)
  age_max <- c("18-44" = 44, "45-64" = 64, "65-85" = 85, ">85" = Inf)
  
  low <- min(age_min[age_values], na.rm = TRUE)
  high <- max(age_max[age_values], na.rm = TRUE)
  
  if (is.infinite(high)) {
    if (low >= 86) {
      return(">85")
    }
    
    return(paste0(low, "+"))
  }
  
  paste0(low, "-", high)
}

empty_collapse_log <- function() {
  tibble(
    year = integer(),
    half_year = integer(),
    sex = character(),
    empty_age_cat = character(),
    n_empty_age_cat = integer(),
    immediate_merge_into_age_cat = character(),
    final_age_group = character(),
    action = character(),
    reason = character()
  )
}

prepare_standard_population <- function(
    std_pop_raw,
    age_levels = AGE_LEVELS,
    sex_levels = SEX_LEVELS,
    std_weight_col = STD_WEIGHT_COL
) {
  required_cols <- c("age_cat", "sex", std_weight_col)
  missing_cols <- setdiff(required_cols, names(std_pop_raw))
  
  if (length(missing_cols) > 0) {
    stop(
      "The standard population file is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  std_tmp <- std_pop_raw %>%
    mutate(
      std_value = suppressWarnings(
        as.numeric(
          gsub(
            ",",
            ".",
            gsub("%", "", as.character(.data[[std_weight_col]]))
          )
        )
      )
    )
  
  convert_percent_to_proportion <- max(std_tmp$std_value, na.rm = TRUE) > 1
  
  std_pop <- std_tmp %>%
    transmute(
      age_cat = normalize_age_cat(age_cat),
      sex = normalize_sex(sex),
      std_w = if (convert_percent_to_proportion) {
        std_value / 100
      } else {
        std_value
      }
    ) %>%
    filter(
      !is.na(age_cat),
      !is.na(sex),
      !is.na(std_w)
    ) %>%
    group_by(age_cat, sex) %>%
    summarise(
      std_w = sum(std_w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      age_cat = factor(age_cat, levels = age_levels),
      sex = factor(sex, levels = sex_levels)
    )
  
  expected_grid <- tidyr::expand_grid(
    age_cat = factor(age_levels, levels = age_levels),
    sex = factor(sex_levels, levels = sex_levels)
  )
  
  std_pop <- expected_grid %>%
    left_join(std_pop, by = c("age_cat", "sex")) %>%
    arrange(age_cat, sex)
  
  if (any(is.na(std_pop$std_w))) {
    stop("The standard population does not contain all required age-sex strata.")
  }
  
  if (abs(sum(std_pop$std_w) - 1) >= 1e-8) {
    stop("The standard population weights do not sum to 1.")
  }
  
  std_pop
}

make_period_age_group_map <- function(
    period_data,
    std_pop,
    age_levels = AGE_LEVELS,
    sex_levels = SEX_LEVELS
) {
  period_keys <- period_data %>%
    distinct(year, half_year)
  
  if (nrow(period_keys) != 1L) {
    stop("Expected data from exactly one year x half-year period.")
  }
  
  year_value <- period_keys$year[[1]]
  half_year_value <- period_keys$half_year[[1]]
  
  counts <- period_data %>%
    mutate(
      age_cat = as.character(age_cat),
      sex = as.character(sex)
    ) %>%
    count(sex, age_cat, name = "n_hh") %>%
    complete(
      sex = sex_levels,
      age_cat = age_levels,
      fill = list(n_hh = 0L)
    )
  
  map_list <- list()
  log_list <- list()
  
  for (sex_value in sex_levels) {
    counts_sex <- counts %>%
      filter(sex == sex_value)
    
    std_sex <- std_pop %>%
      mutate(
        age_cat = as.character(age_cat),
        sex = as.character(sex)
      ) %>%
      filter(sex == sex_value)
    
    std_present <- setNames(
      std_sex$std_w > 0,
      std_sex$age_cat
    )
    
    target <- setNames(age_levels, age_levels)
    
    first_age <- age_levels[1]
    first_n <- counts_sex$n_hh[match(first_age, counts_sex$age_cat)]
    
    if (length(first_n) == 0 || is.na(first_n)) {
      first_n <- 0
    }
    
    if (first_n == 0 && isTRUE(std_present[[first_age]])) {
      log_list[[length(log_list) + 1L]] <- tibble(
        year = year_value,
        half_year = half_year_value,
        sex = sex_value,
        empty_age_cat = first_age,
        n_empty_age_cat = first_n,
        immediate_merge_into_age_cat = NA_character_,
        action = "not_collapsed_no_lower_age_group",
        reason = paste0(
          "No patients in age-sex stratum ",
          first_age,
          "/",
          sex_value,
          ". This is the lowest age category and cannot be merged downward."
        )
      )
    }
    
    for (i in seq(length(age_levels), 2L, by = -1L)) {
      current_age <- age_levels[i]
      lower_age <- age_levels[i - 1L]
      
      n_current <- counts_sex$n_hh[match(current_age, counts_sex$age_cat)]
      
      if (length(n_current) == 0 || is.na(n_current)) {
        n_current <- 0
      }
      
      if (n_current == 0 && isTRUE(std_present[[current_age]])) {
        target[[current_age]] <- lower_age
        
        log_list[[length(log_list) + 1L]] <- tibble(
          year = year_value,
          half_year = half_year_value,
          sex = sex_value,
          empty_age_cat = current_age,
          n_empty_age_cat = n_current,
          immediate_merge_into_age_cat = lower_age,
          action = "collapsed_to_next_lower_age_group",
          reason = paste0(
            "No patients in age-sex stratum ",
            current_age,
            "/",
            sex_value,
            ". The stratum was merged with ",
            lower_age,
            "/",
            sex_value,
            "."
          )
        )
      }
    }
    
    resolve_target <- function(age_group) {
      seen <- character()
      current <- age_group
      
      while (!identical(target[[current]], current)) {
        if (current %in% seen) {
          stop("Circular age-stratum collapse detected.")
        }
        
        seen <- c(seen, current)
        current <- target[[current]]
      }
      
      current
    }
    
    final_anchor <- vapply(age_levels, resolve_target, character(1))
    
    member_table <- tibble(
      age_cat = age_levels,
      age_group_anchor = unname(final_anchor[age_levels])
    )
    
    label_table <- member_table %>%
      group_by(age_group_anchor) %>%
      summarise(
        age_group = age_group_label(age_cat),
        .groups = "drop"
      )
    
    map_list[[length(map_list) + 1L]] <- member_table %>%
      left_join(label_table, by = "age_group_anchor") %>%
      transmute(
        year = year_value,
        half_year = half_year_value,
        sex = sex_value,
        age_cat,
        age_group_anchor,
        age_group
      )
  }
  
  map <- bind_rows(map_list)
  log <- bind_rows(log_list)
  
  if (nrow(log) == 0) {
    log <- empty_collapse_log()
  } else {
    log <- log %>%
      left_join(
        map %>%
          transmute(
            sex,
            empty_age_cat = age_cat,
            final_age_group = age_group
          ),
        by = c("sex", "empty_age_cat")
      )
  }
  
  list(
    map = map,
    log = log
  )
}


###############################################################################
# 4) READ INPUTS
###############################################################################

cohort_raw <- read_tabular_file(
  file = COHORT_FILE,
  sheet = COHORT_SHEET
)

standard_population_raw <- readxl::read_excel(
  STD_FILE,
  sheet = STD_SHEET
)

standard_population <- prepare_standard_population(
  std_pop_raw = standard_population_raw
)


###############################################################################
# 5) PREPARE HALF-YEARLY COHORT
###############################################################################

required_cohort_cols <- c(DATE_VAR, AGE_VAR, SEX_VAR, EXPOSURE_VAR)
missing_cohort_cols <- setdiff(required_cohort_cols, names(cohort_raw))

if (length(missing_cohort_cols) > 0) {
  stop(
    "The cohort file is missing required columns: ",
    paste(missing_cohort_cols, collapse = ", ")
  )
}

cohort <- cohort_raw %>%
  mutate(
    index_date = as.Date(.data[[DATE_VAR]]),
    age_years = suppressWarnings(as.numeric(.data[[AGE_VAR]])),
    sex = normalize_sex(.data[[SEX_VAR]]),
    
    age_cat = case_when(
      age_years >= 18 & age_years <= 44 ~ "18-44",
      age_years >= 45 & age_years <= 64 ~ "45-64",
      age_years >= 65 & age_years <= 85 ~ "65-85",
      age_years > 85 ~ ">85",
      TRUE ~ NA_character_
    ),
    
    index_exposure = as.character(.data[[EXPOSURE_VAR]]),
    
    year = lubridate::year(index_date),
    half_year = if_else(lubridate::month(index_date) <= 6, 1L, 2L),
    
    age_cat = factor(age_cat, levels = AGE_LEVELS),
    sex = factor(sex, levels = SEX_LEVELS)
  ) %>%
  filter(
    year >= START_YEAR,
    year <= END_YEAR,
    !is.na(index_date),
    !is.na(age_cat),
    sex %in% SEX_LEVELS,
    !is.na(index_exposure),
    !is.na(year),
    !is.na(half_year)
  )

if (nrow(cohort) == 0) {
  stop("The analytic cohort is empty after applying required filters.")
}


###############################################################################
# 6) BUILD PERIOD-SPECIFIC AGE-GROUP COLLAPSING MAP
###############################################################################

period_maps_and_logs <- cohort %>%
  group_by(year, half_year) %>%
  group_split() %>%
  purrr::map(
    make_period_age_group_map,
    std_pop = standard_population,
    age_levels = AGE_LEVELS,
    sex_levels = SEX_LEVELS
  )

age_group_map <- purrr::map_dfr(period_maps_and_logs, "map") %>%
  arrange(year, half_year, sex, age_cat)

collapse_log <- purrr::map_dfr(period_maps_and_logs, "log") %>%
  arrange(year, half_year, sex, empty_age_cat)


###############################################################################
# 7) CREATE PERIOD-SPECIFIC STANDARD POPULATION AFTER COLLAPSING
###############################################################################

standard_period <- age_group_map %>%
  left_join(
    standard_population,
    by = c("age_cat", "sex")
  ) %>%
  group_by(year, half_year, sex, age_group) %>%
  summarise(
    std_w = sum(std_w, na.rm = TRUE),
    base_age_cats = paste(as.character(age_cat), collapse = " + "),
    .groups = "drop"
  )

period_std_check <- standard_period %>%
  group_by(year, half_year) %>%
  summarise(
    sum_std_w = sum(std_w, na.rm = TRUE),
    .groups = "drop"
  )

if (any(abs(period_std_check$sum_std_w - 1) > 1e-8, na.rm = TRUE)) {
  stop("At least one period has standard weights that do not sum to 1.")
}


###############################################################################
# 8) ATTACH COLLAPSED AGE GROUPS AND CALCULATE WEIGHTS
###############################################################################

cohort_with_age_groups <- cohort %>%
  left_join(
    age_group_map %>%
      select(year, half_year, sex, age_cat, age_group),
    by = c("year", "half_year", "sex", "age_cat")
  )

if (any(is.na(cohort_with_age_groups$age_group))) {
  stop("Some patients could not be matched to a collapsed age group.")
}

strata_counts <- cohort_with_age_groups %>%
  count(year, half_year, sex, age_group, name = "n_hh")

strata_weights <- standard_period %>%
  left_join(
    strata_counts,
    by = c("year", "half_year", "sex", "age_group")
  ) %>%
  mutate(
    n_hh = coalesce(n_hh, 0L)
  ) %>%
  group_by(year, half_year) %>%
  mutate(
    n_period = sum(n_hh),
    observed_weight = if_else(n_period > 0, n_hh / n_period, NA_real_),
    patient_weight = case_when(
      n_period > 0 & n_hh > 0 ~ std_w / observed_weight,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

period_check <- strata_weights %>%
  mutate(
    missing_stratum_after_collapse = n_hh == 0 & std_w > 0
  ) %>%
  group_by(year, half_year) %>%
  summarise(
    n_period = first(n_period),
    n_missing_strata_after_collapse = sum(missing_stratum_after_collapse),
    valid_standardization = n_missing_strata_after_collapse == 0 & n_period > 0,
    .groups = "drop"
  )

cohort_weighted <- cohort_with_age_groups %>%
  left_join(
    strata_weights %>%
      select(year, half_year, sex, age_group, patient_weight),
    by = c("year", "half_year", "sex", "age_group")
  )


###############################################################################
# 9) CREATE STANDARDIZED COUNTS
###############################################################################

standardized_counts <- cohort_weighted %>%
  group_by(year, half_year, index_exposure) %>%
  summarise(
    standardized_n = sum(patient_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    period_check %>%
      select(year, half_year, valid_standardization),
    by = c("year", "half_year")
  ) %>%
  mutate(
    standardized_n = if_else(
      valid_standardization,
      standardized_n,
      NA_real_
    )
  ) %>%
  select(
    year,
    half_year,
    index_exposure,
    standardized_n
  ) %>%
  arrange(year, half_year, index_exposure)

weight_check <- cohort_weighted %>%
  group_by(year, half_year) %>%
  summarise(
    n_persons = n(),
    sum_weights = sum(patient_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_check, by = c("year", "half_year"))


###############################################################################
# 10) EXPORT RESULTS
###############################################################################

if (nrow(collapse_log) > 0) {
  message("Age-stratum collapsing was applied. See collapse_log in the output.")
} else {
  message("No age-stratum collapsing was needed.")
}

writexl::write_xlsx(
  list(
    standardized_counts = standardized_counts,
    collapse_log = collapse_log,
    period_check = period_check,
    weight_check = weight_check,
    strata_weights = strata_weights,
    age_group_map = age_group_map
  ),
  path = OUTPUT_FILE
)

message("Done. Standardized utilization output written to: ", OUTPUT_FILE)
