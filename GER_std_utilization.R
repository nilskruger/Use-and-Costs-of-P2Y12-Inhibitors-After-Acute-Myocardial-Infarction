###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# STANDARD POPULATION INPUT FILE
# ---------------------------------------------------------------------------
# Required columns in STD_SHEET:
#   - age_cat: age category, e.g. "18-44", "45-64", "65-85", ">85"
#   - sex: sex category, e.g. "M"/"F", "Male"/"Female", "Men"/"Women"
#   - standard_population_weight: percentage or proportion
#
# If percentages are provided, e.g. 12.5, they are converted to proportions.
# If proportions are provided, e.g. 0.125, they are used as-is.
STD_FILE  <- "<STANDARD_POPULATION_EXCEL_FILE>"
STD_SHEET <- "<STANDARD_POPULATION_SHEET>"

# ---------------------------------------------------------------------------
# DATABASE INPUT
# ---------------------------------------------------------------------------
# The cohort table should contain one row per cohort member.
#
# Required columns:
#   - DATE_VAR: index date
#   - EXPOSURE_VAR: treatment/exposure group
#   - AGE_VAR: age in years
#   - SEX_VAR: sex indicator or label
#
# Expected sex coding:
#   - 0 or M/Male/Men = male
#   - 1 or F/Female/Women = female
ODBC_DSN     <- "<ODBC_DSN>"
COHORT_TABLE <- "<DATABASE>.<SCHEMA>.<COHORT_TABLE>"

DATE_VAR     <- "index_date"
EXPOSURE_VAR <- "initial_p2y12i"
AGE_VAR      <- "age_years"
SEX_VAR      <- "sex"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
# The Excel workbook will contain:
#   - standardized_counts
#   - collapse_log
#   - period_check
#   - weight_check
#   - strata_weights
#   - age_group_map
OUTPUT_FILE <- "<STANDARDIZED_UTILIZATION_OUTPUT_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# STANDARDIZATION STRATA
# ---------------------------------------------------------------------------
AGE_LEVELS <- c("18-44", "45-64", "65-85", ">85")
SEX_LEVELS <- c("M", "F")


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "odbc",
  "DBI",
  "dplyr",
  "tidyr",
  "readxl",
  "lubridate",
  "purrr",
  "writexl"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(odbc)
library(DBI)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(purrr)
library(writexl)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

normalize_age_cat <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\u2013|\u2014", "-", x)  # replace en dash/em dash with hyphen
  x <- gsub("\\s+", "", x)            # remove whitespace
  
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
    TRUE ~ x_up
  )
}

age_group_label <- function(age_values) {
  age_values <- unique(as.character(age_values))
  age_values <- AGE_LEVELS[AGE_LEVELS %in% age_values]
  
  age_min <- c("18-44" = 18, "45-64" = 45, "65-85" = 65, ">85" = 86)
  age_max <- c("18-44" = 44, "45-64" = 64, "65-85" = 85, ">85" = Inf)
  
  lo <- min(age_min[age_values], na.rm = TRUE)
  hi <- max(age_max[age_values], na.rm = TRUE)
  
  if (is.infinite(hi)) {
    if (lo >= 86) {
      return(">85")
    }
    return(paste0(lo, "+"))
  }
  
  paste0(lo, "-", hi)
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

make_period_age_group_map <- function(
    period_data,
    std_pop,
    age_levels = AGE_LEVELS,
    sex_levels = SEX_LEVELS
) {
  period_keys <- period_data %>%
    distinct(year, half_year)
  
  if (nrow(period_keys) != 1L) {
    stop("make_period_age_group_map() expects data from exactly one year x half-year period.")
  }
  
  year_val <- period_keys$year[[1]]
  half_val <- period_keys$half_year[[1]]
  
  counts <- period_data %>%
    mutate(
      age_cat = as.character(age_cat),
      sex = as.character(sex)
    ) %>%
    count(sex, age_cat, name = "n_hh") %>%
    complete(
      sex = sex_levels,
      age_cat = age_levels,
      fill = list(n_hh = 0)
    ) %>%
    mutate(
      age_cat = factor(age_cat, levels = age_levels),
      sex = factor(sex, levels = sex_levels)
    ) %>%
    arrange(sex, age_cat)
  
  map_list <- list()
  log_list <- list()
  
  for (sx in sex_levels) {
    ct <- counts %>%
      filter(as.character(sex) == sx) %>%
      arrange(age_cat)
    
    sx_std <- std_pop %>%
      mutate(
        age_cat = as.character(age_cat),
        sex = as.character(sex)
      ) %>%
      filter(sex == sx) %>%
      arrange(match(age_cat, age_levels))
    
    std_present <- setNames(sx_std$std_w > 0, as.character(sx_std$age_cat))
    
    # Initially, each original age group maps to itself.
    target <- setNames(age_levels, age_levels)
    
    # The lowest age stratum cannot be collapsed downward.
    first_age <- age_levels[1]
    first_n <- ct$n_hh[match(first_age, as.character(ct$age_cat))]
    
    if (length(first_n) == 0 || is.na(first_n)) {
      first_n <- 0
    }
    
    if (first_n == 0 && isTRUE(std_present[[first_age]])) {
      log_list[[length(log_list) + 1L]] <- tibble(
        year = year_val,
        half_year = half_val,
        sex = sx,
        empty_age_cat = first_age,
        n_empty_age_cat = first_n,
        immediate_merge_into_age_cat = NA_character_,
        action = "not_collapsed_no_lower_age_group",
        reason = paste0(
          "No patients in age-sex stratum ", first_age, "/", sx,
          ". This is the lowest age category and cannot be merged downward."
        )
      )
    }
    
    # Higher empty age strata are collapsed into the next lower age stratum.
    # This is done within sex because the standardization strata are age x sex.
    for (i in seq(length(age_levels), 2L, by = -1L)) {
      current_age <- age_levels[i]
      lower_age <- age_levels[i - 1L]
      
      n_current <- ct$n_hh[match(current_age, as.character(ct$age_cat))]
      
      if (length(n_current) == 0 || is.na(n_current)) {
        n_current <- 0
      }
      
      if (n_current == 0 && isTRUE(std_present[[current_age]])) {
        target[[current_age]] <- lower_age
        
        log_list[[length(log_list) + 1L]] <- tibble(
          year = year_val,
          half_year = half_val,
          sex = sx,
          empty_age_cat = current_age,
          n_empty_age_cat = n_current,
          immediate_merge_into_age_cat = lower_age,
          action = "collapsed_to_next_lower_age_group",
          reason = paste0(
            "No patients in age-sex stratum ", current_age, "/", sx,
            ". The stratum was merged with the next lower age stratum ",
            lower_age, "/", sx, "."
          )
        )
      }
    }
    
    # Resolve chained collapsing.
    # Example:
    # If >85 is empty and 65-85 is also empty, >85 first maps to 65-85,
    # and both finally map to 45-64.
    resolve_target <- function(a) {
      seen <- character()
      current <- a
      
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
    
    member_tbl <- tibble(
      age_cat = age_levels,
      age_group_anchor = unname(final_anchor[age_levels])
    )
    
    label_tbl <- member_tbl %>%
      group_by(age_group_anchor) %>%
      summarise(
        age_group = age_group_label(age_cat),
        .groups = "drop"
      )
    
    map_sx <- member_tbl %>%
      left_join(label_tbl, by = "age_group_anchor") %>%
      transmute(
        year = year_val,
        half_year = half_val,
        sex = factor(sx, levels = sex_levels),
        age_cat = factor(age_cat, levels = age_levels),
        age_group_anchor,
        age_group
      )
    
    map_list[[length(map_list) + 1L]] <- map_sx
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
            sex = as.character(sex),
            empty_age_cat = as.character(age_cat),
            final_age_group = age_group
          ),
        by = c("sex", "empty_age_cat")
      ) %>%
      select(
        year,
        half_year,
        sex,
        empty_age_cat,
        n_empty_age_cat,
        immediate_merge_into_age_cat,
        final_age_group,
        action,
        reason
      ) %>%
      arrange(year, half_year, sex, empty_age_cat)
  }
  
  list(
    map = map,
    log = log
  )
}


###############################################################################
# 4) READ STANDARD POPULATION
###############################################################################

standard_population_raw <- read_excel(STD_FILE, sheet = STD_SHEET)

required_std_cols <- c("age_cat", "sex", "standard_population_weight")
missing_std_cols <- setdiff(required_std_cols, names(standard_population_raw))

if (length(missing_std_cols) > 0) {
  stop(
    "The standard population file is missing required columns: ",
    paste(missing_std_cols, collapse = ", ")
  )
}

standard_population_prepped <- standard_population_raw %>%
  mutate(
    std_value = suppressWarnings(
      as.numeric(gsub(",", ".", as.character(standard_population_weight)))
    )
  )

convert_percent_to_proportion <- max(
  standard_population_prepped$std_value,
  na.rm = TRUE
) > 1

standard_population <- standard_population_prepped %>%
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
    std_w = sum(std_w),
    .groups = "drop"
  ) %>%
  mutate(
    age_cat = factor(age_cat, levels = AGE_LEVELS),
    sex = factor(sex, levels = SEX_LEVELS)
  )

expected_standard_grid <- expand_grid(
  age_cat = factor(AGE_LEVELS, levels = AGE_LEVELS),
  sex = factor(SEX_LEVELS, levels = SEX_LEVELS)
)

standard_population <- expected_standard_grid %>%
  left_join(standard_population, by = c("age_cat", "sex")) %>%
  arrange(age_cat, sex)

if (any(is.na(standard_population$std_w))) {
  stop("The standard population does not contain all required age x sex strata.")
}

if (abs(sum(standard_population$std_w) - 1) >= 1e-8) {
  stop("The standard population weights do not sum to 1.")
}


###############################################################################
# 5) LOAD COHORT FROM DATABASE
###############################################################################

cohort_query <- paste0("SELECT * FROM ", COHORT_TABLE)

connection <- dbConnect(odbc::odbc(), ODBC_DSN)

cohort_raw <- tryCatch(
  dbGetQuery(connection, cohort_query),
  finally = dbDisconnect(connection)
)

required_cohort_cols <- c(DATE_VAR, EXPOSURE_VAR, AGE_VAR, SEX_VAR)
missing_cohort_cols <- setdiff(required_cohort_cols, names(cohort_raw))

if (length(missing_cohort_cols) > 0) {
  stop(
    "The cohort table is missing required columns: ",
    paste(missing_cohort_cols, collapse = ", ")
  )
}


###############################################################################
# 6) BUILD ANALYTIC COHORT WITH YEAR AND HALF-YEAR
###############################################################################

cohort <- cohort_raw %>%
  mutate(
    index_date = as.Date(.data[[DATE_VAR]]),
    age_years = suppressWarnings(as.numeric(.data[[AGE_VAR]])),
    sex_raw = trimws(as.character(.data[[SEX_VAR]])),
    
    age_cat = case_when(
      between(age_years, 18, 44) ~ "18-44",
      between(age_years, 45, 64) ~ "45-64",
      between(age_years, 65, 85) ~ "65-85",
      age_years > 85 ~ ">85",
      TRUE ~ NA_character_
    ),
    
    sex = normalize_sex(sex_raw),
    
    index_exposure = as.character(.data[[EXPOSURE_VAR]]),
    
    year = year(index_date),
    half_year = if_else(month(index_date) <= 6, 1L, 2L),
    
    age_cat = factor(age_cat, levels = AGE_LEVELS),
    sex = factor(sex, levels = SEX_LEVELS)
  ) %>%
  filter(
    !is.na(index_date),
    !is.na(age_cat),
    !is.na(sex),
    !is.na(index_exposure),
    !is.na(year),
    !is.na(half_year)
  ) %>%
  mutate(
    index_exposure = factor(index_exposure)
  )

if (nrow(cohort) == 0) {
  stop("The analytic cohort is empty after applying required filters.")
}


###############################################################################
# 7) BUILD PERIOD-SPECIFIC AGE-GROUP COLLAPSING MAP
###############################################################################
# For each year x half-year period:
# - Empty upper age strata are collapsed into the next lower age stratum.
# - Collapsing is sex-specific because standardization is age x sex.
# - Every collapse is written to collapse_log.

period_maps_and_logs <- cohort %>%
  group_by(year, half_year) %>%
  group_split() %>%
  map(
    make_period_age_group_map,
    std_pop = standard_population,
    age_levels = AGE_LEVELS,
    sex_levels = SEX_LEVELS
  )

age_group_map <- map_dfr(period_maps_and_logs, "map") %>%
  arrange(year, half_year, sex, age_cat)

collapse_log <- map_dfr(period_maps_and_logs, "log") %>%
  arrange(year, half_year, sex, empty_age_cat)


###############################################################################
# 8) CREATE COLLAPSED STANDARD POPULATION BY HALF-YEAR
###############################################################################

standard_period <- age_group_map %>%
  left_join(standard_population, by = c("age_cat", "sex")) %>%
  group_by(year, half_year, sex, age_group) %>%
  summarise(
    std_w = sum(std_w),
    base_age_cats = paste(as.character(age_cat), collapse = " + "),
    .groups = "drop"
  ) %>%
  arrange(year, half_year, sex, age_group)

period_std_sum_check <- standard_period %>%
  group_by(year, half_year) %>%
  summarise(
    sum_std_w = sum(std_w),
    .groups = "drop"
  )

if (any(abs(period_std_sum_check$sum_std_w - 1) > 1e-8, na.rm = TRUE)) {
  stop("At least one year x half-year period has standard weights that do not sum to 1.")
}


###############################################################################
# 9) ATTACH COLLAPSED AGE GROUP TO EACH PATIENT
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


###############################################################################
# 10) CREATE HALF-YEAR-SPECIFIC STANDARDIZATION WEIGHTS
###############################################################################
# For each year x half-year period:
#
#   observed stratum weight = stratum n / period n
#
#   patient weight =
#       standard population stratum weight / observed stratum weight
#
# If standardization is valid, the sum of patient weights in a period should be
# approximately equal to the number of patients in that period.

strata_counts_after_collapse <- cohort_with_age_groups %>%
  count(year, half_year, sex, age_group, name = "n_hh")

halfyear_strata <- standard_period %>%
  left_join(
    strata_counts_after_collapse,
    by = c("year", "half_year", "sex", "age_group")
  ) %>%
  mutate(
    n_hh = coalesce(n_hh, 0L)
  ) %>%
  group_by(year, half_year) %>%
  mutate(
    n_period = sum(n_hh),
    obs_w = if_else(n_period > 0, n_hh / n_period, NA_real_),
    weight = case_when(
      n_period > 0 & n_hh > 0 ~ std_w / obs_w,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  arrange(year, half_year, sex, age_group)


###############################################################################
# 11) CHECK FOR REMAINING EMPTY STRATA AFTER COLLAPSING
###############################################################################
# Remaining empty strata can occur if the lowest age group is empty, because
# there is no lower age group into which it can be collapsed.

collapse_period_summary <- collapse_log %>%
  filter(action == "collapsed_to_next_lower_age_group") %>%
  group_by(year, half_year) %>%
  summarise(
    n_collapsed_age_sex_strata = n(),
    collapse_applied = n_collapsed_age_sex_strata > 0,
    .groups = "drop"
  )

halfyear_check <- halfyear_strata %>%
  mutate(
    missing_stratum_after_collapse = n_hh == 0 & std_w > 0
  ) %>%
  group_by(year, half_year) %>%
  summarise(
    n_period = first(n_period),
    n_strata_after_collapse = n(),
    n_missing_strata_after_collapse = sum(missing_stratum_after_collapse),
    valid_standardization = n_missing_strata_after_collapse == 0 & n_period > 0,
    .groups = "drop"
  ) %>%
  left_join(
    collapse_period_summary,
    by = c("year", "half_year")
  ) %>%
  mutate(
    n_collapsed_age_sex_strata = coalesce(n_collapsed_age_sex_strata, 0L),
    collapse_applied = coalesce(collapse_applied, FALSE)
  ) %>%
  arrange(year, half_year)


###############################################################################
# 12) ATTACH WEIGHTS TO PATIENT-LEVEL COHORT
###############################################################################

cohort_weighted <- cohort_with_age_groups %>%
  left_join(
    halfyear_strata %>%
      select(
        year,
        half_year,
        sex,
        age_group,
        weight,
        obs_w,
        std_w,
        n_hh,
        base_age_cats
      ),
    by = c("year", "half_year", "sex", "age_group")
  )


###############################################################################
# 13) CALCULATE STANDARDIZED COUNTS PER HALF-YEAR AND EXPOSURE GROUP
###############################################################################

standardized_counts <- cohort_weighted %>%
  group_by(year, half_year, index_exposure) %>%
  summarise(
    standardized_n = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    halfyear_check %>%
      select(year, half_year, valid_standardization),
    by = c("year", "half_year")
  ) %>%
  mutate(
    standardized_n = if_else(valid_standardization, standardized_n, NA_real_)
  ) %>%
  select(year, half_year, index_exposure, standardized_n) %>%
  arrange(year, half_year, index_exposure)


###############################################################################
# 14) WEIGHT SUM CHECK
###############################################################################

halfyear_weight_sum_check <- cohort_weighted %>%
  group_by(year, half_year) %>%
  summarise(
    n_persons = n(),
    sum_weights = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    halfyear_check,
    by = c("year", "half_year")
  ) %>%
  arrange(year, half_year)


###############################################################################
# 15) WRITE OUTPUT
###############################################################################

if (nrow(collapse_log) > 0) {
  message("Age-stratum collapsing was applied. See collapse_log in the Excel output.")
  print(collapse_log, n = Inf)
} else {
  message("No age-stratum collapsing was needed.")
}

write_xlsx(
  list(
    standardized_counts = standardized_counts,
    collapse_log = collapse_log,
    period_check = halfyear_check,
    weight_check = halfyear_weight_sum_check,
    strata_weights = halfyear_strata,
    age_group_map = age_group_map
  ),
  path = OUTPUT_FILE
)

message("Done. Output written to: ", OUTPUT_FILE)
