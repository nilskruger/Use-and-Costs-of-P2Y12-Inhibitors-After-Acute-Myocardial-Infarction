###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# DATABASE INPUT
# ---------------------------------------------------------------------------
# The cohort table should contain one row per cohort member.
#
# Required cohort columns:
# - DATE_VAR: index date
# - AGE_VAR: age in years
# - SEX_VAR: sex indicator or label
# - EXPOSURE_VAR: initial P2Y12 inhibitor group
# - all variables listed in the covariate mapping file where
#   in_baseline_table == TRUE
#
# Expected sex coding:
# - 0 or M/Male/Men = male
# - 1 or F/Female/Women = female
ODBC_DSN <- "<ODBC_DSN>"

COHORT_TABLE <- "<DATABASE>.<SCHEMA>.<COHORT_TABLE>"

DATE_VAR     <- "index_date"
AGE_VAR      <- "age_years"
SEX_VAR      <- "gender"
EXPOSURE_VAR <- "initial_p2y12i"

# ---------------------------------------------------------------------------
# COVARIATE MAPPING INPUT FILE
# ---------------------------------------------------------------------------
# Required columns in COV_MAPPING_FILE:
# - analytic_file_name: variable name in the cohort table
# - printed_name: label shown in the baseline table
# - data_type: one of "num", "cat", or "bin"
# - in_baseline_table: TRUE/FALSE indicator
#
# Optional columns:
# - cat_levels_json: JSON-style mapping of coded values to printed labels
# - print_missing: TRUE/FALSE indicator for whether missing values should print
COV_MAPPING_FILE  <- "<COVARIATE_MAPPING_EXCEL_FILE>"
COV_MAPPING_SHEET <- "<COVARIATE_MAPPING_SHEET>"

# ---------------------------------------------------------------------------
# STANDARD POPULATION INPUT FILE
# ---------------------------------------------------------------------------
# Required columns in STD_SHEET:
# - age_cat: age category, e.g. "18-44", "45-64", "65-85", ">85"
# - sex: sex category, e.g. "M"/"F", "Male"/"Female", "Men"/"Women"
# - standard_population_weight: percentage or proportion
#
# If percentages are provided, e.g. 12.5, they are converted to proportions.
# If proportions are provided, e.g. 0.125, they are used as-is.
STD_FILE  <- "<STANDARD_POPULATION_EXCEL_FILE>"
STD_SHEET <- "<STANDARD_POPULATION_SHEET>"

STD_WEIGHT_COL <- "standard_population_weight"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
OUTPUT_FILE <- "<BASELINE_TABLE_OUTPUT_EXCEL_FILE>"

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
  "purrr",
  "readxl",
  "writexl",
  "lubridate",
  "jsonlite"
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
library(purrr)
library(readxl)
library(writexl)
library(lubridate)
library(jsonlite)


###############################################################################
# 3) HELPER FUNCTIONS FOR STANDARDIZATION
###############################################################################

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
    TRUE ~ x_up
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
      standard_population_weight = if (convert_percent_to_proportion) {
        std_value / 100
      } else {
        std_value
      }
    ) %>%
    filter(
      !is.na(age_cat),
      !is.na(sex),
      !is.na(standard_population_weight)
    ) %>%
    group_by(age_cat, sex) %>%
    summarise(
      standard_population_weight = sum(standard_population_weight, na.rm = TRUE),
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
  
  if (any(is.na(std_pop$standard_population_weight))) {
    stop("The standard population does not contain all required age x sex strata.")
  }
  
  if (abs(sum(std_pop$standard_population_weight) - 1) >= 1e-8) {
    stop("The standard population weights do not sum to 1.")
  }
  
  std_pop
}

add_overall_standardization_weights <- function(
    df,
    std_pop,
    std_vars = c("age_cat", "sex"),
    std_weight_col = "standard_population_weight",
    new_weight_col = "std_w_overall"
) {
  missing_df_cols <- setdiff(std_vars, names(df))
  missing_std_cols <- setdiff(c(std_vars, std_weight_col), names(std_pop))
  
  if (length(missing_df_cols) > 0) {
    stop(
      "Missing standardization columns in cohort: ",
      paste(missing_df_cols, collapse = ", ")
    )
  }
  
  if (length(missing_std_cols) > 0) {
    stop(
      "Missing columns in standard population: ",
      paste(missing_std_cols, collapse = ", ")
    )
  }
  
  target_distribution <- std_pop %>%
    mutate(
      across(all_of(std_vars), as.character),
      target_proportion = as.numeric(.data[[std_weight_col]])
    ) %>%
    select(all_of(std_vars), target_proportion) %>%
    group_by(across(all_of(std_vars))) %>%
    summarise(
      target_proportion = sum(target_proportion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      target_proportion = target_proportion / sum(target_proportion, na.rm = TRUE)
    )
  
  observed_distribution <- df %>%
    mutate(across(all_of(std_vars), as.character)) %>%
    filter(if_all(all_of(std_vars), ~ !is.na(.x))) %>%
    count(across(all_of(std_vars)), name = "n_observed") %>%
    mutate(
      observed_proportion = n_observed / sum(n_observed)
    )
  
  if (nrow(observed_distribution) == 0) {
    stop("No complete age-sex strata were available in the cohort.")
  }
  
  absent_target_strata <- target_distribution %>%
    filter(target_proportion > 0) %>%
    anti_join(observed_distribution, by = std_vars)
  
  if (nrow(absent_target_strata) > 0) {
    message(
      "The following target standardization strata are absent in the cohort. ",
      "No stratum collapsing is applied in this script: ",
      paste(
        apply(absent_target_strata[, std_vars, drop = FALSE], 1, paste, collapse = "/"),
        collapse = ", "
      )
    )
  }
  
  observed_without_target <- observed_distribution %>%
    anti_join(target_distribution, by = std_vars)
  
  if (nrow(observed_without_target) > 0) {
    message(
      "The following cohort strata are not present in the standard population ",
      "and will receive missing weights: ",
      paste(
        apply(observed_without_target[, std_vars, drop = FALSE], 1, paste, collapse = "/"),
        collapse = ", "
      )
    )
  }
  
  weight_lookup <- observed_distribution %>%
    left_join(target_distribution, by = std_vars) %>%
    mutate(
      !!new_weight_col := target_proportion / observed_proportion
    ) %>%
    select(all_of(std_vars), all_of(new_weight_col))
  
  out <- df %>%
    mutate(across(all_of(std_vars), as.character)) %>%
    left_join(weight_lookup, by = std_vars)
  
  missing_std_any <- apply(is.na(out[, std_vars, drop = FALSE]), 1, any)
  out[[new_weight_col]][missing_std_any] <- NA_real_
  
  out
}


###############################################################################
# 4) LOAD COHORT, COVARIATE MAPPING, AND STANDARD POPULATION
###############################################################################

cohort_query <- paste0("SELECT * FROM ", COHORT_TABLE)

connection <- dbConnect(odbc::odbc(), ODBC_DSN)

cohort_raw <- tryCatch(
  dbGetQuery(connection, cohort_query),
  finally = dbDisconnect(connection)
)

cov_mapping <- read_excel(
  COV_MAPPING_FILE,
  sheet = COV_MAPPING_SHEET
)

standard_population_raw <- read_excel(
  STD_FILE,
  sheet = STD_SHEET
)

standard_population <- prepare_standard_population(
  std_pop_raw = standard_population_raw,
  age_levels = AGE_LEVELS,
  sex_levels = SEX_LEVELS,
  std_weight_col = STD_WEIGHT_COL
)


###############################################################################
# 5) BUILD ANALYTIC COHORT WITH AGE, SEX, AND P2Y12 GROUP
###############################################################################

required_cohort_cols <- c(DATE_VAR, AGE_VAR, SEX_VAR, EXPOSURE_VAR)
missing_cohort_cols <- setdiff(required_cohort_cols, names(cohort_raw))

if (length(missing_cohort_cols) > 0) {
  stop(
    "The cohort table is missing required columns: ",
    paste(missing_cohort_cols, collapse = ", ")
  )
}

cohort_std <- cohort_raw %>%
  mutate(
    index_date = as.Date(.data[[DATE_VAR]]),
    index_year = lubridate::year(index_date),
    
    age_years = suppressWarnings(as.numeric(.data[[AGE_VAR]])),
    sex_raw = trimws(as.character(.data[[SEX_VAR]])),
    
    age_cat = case_when(
      is.na(age_years) ~ NA_character_,
      age_years >= 18 & age_years <= 44 ~ "18-44",
      age_years >= 45 & age_years <= 64 ~ "45-64",
      age_years >= 65 & age_years <= 85 ~ "65-85",
      age_years > 85 ~ ">85",
      TRUE ~ NA_character_
    ),
    
    sex = normalize_sex(sex_raw),
    
    p2y12_group = factor(.data[[EXPOSURE_VAR]]),
    
    age_cat = factor(age_cat, levels = AGE_LEVELS),
    sex = factor(sex, levels = SEX_LEVELS)
  ) %>%
  filter(
    sex %in% SEX_LEVELS
  )


###############################################################################
# 6) ADD OVERALL AGE-SEX STANDARDIZATION WEIGHTS
###############################################################################
# These weights are calculated once in the overall cohort.
# They are then reused in the baseline table stratified by P2Y12 group.
#
# Formula:
# standardization weight =
#   target age-sex proportion / observed age-sex proportion

cohort_std <- cohort_std %>%
  add_overall_standardization_weights(
    std_pop = standard_population,
    std_vars = c("age_cat", "sex"),
    std_weight_col = "standard_population_weight",
    new_weight_col = "std_w_overall"
  )

n_missing_weight <- sum(is.na(cohort_std$std_w_overall))

if (n_missing_weight > 0) {
  message(
    n_missing_weight,
    " patients have missing overall standardization weights because age_cat or ",
    "sex was missing or could not be matched to the standard population."
  )
}

n_missing_p2y12_group <- sum(is.na(cohort_std$p2y12_group))

if (n_missing_p2y12_group > 0) {
  message(
    n_missing_p2y12_group,
    " patients have missing p2y12_group. They will be included in the Overall ",
    "column but will not appear in a P2Y12-group-specific column."
  )
}


###############################################################################
# 7) HELPER FUNCTIONS FOR BASELINE TABLE
###############################################################################

`%||%` <- function(a, b) if (!is.null(a)) a else b

to_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  
  toupper(as.character(x)) %in% c("TRUE", "T", "1", "YES", "Y")
}

parse_levels <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(NULL)
  }
  
  s <- trimws(as.character(x))
  s2 <- gsub("=", ":", s, fixed = TRUE)
  s2 <- gsub("'", "\"", s2, fixed = TRUE)
  s2 <- gsub("(\\{|,)\\s*(\\d+)\\s*:", "\\1 \"\\2\":", s2, perl = TRUE)
  
  out <- tryCatch(jsonlite::fromJSON(s2), error = function(e) NULL)
  
  if (is.null(out) || is.null(names(out))) {
    return(NULL)
  }
  
  as.list(out)
}

adjust_percents_to_100 <- function(raw_percent, decimals = 1) {
  raw_percent <- as.numeric(raw_percent)
  raw_percent[is.na(raw_percent)] <- 0
  
  if (length(raw_percent) == 0) {
    return(raw_percent)
  }
  
  factor <- 10^decimals
  scaled <- raw_percent * factor
  floored <- floor(scaled + 1e-12)
  remainder <- scaled - floored
  
  target <- as.integer(round(100 * factor))
  delta_units <- target - as.integer(sum(floored))
  
  adjusted <- floored
  
  if (delta_units > 0) {
    idx <- order(remainder, decreasing = TRUE)
    take <- seq_len(min(delta_units, length(adjusted)))
    adjusted[idx[take]] <- adjusted[idx[take]] + 1
  } else if (delta_units < 0) {
    idx <- order(remainder, decreasing = FALSE)
    take <- seq_len(min(abs(delta_units), length(adjusted)))
    adjusted[idx[take]] <- adjusted[idx[take]] - 1
  }
  
  as.numeric(adjusted / factor)
}

fmt_int <- function(n, big_mark = ",") {
  n <- suppressWarnings(as.numeric(n))
  
  ifelse(
    is.na(n),
    "",
    format(n, big.mark = big_mark, scientific = FALSE, trim = TRUE)
  )
}

fmt_n_pct <- function(n, pct, big_mark = ",") {
  paste0(fmt_int(n, big_mark), " (", sprintf("%.1f", pct), "%)")
}

fmt_num <- function(x, digits = 1, dec_mark = ".", big_mark = ",") {
  ifelse(
    is.na(x),
    "",
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = big_mark,
      decimal.mark = dec_mark
    )
  )
}


###############################################################################
# 8) BASELINE TABLE FUNCTION
###############################################################################
# This function creates:
# - one unweighted N row;
# - weighted summaries for numeric variables;
# - weighted percentages for categorical and binary variables;
# - an Overall column plus one column per P2Y12 group.
#
# In this script, the function uses the already-created std_w_overall column.
# It does not recompute standardization weights within exposure groups.

make_baseline_table <- function(
    cohort,
    mapping,
    strata_vars = NULL,
    weight_col = "std_w_overall",
    digits_num = 1,
    dec_mark = ".",
    big_mark = ",",
    missing_label = "Missing",
    bin_mode = c("positive_only", "both"),
    show_bin_level_label = TRUE
) {
  bin_mode <- match.arg(bin_mode)
  strata_vars <- strata_vars %||% character(0)
  
  if (!weight_col %in% names(cohort)) {
    stop("weight_col not found in cohort: ", weight_col)
  }
  
  w_mean <- function(x, w) {
    ok <- !(is.na(x) | is.na(w) | w <= 0)
    
    if (!any(ok)) {
      return(NA_real_)
    }
    
    stats::weighted.mean(x[ok], w[ok])
  }
  
  w_sd <- function(x, w) {
    ok <- !(is.na(x) | is.na(w) | w <= 0)
    
    if (sum(ok) <= 1) {
      return(NA_real_)
    }
    
    x_ok <- x[ok]
    w_ok <- w[ok] / sum(w[ok])
    mu <- sum(w_ok * x_ok)
    
    sqrt(sum(w_ok * (x_ok - mu)^2))
  }
  
  w_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
    ok <- !(is.na(x) | is.na(w) | w <= 0)
    
    if (!any(ok)) {
      return(rep(NA_real_, length(probs)))
    }
    
    x_ok <- x[ok]
    w_ok <- w[ok]
    
    ord <- order(x_ok)
    x_ok <- x_ok[ord]
    w_ok <- w_ok[ord]
    
    cumulative_weight <- cumsum(w_ok) / sum(w_ok)
    
    vapply(
      probs,
      function(p) {
        x_ok[which(cumulative_weight >= p)[1]]
      },
      numeric(1)
    )
  }
  
  mapping_prepped <- mapping %>%
    mutate(
      in_baseline_table = to_bool(in_baseline_table, FALSE)
    ) %>%
    filter(in_baseline_table) %>%
    mutate(
      data_type = tolower(as.character(data_type)),
      analytic_file_name = as.character(analytic_file_name),
      printed_name = as.character(printed_name),
      printed_name = if_else(
        is.na(printed_name) | !nzchar(trimws(printed_name)),
        analytic_file_name,
        printed_name
      ),
      print_missing = if ("print_missing" %in% names(.)) {
        to_bool(print_missing, FALSE)
      } else {
        FALSE
      }
    )
  
  missing_baseline_cols <- setdiff(mapping_prepped$analytic_file_name, names(cohort))
  
  if (length(missing_baseline_cols) > 0) {
    stop(
      "Missing baseline table columns in cohort: ",
      paste(missing_baseline_cols, collapse = ", ")
    )
  }
  
  missing_strata_cols <- setdiff(strata_vars, names(cohort))
  
  if (length(missing_strata_cols) > 0) {
    stop(
      "Missing strata variables in cohort: ",
      paste(missing_strata_cols, collapse = ", ")
    )
  }
  
  groups <- list(
    list(
      label = "Overall",
      data = cohort
    )
  )
  
  for (strata_var in strata_vars) {
    tmp <- cohort %>%
      mutate(.stratum = as.character(.data[[strata_var]]))
    
    if (is.factor(cohort[[strata_var]])) {
      levels_to_print <- levels(cohort[[strata_var]])
      levels_to_print <- levels_to_print[levels_to_print %in% tmp$.stratum]
    } else {
      levels_to_print <- sort(unique(tmp$.stratum[!is.na(tmp$.stratum)]))
    }
    
    for (level_value in levels_to_print) {
      groups <- append(
        groups,
        list(
          list(
            label = paste0(strata_var, "=", level_value),
            data = tmp %>%
              filter(.stratum == level_value) %>%
              select(-.stratum)
          )
        )
      )
    }
  }
  
  groups <- purrr::map(
    groups,
    function(g) {
      g$data$.__std_w__ <- as.numeric(g$data[[weight_col]])
      g
    }
  )
  
  n_long <- purrr::map_dfr(
    groups,
    ~ tibble(
      row_id = "0__N",
      characteristic = "N",
      group = .x$label,
      value = fmt_int(nrow(.x$data), big_mark),
      map_order = 0L,
      within_order = 0L
    )
  )
  
  summarize_num_block <- function(df, printed_name, var, map_order, print_missing) {
    x_all <- suppressWarnings(as.numeric(df[[var]]))
    w_all <- df$.__std_w__
    
    miss_n <- sum(is.na(x_all))
    nonmissing_x <- !is.na(x_all)
    ok_weight <- nonmissing_x & !is.na(w_all) & w_all > 0
    
    x <- x_all[ok_weight]
    w <- w_all[ok_weight]
    
    mean_val <- if (length(x) > 0) w_mean(x, w) else NA_real_
    sd_val <- if (length(x) > 1) w_sd(x, w) else NA_real_
    
    quantiles <- if (length(x) > 0) {
      w_quantile(x, w, probs = c(0.25, 0.5, 0.75))
    } else {
      c(NA_real_, NA_real_, NA_real_)
    }
    
    q1_val <- as.numeric(quantiles[1])
    median_val <- as.numeric(quantiles[2])
    q3_val <- as.numeric(quantiles[3])
    
    x_observed <- x_all[nonmissing_x]
    min_val <- if (length(x_observed) > 0) min(x_observed) else NA_real_
    max_val <- if (length(x_observed) > 0) max(x_observed) else NA_real_
    
    base <- tibble(
      row_id = c(
        paste0(map_order, "__", var, "__hdr"),
        paste0(map_order, "__", var, "__mean_sd"),
        paste0(map_order, "__", var, "__median_iqr"),
        paste0(map_order, "__", var, "__min_max")
      ),
      characteristic = c(
        printed_name,
        "...mean (sd)",
        "...median [Q1-Q3]",
        "...min, max"
      ),
      value = c(
        "",
        ifelse(
          is.na(mean_val) & is.na(sd_val),
          "",
          paste0(
            fmt_num(mean_val, digits_num, dec_mark, big_mark),
            " (",
            fmt_num(sd_val, digits_num, dec_mark, big_mark),
            ")"
          )
        ),
        ifelse(
          is.na(median_val) & is.na(q1_val) & is.na(q3_val),
          "",
          paste0(
            fmt_num(median_val, digits_num, dec_mark, big_mark),
            " [",
            fmt_num(q1_val, digits_num, dec_mark, big_mark),
            "-",
            fmt_num(q3_val, digits_num, dec_mark, big_mark),
            "]"
          )
        ),
        ifelse(
          is.na(min_val) & is.na(max_val),
          "",
          paste0(
            fmt_num(min_val, digits_num, dec_mark, big_mark),
            ", ",
            fmt_num(max_val, digits_num, dec_mark, big_mark)
          )
        )
      ),
      map_order = map_order,
      within_order = c(0L, 1L, 2L, 3L)
    )
    
    if (isTRUE(print_missing)) {
      denominator_weight <- sum(w_all[!is.na(w_all) & w_all > 0])
      
      missing_percent <- if (denominator_weight > 0) {
        100 * sum(w_all[is.na(x_all) & !is.na(w_all) & w_all > 0]) /
          denominator_weight
      } else {
        0
      }
      
      missing_row <- tibble(
        row_id = paste0(map_order, "__", var, "__missing"),
        characteristic = paste0("...", missing_label),
        value = fmt_n_pct(miss_n, round(missing_percent, 1), big_mark),
        map_order = map_order,
        within_order = 4L
      )
      
      bind_rows(base, missing_row)
    } else {
      base
    }
  }
  
  summarize_cat_block <- function(
    df,
    printed_name,
    var,
    map_order,
    levels_map,
    print_missing
  ) {
    x <- df[[var]]
    w_all <- df$.__std_w__
    
    x_chr <- as.character(x)
    x_chr <- ifelse(is.na(x_chr), NA_character_, trimws(x_chr))
    
    is_missing <- is.na(x_chr) | x_chr == ""
    missing_n <- sum(is_missing, na.rm = TRUE)
    
    if (isTRUE(print_missing)) {
      denominator_weight <- sum(w_all[!is.na(w_all) & w_all > 0])
    } else {
      denominator_weight <- sum(w_all[!is_missing & !is.na(w_all) & w_all > 0])
    }
    
    if (!is.null(levels_map)) {
      keys <- names(levels_map)
      labels <- unname(unlist(levels_map))
    } else if (is.factor(x)) {
      keys <- levels(x)
      labels <- keys
    } else {
      keys <- sort(unique(x_chr[!is_missing]))
      labels <- keys
    }
    
    counts <- vapply(
      keys,
      function(k) sum(x_chr == k, na.rm = TRUE),
      integer(1)
    )
    
    raw_percent <- vapply(
      keys,
      function(k) {
        if (denominator_weight <= 0) {
          return(0)
        }
        
        idx <- !is_missing & x_chr == k & !is.na(w_all) & w_all > 0
        
        100 * sum(w_all[idx], na.rm = TRUE) / denominator_weight
      },
      numeric(1)
    )
    
    counts2 <- counts
    labels2 <- labels
    raw2 <- raw_percent
    
    if (isTRUE(print_missing)) {
      missing_weight_percent <- if (denominator_weight > 0) {
        100 * sum(w_all[is_missing & !is.na(w_all) & w_all > 0], na.rm = TRUE) /
          denominator_weight
      } else {
        0
      }
      
      counts2 <- c(counts2, missing_n)
      labels2 <- c(labels2, missing_label)
      raw2 <- c(raw2, missing_weight_percent)
    }
    
    percent <- adjust_percents_to_100(raw2, decimals = 1)
    
    tibble(
      row_id = c(
        paste0(map_order, "__", var, "__hdr"),
        paste0(map_order, "__", var, "__lvl__", seq_along(labels2))
      ),
      characteristic = c(
        printed_name,
        paste0("...", labels2)
      ),
      value = c(
        "",
        purrr::map2_chr(
          as.integer(counts2),
          percent,
          ~ fmt_n_pct(.x, .y, big_mark)
        )
      ),
      map_order = map_order,
      within_order = c(0L, seq_along(labels2))
    )
  }
  
  summarize_bin_block <- function(df, printed_name, var, map_order, levels_map) {
    x <- as.character(df[[var]])
    w_all <- df$.__std_w__
    
    x <- ifelse(is.na(x), NA_character_, trimws(x))
    is_missing <- is.na(x) | x == ""
    
    denominator_weight <- sum(w_all[!is_missing & !is.na(w_all) & w_all > 0])
    
    if (bin_mode == "both") {
      if (!is.null(levels_map)) {
        keys <- names(levels_map)
        labels <- unname(unlist(levels_map))
      } else {
        keys <- sort(unique(x[!is_missing]))
        labels <- keys
      }
      
      counts <- vapply(
        keys,
        function(k) sum(x == k, na.rm = TRUE),
        integer(1)
      )
      
      raw_percent <- vapply(
        keys,
        function(k) {
          if (denominator_weight <= 0) {
            return(0)
          }
          
          idx <- !is_missing & x == k & !is.na(w_all) & w_all > 0
          
          100 * sum(w_all[idx], na.rm = TRUE) / denominator_weight
        },
        numeric(1)
      )
      
      percent <- adjust_percents_to_100(raw_percent, decimals = 1)
      
      return(
        tibble(
          row_id = c(
            paste0(map_order, "__", var, "__hdr"),
            paste0(map_order, "__", var, "__lvl__", seq_along(labels))
          ),
          characteristic = c(
            printed_name,
            paste0("...", labels)
          ),
          value = c(
            "",
            purrr::map2_chr(
              as.integer(counts),
              percent,
              ~ fmt_n_pct(.x, .y, big_mark)
            )
          ),
          map_order = map_order,
          within_order = c(0L, seq_along(labels))
        )
      )
    }
    
    # positive_only mode:
    # Show only the positive binary level.
    # The displayed percentage is not forced to 100 because only one level is shown.
    if (!is.null(levels_map)) {
      keys <- names(levels_map)
      labels <- unname(unlist(levels_map))
      
      positive_index <- if ("1" %in% keys) {
        which(keys == "1")[1]
      } else {
        1
      }
      
      positive_key <- keys[positive_index]
      positive_label <- labels[positive_index]
    } else {
      candidate_positive_values <- c(
        "1", "TRUE", "True", "true",
        "YES", "Yes", "yes", "Y"
      )
      
      positive_key <- intersect(candidate_positive_values, unique(x[!is_missing]))[1] %||%
        sort(unique(x[!is_missing]))[1] %||%
        "1"
      
      positive_label <- positive_key
    }
    
    positive_n <- sum(x == positive_key, na.rm = TRUE)
    
    positive_percent <- if (denominator_weight > 0) {
      idx <- !is_missing & x == positive_key & !is.na(w_all) & w_all > 0
      100 * sum(w_all[idx], na.rm = TRUE) / denominator_weight
    } else {
      0
    }
    
    label_suffix <- if (isTRUE(show_bin_level_label)) {
      paste0(" (", positive_label, ")")
    } else {
      ""
    }
    
    tibble(
      row_id = paste0(map_order, "__", var, "__bin_main"),
      characteristic = paste0(printed_name, label_suffix),
      value = fmt_n_pct(positive_n, round(positive_percent, 1), big_mark),
      map_order = map_order,
      within_order = 0L
    )
  }
  
  long_vars <- purrr::map_dfr(
    groups,
    function(g) {
      label <- g$label
      df <- g$data
      
      purrr::map_dfr(
        seq_len(nrow(mapping_prepped)),
        function(i) {
          row <- mapping_prepped[i, ]
          
          var <- row$analytic_file_name
          printed <- row$printed_name
          dtype <- row$data_type
          map_order <- i
          
          levels_map <- if ("cat_levels_json" %in% names(row)) {
            parse_levels(row$cat_levels_json)
          } else {
            NULL
          }
          
          block <- if (dtype == "num") {
            summarize_num_block(
              df = df,
              printed_name = printed,
              var = var,
              map_order = map_order,
              print_missing = row$print_missing
            )
          } else if (dtype == "cat") {
            summarize_cat_block(
              df = df,
              printed_name = printed,
              var = var,
              map_order = map_order,
              levels_map = levels_map,
              print_missing = row$print_missing
            )
          } else if (dtype == "bin") {
            summarize_bin_block(
              df = df,
              printed_name = printed,
              var = var,
              map_order = map_order,
              levels_map = levels_map
            )
          } else {
            stop("Unknown data_type: ", dtype, " for ", var)
          }
          
          block %>%
            mutate(group = label)
        }
      )
    }
  )
  
  long_all <- bind_rows(n_long, long_vars)
  
  long_all %>%
    select(row_id, map_order, within_order, characteristic, group, value) %>%
    pivot_wider(
      id_cols = c(row_id, map_order, within_order, characteristic),
      names_from = group,
      values_from = value
    ) %>%
    arrange(map_order, within_order) %>%
    select(characteristic, everything(), -row_id, -map_order, -within_order)
}


###############################################################################
# 9) CREATE FINAL BASELINE TABLE
###############################################################################

baseline_table_p2y12 <- make_baseline_table(
  cohort = cohort_std,
  mapping = cov_mapping,
  strata_vars = "p2y12_group",
  weight_col = "std_w_overall",
  dec_mark = ".",
  big_mark = ",",
  bin_mode = "positive_only",
  show_bin_level_label = TRUE
)


###############################################################################
# 10) WRITE OUTPUT
###############################################################################

write_xlsx(
  baseline_table_p2y12,
  path = OUTPUT_FILE
)

message("Done. Baseline table written to: ", OUTPUT_FILE)