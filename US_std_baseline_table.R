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
# - all variables listed in COV_MAPPING_FILE where in_baseline_table == TRUE
COHORT_FILE  <- "<COHORT_COVARIATE_FILE>"
COHORT_SHEET <- "<COHORT_COVARIATE_SHEET>"  # use 1 for first sheet, or NULL for csv/rds

DATE_VAR     <- "index_date"
AGE_VAR      <- "age_years"
SEX_VAR      <- "sex"
EXPOSURE_VAR <- "initial_p2y12i"

# ---------------------------------------------------------------------------
# COVARIATE MAPPING INPUT FILE
# ---------------------------------------------------------------------------
# Required columns:
# - analytic_file_name
# - printed_name
# - data_type: "num", "cat", or "bin"
# - in_baseline_table
#
# Optional columns:
# - cat_levels_json
# - print_missing
COV_MAPPING_FILE  <- "<COVARIATE_MAPPING_EXCEL_FILE>"
COV_MAPPING_SHEET <- "<COVARIATE_MAPPING_SHEET>"

# ---------------------------------------------------------------------------
# STANDARD POPULATION INPUT FILE
# ---------------------------------------------------------------------------
# Required columns:
# - age_cat
# - sex
# - standard_population_weight
#
# If your standard population file uses another column name, for example
# "US_std_perc", change STD_WEIGHT_COL accordingly.
STD_FILE       <- "<STANDARD_POPULATION_EXCEL_FILE>"
STD_SHEET      <- "<STANDARD_POPULATION_SHEET>"
STD_WEIGHT_COL <- "standard_population_weight"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
OUTPUT_FILE <- "<STANDARDIZED_BASELINE_TABLE_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# STANDARDIZATION STRATA
# ---------------------------------------------------------------------------
AGE_LEVELS <- c("18-44", "45-64", "65-85", ">85")
SEX_LEVELS <- c("M", "F")

# ---------------------------------------------------------------------------
# TABLE OPTIONS
# ---------------------------------------------------------------------------
DECIMAL_MARK <- "."
BIG_MARK <- ","
MISSING_LABEL <- "Missing"
BIN_MODE <- "positive_only"  # "positive_only" or "both"


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
  "jsonlite",
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
library(jsonlite)
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
  
  x <- trimws(as.character(x))
  x <- gsub("=", ":", x, fixed = TRUE)
  x <- gsub("'", "\"", x, fixed = TRUE)
  x <- gsub("(\\{|,)\\s*(\\d+)\\s*:", "\\1 \"\\2\":", x, perl = TRUE)
  
  out <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
  
  if (is.null(out) || is.null(names(out))) {
    return(NULL)
  }
  
  as.list(out)
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
      standard_weight = if (convert_percent_to_proportion) {
        std_value / 100
      } else {
        std_value
      }
    ) %>%
    filter(
      !is.na(age_cat),
      !is.na(sex),
      !is.na(standard_weight)
    ) %>%
    group_by(age_cat, sex) %>%
    summarise(
      standard_weight = sum(standard_weight, na.rm = TRUE),
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
  
  if (any(is.na(std_pop$standard_weight))) {
    stop("The standard population does not contain all required age-sex strata.")
  }
  
  if (abs(sum(std_pop$standard_weight) - 1) >= 1e-8) {
    stop("The standard population weights do not sum to 1.")
  }
  
  std_pop
}

add_overall_standardization_weights <- function(
    data,
    std_pop,
    std_vars = c("age_cat", "sex"),
    new_weight_col = "std_w_overall"
) {
  observed_distribution <- data %>%
    mutate(across(all_of(std_vars), as.character)) %>%
    filter(if_all(all_of(std_vars), ~ !is.na(.x))) %>%
    count(across(all_of(std_vars)), name = "n_observed") %>%
    mutate(
      observed_weight = n_observed / sum(n_observed)
    )
  
  if (nrow(observed_distribution) == 0) {
    stop("No complete age-sex strata were available in the cohort.")
  }
  
  target_distribution <- std_pop %>%
    mutate(across(all_of(std_vars), as.character)) %>%
    group_by(across(all_of(std_vars))) %>%
    summarise(
      target_weight = sum(standard_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      target_weight = target_weight / sum(target_weight, na.rm = TRUE)
    )
  
  weight_lookup <- observed_distribution %>%
    left_join(target_distribution, by = std_vars) %>%
    mutate(
      !!new_weight_col := target_weight / observed_weight
    ) %>%
    select(all_of(std_vars), all_of(new_weight_col))
  
  data %>%
    mutate(across(all_of(std_vars), as.character)) %>%
    left_join(weight_lookup, by = std_vars)
}

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  stats::weighted.mean(x[ok], w[ok])
}

weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (sum(ok) <= 1) {
    return(NA_real_)
  }
  
  x_ok <- x[ok]
  w_ok <- w[ok] / sum(w[ok])
  mu <- sum(w_ok * x_ok)
  
  sqrt(sum(w_ok * (x_ok - mu)^2))
}

fmt_int <- function(n, big_mark = BIG_MARK) {
  n <- suppressWarnings(as.numeric(n))
  
  ifelse(
    is.na(n),
    "",
    format(n, big.mark = big_mark, scientific = FALSE, trim = TRUE)
  )
}

fmt_num <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "",
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = BIG_MARK,
      decimal.mark = DECIMAL_MARK
    )
  )
}

fmt_n_pct <- function(n, pct) {
  paste0(fmt_int(n), " (", sprintf("%.1f", pct), "%)")
}

weighted_percent <- function(x, level, w, include_missing_in_denominator = FALSE) {
  x <- as.character(x)
  is_missing <- is.na(x) | x == ""
  
  if (include_missing_in_denominator) {
    denominator <- !is.na(w) & w > 0
  } else {
    denominator <- !is_missing & !is.na(w) & w > 0
  }
  
  if (!any(denominator)) {
    return(NA_real_)
  }
  
  numerator <- denominator & x == level
  
  100 * sum(w[numerator], na.rm = TRUE) / sum(w[denominator], na.rm = TRUE)
}

make_baseline_table <- function(
    cohort,
    mapping,
    strata_var = "p2y12_group",
    weight_col = "std_w_overall"
) {
  mapping_prepped <- mapping %>%
    mutate(
      in_baseline_table = to_bool(in_baseline_table, FALSE)
    ) %>%
    filter(in_baseline_table) %>%
    mutate(
      analytic_file_name = as.character(analytic_file_name),
      printed_name = as.character(printed_name),
      printed_name = if_else(
        is.na(printed_name) | !nzchar(trimws(printed_name)),
        analytic_file_name,
        printed_name
      ),
      data_type = tolower(as.character(data_type)),
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
  
  if (!weight_col %in% names(cohort)) {
    stop("Weight column not found in cohort: ", weight_col)
  }
  
  if (!strata_var %in% names(cohort)) {
    stop("Stratification variable not found in cohort: ", strata_var)
  }
  
  cohort <- cohort %>%
    mutate(
      .weight = as.numeric(.data[[weight_col]])
    )
  
  strata_levels <- if (is.factor(cohort[[strata_var]])) {
    levels(cohort[[strata_var]])
  } else {
    sort(unique(as.character(cohort[[strata_var]][!is.na(cohort[[strata_var]])])))
  }
  
  groups <- c("Overall", strata_levels)
  
  get_group_data <- function(group_name) {
    if (group_name == "Overall") {
      return(cohort)
    }
    
    cohort %>%
      filter(as.character(.data[[strata_var]]) == group_name)
  }
  
  summarise_one_group <- function(data, variable, data_type, levels_map = NULL) {
    w <- data$.weight
    
    if (data_type == "num") {
      x <- suppressWarnings(as.numeric(data[[variable]]))
      mu <- weighted_mean(x, w)
      sd <- weighted_sd(x, w)
      
      return(paste0(fmt_num(mu), " (", fmt_num(sd), ")"))
    }
    
    if (data_type == "bin") {
      x <- as.character(data[[variable]])
      
      if (!is.null(levels_map)) {
        keys <- names(levels_map)
        positive_level <- if ("1" %in% keys) "1" else keys[[1]]
      } else {
        positive_level <- "1"
      }
      
      n_positive <- sum(x == positive_level, na.rm = TRUE)
      pct_positive <- weighted_percent(x, positive_level, w)
      
      return(fmt_n_pct(n_positive, pct_positive))
    }
    
    stop("summarise_one_group() is only used for numeric and binary rows.")
  }
  
  table_rows <- list()
  
  table_rows[[length(table_rows) + 1L]] <- tibble(
    characteristic = "N",
    !!!setNames(
      as.list(
        vapply(
          groups,
          function(group_name) fmt_int(nrow(get_group_data(group_name))),
          character(1)
        )
      ),
      groups
    )
  )
  
  for (i in seq_len(nrow(mapping_prepped))) {
    map_row <- mapping_prepped[i, ]
    variable <- map_row$analytic_file_name
    printed_name <- map_row$printed_name
    data_type <- map_row$data_type
    
    levels_map <- if ("cat_levels_json" %in% names(mapping_prepped)) {
      parse_levels(map_row$cat_levels_json)
    } else {
      NULL
    }
    
    if (data_type %in% c("num", "bin")) {
      values <- vapply(
        groups,
        function(group_name) {
          summarise_one_group(
            data = get_group_data(group_name),
            variable = variable,
            data_type = data_type,
            levels_map = levels_map
          )
        },
        character(1)
      )
      
      table_rows[[length(table_rows) + 1L]] <- tibble(
        characteristic = printed_name,
        !!!setNames(as.list(values), groups)
      )
    }
    
    if (data_type == "cat") {
      x_all <- cohort[[variable]]
      
      if (!is.null(levels_map)) {
        level_keys <- names(levels_map)
        level_labels <- unname(unlist(levels_map))
      } else if (is.factor(x_all)) {
        level_keys <- levels(x_all)
        level_labels <- levels(x_all)
      } else {
        level_keys <- sort(unique(as.character(x_all[!is.na(x_all)])))
        level_labels <- level_keys
      }
      
      table_rows[[length(table_rows) + 1L]] <- tibble(
        characteristic = printed_name,
        !!!setNames(as.list(rep("", length(groups))), groups)
      )
      
      for (j in seq_along(level_keys)) {
        level_key <- level_keys[[j]]
        level_label <- level_labels[[j]]
        
        values <- vapply(
          groups,
          function(group_name) {
            group_data <- get_group_data(group_name)
            x <- as.character(group_data[[variable]])
            w <- group_data$.weight
            
            n_level <- sum(x == level_key, na.rm = TRUE)
            pct_level <- weighted_percent(x, level_key, w)
            
            fmt_n_pct(n_level, pct_level)
          },
          character(1)
        )
        
        table_rows[[length(table_rows) + 1L]] <- tibble(
          characteristic = paste0("...", level_label),
          !!!setNames(as.list(values), groups)
        )
      }
    }
  }
  
  bind_rows(table_rows)
}


###############################################################################
# 4) READ INPUTS
###############################################################################

cohort_raw <- read_tabular_file(
  file = COHORT_FILE,
  sheet = COHORT_SHEET
)

cov_mapping <- readxl::read_excel(
  COV_MAPPING_FILE,
  sheet = COV_MAPPING_SHEET
)

standard_population_raw <- readxl::read_excel(
  STD_FILE,
  sheet = STD_SHEET
)

standard_population <- prepare_standard_population(
  std_pop_raw = standard_population_raw
)


###############################################################################
# 5) PREPARE COHORT AND ADD STANDARDIZATION WEIGHTS
###############################################################################

required_cohort_cols <- c(DATE_VAR, AGE_VAR, SEX_VAR, EXPOSURE_VAR)
missing_cohort_cols <- setdiff(required_cohort_cols, names(cohort_raw))

if (length(missing_cohort_cols) > 0) {
  stop(
    "The cohort file is missing required columns: ",
    paste(missing_cohort_cols, collapse = ", ")
  )
}

cohort_std <- cohort_raw %>%
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
    
    age_cat = factor(age_cat, levels = AGE_LEVELS),
    sex = factor(sex, levels = SEX_LEVELS),
    
    p2y12_group = factor(
      as.character(.data[[EXPOSURE_VAR]])
    )
  ) %>%
  filter(
    !is.na(index_date),
    !is.na(age_cat),
    sex %in% SEX_LEVELS,
    !is.na(p2y12_group)
  ) %>%
  add_overall_standardization_weights(
    std_pop = standard_population,
    std_vars = c("age_cat", "sex"),
    new_weight_col = "std_w_overall"
  )

if (any(is.na(cohort_std$std_w_overall))) {
  message(
    sum(is.na(cohort_std$std_w_overall)),
    " records have missing standardization weights."
  )
}


###############################################################################
# 6) CREATE AND EXPORT BASELINE TABLE
###############################################################################

baseline_table <- make_baseline_table(
  cohort = cohort_std,
  mapping = cov_mapping,
  strata_var = "p2y12_group",
  weight_col = "std_w_overall"
)

writexl::write_xlsx(
  list(
    standardized_baseline_table = baseline_table
  ),
  path = OUTPUT_FILE
)

message("Done. Standardized baseline table written to: ", OUTPUT_FILE)