###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# INPUT FILE
# ---------------------------------------------------------------------------
# Expected columns in INPUT_SHEET:
# - QUARTER_COL: quarter label, e.g. "2011-Q1"
# - OUTCOME_COL: standardized 1-year risk on the proportion scale
#                e.g. 0.052 means 5.2%
# - SE_COL: standard error of the standardized risk
# - N_COL: number of persons in the quarter
#
# The SE column is required because the model uses inverse-variance weights:
#   weight = 1 / SE^2
INPUT_FILE  <- "<QUARTERLY_STANDARDIZED_OUTCOME_EXCEL_FILE>"
INPUT_SHEET <- "<QUARTERLY_STANDARDIZED_OUTCOME_SHEET>"

QUARTER_COL <- "cohort_q"
OUTCOME_COL <- "standardized_risk_365d"
SE_COL      <- "se_standardized_risk_365d"
N_COL       <- "n"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
OUTPUT_FILE <- "<SEGMENTED_REGRESSION_RESULTS_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# OUTCOME LABEL
# ---------------------------------------------------------------------------
# Used only for documentation in the exported model data.
OUTCOME_NAME <- "<OUTCOME_NAME>"  # Example: "bleeding", "mace"

# ---------------------------------------------------------------------------
# ANALYSIS WINDOW
# ---------------------------------------------------------------------------
# The model is restricted to quarters from ANALYSIS_START through ANALYSIS_END.
ANALYSIS_START <- "2011-Q1"
ANALYSIS_END   <- "2023-Q4"

# ---------------------------------------------------------------------------
# SEGMENT DEFINITIONS
# ---------------------------------------------------------------------------
# Quarters up to and including PRE_PERIOD_END are included in the pre segment.
# Quarters from POST_PERIOD_START onward are included in the post segment.
# Quarters between these two cutoffs are excluded as a transition/gap period.
PRE_PERIOD_END    <- "2019-Q3"
POST_PERIOD_START <- "2020-Q4"

# ---------------------------------------------------------------------------
# TIME SCALE
# ---------------------------------------------------------------------------
# TIME_ORIGIN_YEAR defines the zero point for the numeric quarter index.
# With TIME_ORIGIN_YEAR = 2011:
# - 2011-Q1 has time = 0
# - 2011-Q2 has time = 1
# - etc.
TIME_ORIGIN_YEAR <- 2011

# ---------------------------------------------------------------------------
# RESULT SCALE
# ---------------------------------------------------------------------------
# The outcome is assumed to be on the proportion scale.
# Multiplying by 100 reports estimates in percentage points.
SCALE_FACTOR <- 100

# Number of decimals in the compact table.
ESTIMATE_DIGITS <- 2
P_VALUE_DIGITS  <- 3


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "dplyr",
  "stringr",
  "readxl",
  "lubridate",
  "broom",
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

library(dplyr)
library(stringr)
library(readxl)
library(lubridate)
library(broom)
library(writexl)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

parse_quarter <- function(quarter_label) {
  quarter_label <- as.character(quarter_label)
  
  year <- as.integer(stringr::str_extract(quarter_label, "^\\d{4}"))
  quarter <- as.integer(stringr::str_extract(quarter_label, "(?<=-Q)\\d$"))
  
  if (any(is.na(year) | is.na(quarter))) {
    stop("Quarter labels must have the format 'YYYY-Qn', for example '2019-Q3'.")
  }
  
  tibble(
    cohort_q = quarter_label,
    year = year,
    quarter = quarter,
    quarter_start_date = lubridate::make_date(
      year,
      (quarter - 1L) * 3L + 1L,
      1L
    ),
    time = (year - TIME_ORIGIN_YEAR) * 4L + (quarter - 1L)
  )
}

quarter_to_time <- function(quarter_label) {
  parse_quarter(quarter_label)$time[[1]]
}

linear_combo <- function(model, weights, term_label, parameter_label) {
  coefficients <- stats::coef(model)
  covariance_matrix <- stats::vcov(model)
  
  contrast <- setNames(rep(0, length(coefficients)), names(coefficients))
  contrast[names(weights)] <- weights
  
  estimate <- as.numeric(sum(contrast * coefficients))
  standard_error <- as.numeric(
    sqrt(t(contrast) %*% covariance_matrix %*% contrast)
  )
  
  residual_df <- stats::df.residual(model)
  critical_value <- stats::qt(0.975, df = residual_df)
  
  statistic <- estimate / standard_error
  p_value <- 2 * stats::pt(
    abs(statistic),
    df = residual_df,
    lower.tail = FALSE
  )
  
  tibble(
    term = term_label,
    parameter = parameter_label,
    estimate = estimate,
    std_error = standard_error,
    conf_low = estimate - critical_value * standard_error,
    conf_high = estimate + critical_value * standard_error,
    statistic = statistic,
    p_value = p_value
  )
}

format_p_value <- function(p_value, digits = P_VALUE_DIGITS) {
  dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "<0.001",
    TRUE ~ sprintf(paste0("%.", digits, "f"), p_value)
  )
}


###############################################################################
# 4) READ INPUT DATA
###############################################################################

input_data_raw <- readxl::read_excel(
  INPUT_FILE,
  sheet = INPUT_SHEET
)

required_columns <- c(QUARTER_COL, OUTCOME_COL, SE_COL, N_COL)
missing_columns <- setdiff(required_columns, names(input_data_raw))

if (length(missing_columns) > 0) {
  stop(
    "The input file is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(input_data_raw), collapse = ", ")
  )
}


###############################################################################
# 5) PREPARE ANALYSIS DATA
###############################################################################

analysis_start_time <- quarter_to_time(ANALYSIS_START)
analysis_end_time <- quarter_to_time(ANALYSIS_END)

pre_period_end_time <- quarter_to_time(PRE_PERIOD_END)
post_period_start_time <- quarter_to_time(POST_PERIOD_START)

quarter_info <- parse_quarter(input_data_raw[[QUARTER_COL]])

analysis_data <- input_data_raw %>%
  mutate(
    cohort_q = as.character(.data[[QUARTER_COL]]),
    outcome_name = OUTCOME_NAME,
    
    y = as.numeric(.data[[OUTCOME_COL]]),
    standard_error = as.numeric(.data[[SE_COL]]),
    n = as.numeric(.data[[N_COL]])
  ) %>%
  bind_cols(
    quarter_info %>%
      select(year, quarter, quarter_start_date, time)
  ) %>%
  mutate(
    model_weight = 1 / standard_error^2,
    
    valid_core_data =
      !is.na(year) &
      !is.na(quarter) &
      !is.na(quarter_start_date) &
      !is.na(time) &
      !is.na(y) &
      !is.na(standard_error) &
      !is.na(model_weight) &
      is.finite(model_weight) &
      model_weight > 0,
    
    in_analysis_window =
      valid_core_data &
      time >= analysis_start_time &
      time <= analysis_end_time,
    
    period = case_when(
      !in_analysis_window ~ NA_character_,
      time <= pre_period_end_time ~ "pre",
      time >= post_period_start_time ~ "post",
      TRUE ~ "gap"
    ),
    
    analysis_status = case_when(
      !valid_core_data ~ "excluded_invalid_outcome_se_or_weight",
      !in_analysis_window ~ "excluded_outside_analysis_window",
      period == "pre" ~ "included_pre_segment",
      period == "post" ~ "included_post_segment",
      period == "gap" ~ "excluded_transition_gap",
      TRUE ~ "excluded_unknown"
    ),
    
    analysis_included = analysis_status %in% c(
      "included_pre_segment",
      "included_post_segment"
    )
  ) %>%
  arrange(time)

model_data <- analysis_data %>%
  filter(analysis_included) %>%
  mutate(
    post = as.integer(period == "post"),
    
    # Center time at the start of the post period.
    # This makes the post coefficient interpretable as the immediate level
    # change at POST_PERIOD_START.
    time_c = time - post_period_start_time
  ) %>%
  arrange(time)

if (sum(model_data$period == "pre") < 2) {
  stop("The pre segment has fewer than two observations.")
}

if (sum(model_data$period == "post") < 2) {
  stop("The post segment has fewer than two observations.")
}


###############################################################################
# 6) FIT WEIGHTED INTERACTION MODEL
###############################################################################
# Model:
#   y ~ time_c * post
#
# Because y is on the proportion scale, model coefficients are also on the
# proportion scale. They are multiplied by SCALE_FACTOR in the compact output.

interaction_model <- stats::lm(
  y ~ time_c * post,
  data = model_data,
  weights = model_weight
)


###############################################################################
# 7) EXTRACT MODEL COEFFICIENTS
###############################################################################

coefficient_table <- broom::tidy(
  interaction_model,
  conf.int = TRUE
) %>%
  mutate(
    parameter = case_when(
      term == "(Intercept)" ~
        paste0("Projected pre-intervention level at ", POST_PERIOD_START, " (%)"),
      
      term == "time_c" ~
        "Pre-intervention trend (%/quarter)",
      
      term == "post" ~
        paste0("Immediate level change at ", POST_PERIOD_START, " (%)"),
      
      term == "time_c:post" ~
        paste0("Trend change after ", POST_PERIOD_START, " (%/quarter)"),
      
      TRUE ~ term
    )
  ) %>%
  transmute(
    term,
    parameter,
    estimate,
    std_error = std.error,
    conf_low = conf.low,
    conf_high = conf.high,
    statistic,
    p_value = p.value
  )


###############################################################################
# 8) DERIVED ESTIMATES
###############################################################################
# Derived estimates are calculated as linear combinations of model coefficients.

derived_table <- bind_rows(
  linear_combo(
    model = interaction_model,
    weights = c("time_c" = 1),
    term_label = "pre_slope",
    parameter_label = "Pre-intervention trend (%/quarter)"
  ),
  
  linear_combo(
    model = interaction_model,
    weights = c("time_c:post" = 1),
    term_label = "slope_change",
    parameter_label = paste0(
      "Trend change after ",
      POST_PERIOD_START,
      " (%/quarter)"
    )
  ),
  
  linear_combo(
    model = interaction_model,
    weights = c("time_c" = 1, "time_c:post" = 1),
    term_label = "post_slope",
    parameter_label = "Post-intervention trend (%/quarter)"
  ),
  
  linear_combo(
    model = interaction_model,
    weights = c("time_c" = 4),
    term_label = "pre_slope_per_year",
    parameter_label = "Pre-intervention trend (%/year)"
  ),
  
  linear_combo(
    model = interaction_model,
    weights = c("time_c" = 4, "time_c:post" = 4),
    term_label = "post_slope_per_year",
    parameter_label = "Post-intervention trend (%/year)"
  )
)


###############################################################################
# 9) FULL MODEL TABLE
###############################################################################

full_model_table <- bind_rows(
  coefficient_table,
  derived_table
) %>%
  mutate(
    estimate = as.numeric(estimate),
    std_error = as.numeric(std_error),
    conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high),
    statistic = as.numeric(statistic),
    p_value = as.numeric(p_value)
  )


###############################################################################
# 10) COMPACT PUBLICATION TABLE
###############################################################################

compact_table <- full_model_table %>%
  mutate(
    estimate_pct = estimate * SCALE_FACTOR,
    conf_low_pct = conf_low * SCALE_FACTOR,
    conf_high_pct = conf_high * SCALE_FACTOR,
    
    `Estimate [LCL - UCL]` = paste0(
      sprintf(paste0("%.", ESTIMATE_DIGITS, "f"), estimate_pct),
      " [",
      sprintf(paste0("%.", ESTIMATE_DIGITS, "f"), conf_low_pct),
      " - ",
      sprintf(paste0("%.", ESTIMATE_DIGITS, "f"), conf_high_pct),
      "]"
    ),
    
    `p-value` = format_p_value(p_value)
  ) %>%
  select(
    `Formula term` = term,
    Parameter = parameter,
    `Estimate [LCL - UCL]`,
    `p-value`
  )

print(compact_table, n = Inf)


###############################################################################
# 11) MODEL FIT TABLE
###############################################################################

model_fit_table <- broom::glance(interaction_model) %>%
  transmute(
    nobs = nobs,
    r_squared = r.squared,
    adj_r_squared = adj.r.squared,
    sigma = sigma,
    p_value_model = p.value,
    AIC = AIC,
    BIC = BIC
  )

print(model_fit_table)


###############################################################################
# 12) EXPORT RESULTS
###############################################################################

write_xlsx(
  list(
    compact_table = compact_table,
    full_model_table = full_model_table,
    model_fit = model_fit_table,
    model_data = model_data,
    excluded_rows = analysis_data %>%
      filter(!analysis_included)
  ),
  path = OUTPUT_FILE
)

message("Done. Segmented regression results written to: ", OUTPUT_FILE)