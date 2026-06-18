###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# DATABASE INPUT
# ---------------------------------------------------------------------------
# Required columns in OUTCOME_TABLE:
# - ID_COL: unique person identifier
# - DATE_COL: cohort index date
# - EVENT_COL: binary 1-year outcome indicator
# - AGE_COL: age in years at cohort entry
# - SEX_COL: sex category or sex indicator
#
# Expected event coding:
# - 1 / TRUE / Yes / Y = event
# - 0 / FALSE / No / N = no event
#
# Expected sex coding:
# - 0 or M/Male/Men = male
# - 1 or F/Female/Women = female
ODBC_DSN <- "<ODBC_DSN>"

OUTCOME_TABLE <- "<DATABASE>.<SCHEMA>.<OUTCOME_TABLE>"

ID_COL    <- "person_id"
DATE_COL  <- "index_date"
EVENT_COL <- "outcome_1y"
AGE_COL   <- "age_years"
SEX_COL   <- "sex"

# ---------------------------------------------------------------------------
# OUTCOME SETTINGS
# ---------------------------------------------------------------------------
# This label is written to the output and helps distinguish results when the
# same script is run for multiple outcomes.
OUTCOME_NAME <- "<OUTCOME_NAME>"  # Example: "bleeding", "mace"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
# This file is used as input for the plotting script.
OUTPUT_FILE <- "<QUARTERLY_STANDARDIZED_OUTCOME_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# AGE-SEX STANDARDIZATION STRATA
# ---------------------------------------------------------------------------
# The original script used 10-year age bands up to 100+.
# Use coarser age bands if many quarter-specific strata are empty.
AGE_BREAKS <- c(seq(0, 100, by = 10), Inf)

# Optional study-period restriction. Set to NULL to keep all available years.
STUDY_START_YEAR <- 2011
STUDY_END_YEAR   <- 2023


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "odbc",
  "DBI",
  "dplyr",
  "tidyr",
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
library(writexl)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

to_binary_01 <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(as.integer(x != 0))
  }
  
  x_clean <- tolower(trimws(as.character(x)))
  
  as.integer(x_clean %in% c("1", "y", "yes", "ja", "true", "t"))
}

normalize_sex <- function(x) {
  x_clean <- toupper(trimws(as.character(x)))
  
  dplyr::case_when(
    x_clean %in% c("0", "M", "MALE", "MEN", "MAN") ~ "M",
    x_clean %in% c("1", "F", "FEMALE", "WOMEN", "WOMAN") ~ "F",
    TRUE ~ NA_character_
  )
}

make_quarter_label <- function(date_value) {
  year_value <- as.integer(format(date_value, "%Y"))
  month_value <- as.integer(format(date_value, "%m"))
  quarter_value <- ((month_value - 1L) %/% 3L) + 1L
  
  sprintf("%d-Q%d", year_value, quarter_value)
}


###############################################################################
# 4) LOAD OUTCOME TABLE
###############################################################################

outcome_query <- paste0("SELECT * FROM ", OUTCOME_TABLE)

connection <- DBI::dbConnect(odbc::odbc(), ODBC_DSN)

outcome_raw <- tryCatch(
  DBI::dbGetQuery(connection, outcome_query),
  finally = DBI::dbDisconnect(connection)
)

required_cols <- c(ID_COL, DATE_COL, EVENT_COL, AGE_COL, SEX_COL)
missing_cols <- setdiff(required_cols, names(outcome_raw))

if (length(missing_cols) > 0) {
  stop(
    "The outcome table is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


###############################################################################
# 5) PREPARE ANALYTIC DATASET
###############################################################################

analytic_data <- outcome_raw %>%
  transmute(
    person_id = .data[[ID_COL]],
    index_date = as.Date(.data[[DATE_COL]]),
    event_1y = to_binary_01(.data[[EVENT_COL]]),
    age_years = suppressWarnings(as.numeric(.data[[AGE_COL]])),
    sex = normalize_sex(.data[[SEX_COL]])
  ) %>%
  mutate(
    cohort_q = make_quarter_label(index_date),
    cohort_year = as.integer(format(index_date, "%Y")),
    age_group = cut(
      age_years,
      breaks = AGE_BREAKS,
      right = FALSE,
      include.lowest = TRUE
    ),
    age_sex_stratum = interaction(age_group, sex, drop = TRUE, sep = " | ")
  ) %>%
  filter(
    !is.na(person_id),
    !is.na(index_date),
    !is.na(event_1y),
    !is.na(age_years),
    !is.na(sex),
    !is.na(age_group),
    !is.na(age_sex_stratum)
  )

if (!is.null(STUDY_START_YEAR)) {
  analytic_data <- analytic_data %>%
    filter(cohort_year >= STUDY_START_YEAR)
}

if (!is.null(STUDY_END_YEAR)) {
  analytic_data <- analytic_data %>%
    filter(cohort_year <= STUDY_END_YEAR)
}

if (nrow(analytic_data) == 0) {
  stop("The analytic dataset is empty after applying filters.")
}


###############################################################################
# 6) DEFINE STANDARD POPULATION FROM THE OVERALL COHORT
###############################################################################
# Direct standardization uses the age-sex distribution of the full analytic
# cohort as the standard population.

standard_population <- analytic_data %>%
  count(age_sex_stratum, name = "standard_n") %>%
  mutate(
    standard_weight = standard_n / sum(standard_n)
  )

if (abs(sum(standard_population$standard_weight) - 1) >= 1e-8) {
  stop("Standard population weights do not sum to 1.")
}


###############################################################################
# 7) CALCULATE QUARTER- AND STRATUM-SPECIFIC RISKS
###############################################################################

quarter_stratum_counts <- analytic_data %>%
  group_by(cohort_q, age_sex_stratum) %>%
  summarise(
    n_stratum = n(),
    events_stratum = sum(event_1y, na.rm = TRUE),
    risk_stratum = events_stratum / n_stratum,
    .groups = "drop"
  )

quarter_totals <- analytic_data %>%
  group_by(cohort_q) %>%
  summarise(
    n = n(),
    events_by_365d = sum(event_1y, na.rm = TRUE),
    crude_risk_365d = events_by_365d / n,
    .groups = "drop"
  )

quarter_grid <- tidyr::expand_grid(
  cohort_q = sort(unique(analytic_data$cohort_q)),
  age_sex_stratum = standard_population$age_sex_stratum
) %>%
  left_join(
    standard_population,
    by = "age_sex_stratum"
  ) %>%
  left_join(
    quarter_stratum_counts,
    by = c("cohort_q", "age_sex_stratum")
  )


###############################################################################
# 8) DIRECTLY STANDARDIZE RISK BY QUARTER
###############################################################################
# Standardized risk:
#
#   sum_s(standard_weight_s * quarter_stratum_risk_s)
#
# where s indexes age-sex strata.
#
# If a quarter has no patients in a standard-population stratum, that stratum
# has missing risk for that quarter. The coverage column shows the proportion
# of the standard population represented by non-empty strata in that quarter.

quarterly_standardized_risk <- quarter_grid %>%
  group_by(cohort_q) %>%
  summarise(
    coverage_standard_population = sum(
      standard_weight[!is.na(risk_stratum) & n_stratum > 0],
      na.rm = TRUE
    ),
    
    standardized_risk_365d = sum(
      standard_weight * if_else(is.na(risk_stratum), 0, risk_stratum),
      na.rm = TRUE
    ),
    
    variance_standardized_risk_365d = sum(
      if_else(
        !is.na(risk_stratum) & n_stratum > 0,
        (standard_weight^2) * risk_stratum * (1 - risk_stratum) / n_stratum,
        0
      ),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    se_standardized_risk_365d = sqrt(variance_standardized_risk_365d),
    standardized_risk_365d_lcl = pmax(
      0,
      standardized_risk_365d - 1.96 * se_standardized_risk_365d
    ),
    standardized_risk_365d_ucl = pmin(
      1,
      standardized_risk_365d + 1.96 * se_standardized_risk_365d
    )
  ) %>%
  left_join(
    quarter_totals,
    by = "cohort_q"
  ) %>%
  mutate(
    outcome_name = OUTCOME_NAME
  ) %>%
  arrange(cohort_q) %>%
  mutate(
    time_index = row_number()
  ) %>%
  select(
    outcome_name,
    cohort_q,
    time_index,
    n,
    events_by_365d,
    crude_risk_365d,
    standardized_risk_365d,
    standardized_risk_365d_lcl,
    standardized_risk_365d_ucl,
    se_standardized_risk_365d,
    coverage_standard_population
  )


###############################################################################
# 9) WRITE OUTPUT
###############################################################################

write_xlsx(
  list(
    quarterly_standardized_risk = quarterly_standardized_risk,
    standard_population = standard_population,
    quarter_stratum_counts = quarter_stratum_counts
  ),
  path = OUTPUT_FILE
)

message("Done. Quarterly standardized outcome table written to: ", OUTPUT_FILE)
