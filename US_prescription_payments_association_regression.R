###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# INPUT FILE
# ---------------------------------------------------------------------------
# This should be the provider-level analytic cohort created in other
# data-preparation script.
#
# Supported file types:
# - .csv
# - .rds
ANALYTIC_COHORT_FILE <- "<PROVIDER_LEVEL_ANALYTIC_COHORT_FILE>"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
# The output workbook will contain:
# - multinomial_or_table
# - model_summary
# - outcome_distribution
# - model_data_dictionary
OUTPUT_MODEL_RESULTS_FILE <- "<MULTINOMIAL_MODEL_RESULTS_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# MODEL SETTINGS
# ---------------------------------------------------------------------------
# Outcome categories.
# The first level is the reference outcome category.
OUTCOME_LEVELS <- c("<20%", "20-80%", ">80%")

# Predictor category levels.
TARGET_DRUG_CLAIMS_LEVELS <- c("<=25", "26-44", ">=45")
TICAGRELOR_SHARE_LEVELS <- c("<20%", "20-80%", ">80%")
OVERALL_PART_D_CLAIMS_LEVELS <- c("<3000", "3000-7999", "8000-15999", ">=16000")
PAYMENT_LEVELS <- c("0", "1-250", ">250")
CARDIOLOGIST_LEVELS <- c("No", "Yes")
REGION_LEVELS <- c("Northeast", "Midwest", "South", "West")

# Confidence interval level.
CONFIDENCE_LEVEL <- 0.95


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "readr",
  "readxl",
  "nnet",
  "writexl",
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
library(tibble)
library(readr)
library(readxl)
library(nnet)
library(writexl)
library(tools)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

read_input_data <- function(file) {
  extension <- tolower(tools::file_ext(file))
  
  if (extension == "csv") {
    return(readr::read_csv(file, show_col_types = FALSE))
  }
  
  if (extension == "rds") {
    return(readRDS(file))
  }
  
  stop(
    "Unsupported input file type: .",
    extension,
    ". Please provide a .csv or .rds file."
  )
}

normalize_cardiologist <- function(x) {
  x_chr <- trimws(as.character(x))
  
  dplyr::case_when(
    x_chr %in% c("1", "Yes", "yes", "YES", "TRUE", "True", "true") ~ "Yes",
    x_chr %in% c("0", "No", "no", "NO", "FALSE", "False", "false") ~ "No",
    TRUE ~ NA_character_
  )
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

make_multinomial_or_table <- function(fit, conf.level = CONFIDENCE_LEVEL) {
  model_summary <- summary(fit)
  
  coefficient_matrix <- model_summary$coefficients
  standard_error_matrix <- model_summary$standard.errors
  
  # If the outcome has only two non-reference levels, nnet::multinom may return
  # vectors rather than matrices. This block standardizes the structure.
  if (is.null(dim(coefficient_matrix))) {
    coefficient_matrix <- matrix(
      coefficient_matrix,
      nrow = 1,
      dimnames = list(fit$lev[2], names(coefficient_matrix))
    )
    
    standard_error_matrix <- matrix(
      standard_error_matrix,
      nrow = 1,
      dimnames = list(fit$lev[2], names(standard_error_matrix))
    )
  }
  
  z_critical <- qnorm(1 - (1 - conf.level) / 2)
  reference_outcome <- fit$lev[1]
  
  factor_variables <- names(fit$xlevels)
  factor_variables_parse <- factor_variables[
    order(nchar(factor_variables), decreasing = TRUE)
  ]
  
  model_term_order <- attr(terms(fit), "term.labels")
  
  parse_model_term <- function(term) {
    if (term == "(Intercept)") {
      return(tibble(variable = "(Intercept)", level = NA_character_))
    }
    
    matching_variables <- factor_variables_parse[
      startsWith(term, factor_variables_parse)
    ]
    
    if (length(matching_variables) > 0) {
      variable_name <- matching_variables[1]
      level_name <- sub(
        paste0("^", escape_regex(variable_name)),
        "",
        term
      )
      
      return(
        tibble(
          variable = variable_name,
          level = level_name
        )
      )
    }
    
    tibble(
      variable = term,
      level = NA_character_
    )
  }
  
  or_rows <- purrr::map_dfr(
    rownames(coefficient_matrix),
    function(outcome_level) {
      tibble(
        outcome_comparison = paste0(outcome_level, " vs ", reference_outcome),
        term = colnames(coefficient_matrix),
        estimate = as.numeric(coefficient_matrix[outcome_level, ]),
        std_error = as.numeric(standard_error_matrix[outcome_level, ])
      ) %>%
        mutate(
          odds_ratio = exp(estimate),
          ci_low = exp(estimate - z_critical * std_error),
          ci_high = exp(estimate + z_critical * std_error),
          z_value = estimate / std_error,
          p_value_numeric = 2 * (1 - pnorm(abs(z_value)))
        )
    }
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(parsed = purrr::map(term, parse_model_term)) %>%
    tidyr::unnest(parsed)
  
  reference_rows <- tidyr::expand_grid(
    outcome_comparison = unique(or_rows$outcome_comparison),
    variable = factor_variables
  ) %>%
    mutate(
      level = purrr::map_chr(variable, ~ fit$xlevels[[.x]][1]),
      odds_ratio = 1,
      ci_low = NA_real_,
      ci_high = NA_real_,
      z_value = NA_real_,
      p_value_numeric = NA_real_,
      OR_CI = "Reference",
      p_value = NA_character_
    )
  
  variable_order <- tibble(
    variable = model_term_order,
    variable_order = seq_along(model_term_order)
  )
  
  level_order <- purrr::map_dfr(
    names(fit$xlevels),
    function(variable_name) {
      tibble(
        variable = variable_name,
        level = fit$xlevels[[variable_name]],
        level_order = seq_along(fit$xlevels[[variable_name]])
      )
    }
  )
  
  or_rows_formatted <- or_rows %>%
    mutate(
      OR_CI = sprintf("%.2f (%.2f, %.2f)", odds_ratio, ci_low, ci_high),
      p_value = case_when(
        is.na(p_value_numeric) ~ NA_character_,
        p_value_numeric < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p_value_numeric)
      )
    )
  
  bind_rows(reference_rows, or_rows_formatted) %>%
    left_join(variable_order, by = "variable") %>%
    left_join(level_order, by = c("variable", "level")) %>%
    mutate(
      variable_order = if_else(is.na(variable_order), 999L, variable_order),
      level_order = if_else(is.na(level_order), 1L, level_order)
    ) %>%
    arrange(
      outcome_comparison,
      variable_order,
      level_order
    ) %>%
    select(
      outcome_comparison,
      variable,
      level,
      odds_ratio,
      ci_low,
      ci_high,
      OR_CI,
      p_value
    )
}


###############################################################################
# 4) READ INPUT DATA
###############################################################################

analytic_cohort_all_providers <- read_input_data(ANALYTIC_COHORT_FILE)

required_columns <- c(
  "Prscrbr_NPI",
  "outcome_2022_2023_ticagrelor_share_cat",
  "baseline_2020_2021_ticagrelor_prasugrel_claims_cat",
  "baseline_2020_2021_ticagrelor_share_cat",
  "region",
  "cardiologist",
  "baseline_2020_2021_overall_part_d_claims_cat",
  "payments_received_usd_baseline_2020_2021",
  "payments_received_usd_baseline_2020_2021_cat"
)

missing_columns <- setdiff(required_columns, names(analytic_cohort_all_providers))

if (length(missing_columns) > 0) {
  stop(
    "The analytic cohort is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


###############################################################################
# 5) BUILD ANALYSIS DATASET
###############################################################################
# Outcome:
# - outcome_2022_2023_ticagrelor_share_cat
# - Reference outcome category: first level of OUTCOME_LEVELS, usually "<20%"
#
# Predictors:
# - baseline combined ticagrelor/prasugrel prescribing volume
# - baseline ticagrelor share
# - Census region
# - cardiologist status
# - baseline overall Part D prescribing volume
# - baseline payment category

multinomial_data <- analytic_cohort_all_providers %>%
  mutate(
    outcome_2022_2023_ticagrelor_share_cat = factor(
      outcome_2022_2023_ticagrelor_share_cat,
      levels = OUTCOME_LEVELS
    ),
    
    baseline_2020_2021_ticagrelor_prasugrel_claims_cat = factor(
      baseline_2020_2021_ticagrelor_prasugrel_claims_cat,
      levels = TARGET_DRUG_CLAIMS_LEVELS
    ),
    
    baseline_2020_2021_ticagrelor_share_cat = factor(
      baseline_2020_2021_ticagrelor_share_cat,
      levels = TICAGRELOR_SHARE_LEVELS
    ),
    
    baseline_2020_2021_overall_part_d_claims_cat = factor(
      baseline_2020_2021_overall_part_d_claims_cat,
      levels = OVERALL_PART_D_CLAIMS_LEVELS
    ),
    
    payments_received_usd_baseline_2020_2021_cat = factor(
      payments_received_usd_baseline_2020_2021_cat,
      levels = PAYMENT_LEVELS
    ),
    
    cardiologist = factor(
      normalize_cardiologist(cardiologist),
      levels = CARDIOLOGIST_LEVELS
    ),
    
    region = factor(
      region,
      levels = REGION_LEVELS
    )
  ) %>%
  select(
    Prscrbr_NPI,
    
    # Outcome
    outcome_2022_2023_ticagrelor_share_cat,
    
    # Predictors
    baseline_2020_2021_ticagrelor_prasugrel_claims_cat,
    baseline_2020_2021_ticagrelor_share_cat,
    region,
    cardiologist,
    baseline_2020_2021_overall_part_d_claims_cat,
    payments_received_usd_baseline_2020_2021_cat
  ) %>%
  filter(
    complete.cases(.)
  ) %>%
  droplevels()

if (nrow(multinomial_data) == 0) {
  stop("The multinomial analysis dataset is empty after filtering.")
}

if (nlevels(multinomial_data$outcome_2022_2023_ticagrelor_share_cat) < 2) {
  stop("The outcome has fewer than two observed categories after filtering.")
}


###############################################################################
# 6) DESCRIPTIVE CHECKS
###############################################################################

outcome_distribution <- multinomial_data %>%
  count(
    outcome_2022_2023_ticagrelor_share_cat,
    name = "n_providers"
  ) %>%
  mutate(
    percent = 100 * n_providers / sum(n_providers)
  )

print(outcome_distribution)


###############################################################################
# 7) FIT MULTINOMIAL MODEL
###############################################################################
# Model:
# Outcome-period ticagrelor share category is predicted by baseline prescribing
# behavior, baseline payments, provider region, cardiologist status, and baseline
# overall Part D claim volume.
#
# The reference outcome category is the first level of OUTCOME_LEVELS.

multinomial_model <- nnet::multinom(
  outcome_2022_2023_ticagrelor_share_cat ~
    baseline_2020_2021_ticagrelor_prasugrel_claims_cat +
    baseline_2020_2021_ticagrelor_share_cat +
    region +
    cardiologist +
    baseline_2020_2021_overall_part_d_claims_cat +
    payments_received_usd_baseline_2020_2021_cat,
  data = multinomial_data,
  Hess = TRUE,
  trace = FALSE
)


###############################################################################
# 8) CREATE ODDS RATIO TABLE
###############################################################################

multinomial_or_table <- make_multinomial_or_table(
  fit = multinomial_model,
  conf.level = CONFIDENCE_LEVEL
)

print(multinomial_or_table, n = Inf)


###############################################################################
# 9) MODEL SUMMARY AND DATA DICTIONARY
###############################################################################

model_summary <- tibble(
  parameter = c(
    "N providers in model",
    "Outcome reference category",
    "Maximum baseline payment included",
    "Confidence interval level",
    "Model family",
    "Outcome",
    "Predictors"
  ),
  value = c(
    as.character(nrow(multinomial_data)),
    OUTCOME_LEVELS[1],
    as.character(CONFIDENCE_LEVEL),
    "Multinomial logistic regression using nnet::multinom",
    "Outcome-period ticagrelor share category",
    paste(
      c(
        "baseline combined ticagrelor/prasugrel prescribing volume",
        "baseline ticagrelor share",
        "Census region",
        "cardiologist status",
        "baseline overall Part D claim volume",
        "baseline payment category"
      ),
      collapse = "; "
    )
  )
)

model_data_dictionary <- tibble(
  variable = c(
    "outcome_2022_2023_ticagrelor_share_cat",
    "baseline_2020_2021_ticagrelor_prasugrel_claims_cat",
    "baseline_2020_2021_ticagrelor_share_cat",
    "region",
    "cardiologist",
    "baseline_2020_2021_overall_part_d_claims_cat",
    "payments_received_usd_baseline_2020_2021_cat"
  ),
  role = c(
    "Outcome",
    "Predictor",
    "Predictor",
    "Predictor",
    "Predictor",
    "Predictor",
    "Predictor"
  ),
  levels = c(
    paste(OUTCOME_LEVELS, collapse = " | "),
    paste(TARGET_DRUG_CLAIMS_LEVELS, collapse = " | "),
    paste(TICAGRELOR_SHARE_LEVELS, collapse = " | "),
    paste(REGION_LEVELS, collapse = " | "),
    paste(CARDIOLOGIST_LEVELS, collapse = " | "),
    paste(OVERALL_PART_D_CLAIMS_LEVELS, collapse = " | "),
    paste(PAYMENT_LEVELS, collapse = " | ")
  )
)


###############################################################################
# 10) EXPORT RESULTS
###############################################################################

writexl::write_xlsx(
  list(
    multinomial_or_table = multinomial_or_table,
    model_summary = model_summary,
    outcome_distribution = outcome_distribution,
    model_data_dictionary = model_data_dictionary
  ),
  path = OUTPUT_MODEL_RESULTS_FILE
)

message("Done. Multinomial model results written to: ", OUTPUT_MODEL_RESULTS_FILE)