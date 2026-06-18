###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# OPEN PAYMENTS INPUT FILES
# ---------------------------------------------------------------------------
# These should be baseline-period general-payment files, already downloaded
# from CMS Open Payments or otherwise prepared for this analysis.
OPEN_PAYMENTS_FILES <- c(
  "<OPEN_PAYMENTS_GENERAL_PAYMENTS_2020_CSV_FILE>",
  "<OPEN_PAYMENTS_GENERAL_PAYMENTS_2021_CSV_FILE>"
)

# ---------------------------------------------------------------------------
# MEDICARE PART D INPUT FILES
# ---------------------------------------------------------------------------
# These should be Medicare Part D Prescribers by Provider and Drug files.
PART_D_FILES <- c(
  "2020" = "<MEDICARE_PART_D_PROVIDER_DRUG_2020_CSV_FILE>",
  "2021" = "<MEDICARE_PART_D_PROVIDER_DRUG_2021_CSV_FILE>",
  "2022" = "<MEDICARE_PART_D_PROVIDER_DRUG_2022_CSV_FILE>",
  "2023" = "<MEDICARE_PART_D_PROVIDER_DRUG_2023_CSV_FILE>"
)

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
OUTPUT_ANALYTIC_COHORT_FILE <- "<PROVIDER_LEVEL_ANALYTIC_COHORT_CSV_FILE>"

# ---------------------------------------------------------------------------
# STUDY PERIOD DEFINITIONS
# ---------------------------------------------------------------------------
BASELINE_YEARS <- c("2020", "2021")
OUTCOME_YEARS  <- c("2022", "2023")

# ---------------------------------------------------------------------------
# TARGET DRUGS
# ---------------------------------------------------------------------------
# Generic drug names are normalized to lower case.
TARGET_DRUGS <- c("ticagrelor", "prasugrel")

# Optional recoding of generic drug names from source files.
GENERIC_NAME_RECODE <- c(
  "prasugrel hcl" = "prasugrel",
  "ticagrelor" = "ticagrelor"
)

# ---------------------------------------------------------------------------
# MANUFACTURERS INCLUDED IN PAYMENT EXPOSURE
# ---------------------------------------------------------------------------
# Replace or extend this list depending on the payment exposure definition.
# Keeping this as a user setting makes the analytic choice transparent.
TARGET_MANUFACTURERS <- c(
  "<MANUFACTURER_NAME_1>",
  "<MANUFACTURER_NAME_2>",
  "<MANUFACTURER_NAME_3>"
)

# ---------------------------------------------------------------------------
# CATEGORIZATION CUTPOINTS
# ---------------------------------------------------------------------------
OVERALL_CLAIMS_LEVELS <- c("<3000", "3000-7999", "8000-15999", ">=16000")
PAYMENT_LEVELS <- c("0", "1-250", ">250")
TICAGRELOR_SHARE_LEVELS <- c("<20%", "20-80%", ">80%")
TARGET_DRUG_CLAIMS_LEVELS <- c("<=25", "26-44", ">=45")


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "tibble",
  "readr",
  "purrr"
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
library(stringr)
library(tibble)
library(readr)
library(purrr)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  as.character(x[1])
}

read_csv_with_year <- function(file, year) {
  readr::read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(source_year = as.integer(year))
}

categorize_overall_part_d_claims <- function(x) {
  case_when(
    x < 3000 ~ "<3000",
    x >= 3000 & x <= 7999 ~ "3000-7999",
    x >= 8000 & x <= 15999 ~ "8000-15999",
    x >= 16000 ~ ">=16000",
    TRUE ~ NA_character_
  )
}

categorize_payment_amount <- function(x) {
  case_when(
    x == 0 ~ "0",
    x > 0 & x <= 250 ~ "1-250",
    x > 250 ~ ">250",
    TRUE ~ NA_character_
  )
}

categorize_ticagrelor_share <- function(x) {
  case_when(
    x < 0.20 ~ "<20%",
    x >= 0.20 & x <= 0.80 ~ "20-80%",
    x > 0.80 ~ ">80%",
    TRUE ~ NA_character_
  )
}

categorize_target_drug_claims <- function(x) {
  case_when(
    x <= 25 ~ "<=25",
    x >= 26 & x <= 44 ~ "26-44",
    x >= 45 ~ ">=45",
    TRUE ~ NA_character_
  )
}


###############################################################################
# 4) CENSUS REGION MAPPING
###############################################################################
# This maps US state FIPS codes to broad Census regions.
# Modify if using a different regional classification.

state_region_map <- tibble::tribble(
  ~state_fips, ~region,
  "01", "South",
  "02", "West",
  "04", "West",
  "05", "South",
  "06", "West",
  "08", "West",
  "09", "Northeast",
  "10", "South",
  "11", "South",
  "12", "South",
  "13", "South",
  "15", "West",
  "16", "West",
  "17", "Midwest",
  "18", "Midwest",
  "19", "Midwest",
  "20", "Midwest",
  "21", "South",
  "22", "South",
  "23", "Northeast",
  "24", "South",
  "25", "Northeast",
  "26", "Midwest",
  "27", "Midwest",
  "28", "South",
  "29", "Midwest",
  "30", "West",
  "31", "Midwest",
  "32", "West",
  "33", "Northeast",
  "34", "Northeast",
  "35", "West",
  "36", "Northeast",
  "37", "South",
  "38", "Midwest",
  "39", "Midwest",
  "40", "South",
  "41", "West",
  "42", "Northeast",
  "44", "Northeast",
  "45", "South",
  "46", "Midwest",
  "47", "South",
  "48", "South",
  "49", "West",
  "50", "Northeast",
  "51", "South",
  "53", "West",
  "54", "South",
  "55", "Midwest",
  "56", "West"
)


###############################################################################
# 5) READ INPUT DATA
###############################################################################

open_payments <- purrr::map_dfr(
  OPEN_PAYMENTS_FILES,
  ~ readr::read_csv(.x, show_col_types = FALSE)
)

part_d_raw <- purrr::imap_dfr(
  PART_D_FILES,
  read_csv_with_year
)


###############################################################################
# 6) PREPARE MEDICARE PART D DATA
###############################################################################
# Standardizes NPI, generic drug name, and state FIPS.
# This step also recodes source-specific generic names to harmonized drug names.

part_d_prepared <- part_d_raw %>%
  mutate(
    Prscrbr_NPI = as.character(Prscrbr_NPI),
    
    Gnrc_Name = stringr::str_to_lower(Gnrc_Name),
    Gnrc_Name = stringr::str_trim(Gnrc_Name),
    Gnrc_Name = dplyr::recode(
      Gnrc_Name,
      !!!GENERIC_NAME_RECODE,
      .default = Gnrc_Name
    ),
    
    state_fips = sprintf(
      "%02d",
      as.integer(Prscrbr_State_FIPS)
    )
  )

baseline_part_d <- part_d_prepared %>%
  filter(as.character(source_year) %in% BASELINE_YEARS)

outcome_part_d <- part_d_prepared %>%
  filter(as.character(source_year) %in% OUTCOME_YEARS)


###############################################################################
# 7) PROVIDER-LEVEL PART D SUMMARIES
###############################################################################
# For each period, calculate:
# - all Part D claims
# - ticagrelor claims
# - prasugrel claims
# - combined ticagrelor/prasugrel claims
# - ticagrelor share among ticagrelor/prasugrel claims

summarise_period <- function(data, prefix) {
  overall_claims <- data %>%
    group_by(Prscrbr_NPI) %>%
    summarise(
      overall_part_d_claims = sum(Tot_Clms, na.rm = TRUE),
      .groups = "drop"
    )
  
  target_drug_claims <- data %>%
    filter(Gnrc_Name %in% TARGET_DRUGS) %>%
    group_by(Prscrbr_NPI) %>%
    summarise(
      ticagrelor_claims = sum(
        if_else(Gnrc_Name == "ticagrelor", Tot_Clms, 0),
        na.rm = TRUE
      ),
      prasugrel_claims = sum(
        if_else(Gnrc_Name == "prasugrel", Tot_Clms, 0),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      ticagrelor_prasugrel_claims = ticagrelor_claims + prasugrel_claims,
      ticagrelor_share = if_else(
        ticagrelor_prasugrel_claims > 0,
        ticagrelor_claims / ticagrelor_prasugrel_claims,
        NA_real_
      )
    )
  
  full_join(
    overall_claims,
    target_drug_claims,
    by = "Prscrbr_NPI"
  ) %>%
    mutate(
      overall_part_d_claims = replace_na(overall_part_d_claims, 0),
      ticagrelor_claims = replace_na(ticagrelor_claims, 0),
      prasugrel_claims = replace_na(prasugrel_claims, 0),
      ticagrelor_prasugrel_claims =
        replace_na(ticagrelor_prasugrel_claims, 0)
    ) %>%
    rename_with(
      ~ paste0(prefix, "_", .x),
      .cols = -Prscrbr_NPI
    )
}

baseline_summary <- summarise_period(
  data = baseline_part_d,
  prefix = "baseline_2020_2021"
)

outcome_summary <- summarise_period(
  data = outcome_part_d,
  prefix = "outcome_2022_2023"
)


###############################################################################
# 8) DEFINE PROVIDER COHORT
###############################################################################
# Include providers with at least one ticagrelor or prasugrel claim during the
# combined baseline/outcome period.

cohort_npis <- part_d_prepared %>%
  filter(Gnrc_Name %in% TARGET_DRUGS) %>%
  distinct(Prscrbr_NPI)


###############################################################################
# 9) PROVIDER METADATA
###############################################################################
# cardiologist is defined from the prescriber type string.
# region is derived from state FIPS.

prescriber_metadata <- part_d_prepared %>%
  group_by(Prscrbr_NPI) %>%
  summarise(
    prescriber_type = first_nonmissing(Prscrbr_Type),
    state_fips = first_nonmissing(state_fips),
    .groups = "drop"
  ) %>%
  mutate(
    cardiologist = if_else(
      stringr::str_detect(
        prescriber_type,
        regex("cardio", ignore_case = TRUE)
      ),
      1L,
      0L
    )
  ) %>%
  left_join(
    state_region_map,
    by = "state_fips"
  ) %>%
  select(
    Prscrbr_NPI,
    cardiologist,
    region
  )


###############################################################################
# 10) BASELINE OPEN PAYMENTS
###############################################################################
# Sum baseline-period payments from selected manufacturers by recipient NPI.
# Missing payment amount is later interpreted as zero dollars.

baseline_payments <- open_payments %>%
  mutate(
    Covered_Recipient_NPI = as.character(Covered_Recipient_NPI)
  ) %>%
  filter(
    Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name %in%
      TARGET_MANUFACTURERS
  ) %>%
  group_by(Covered_Recipient_NPI) %>%
  summarise(
    payments_received_usd_baseline_2020_2021 =
      sum(Total_Amount_of_Payment_USDollars, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Prscrbr_NPI = Covered_Recipient_NPI
  )


###############################################################################
# 11) BUILD ANALYTIC PROVIDER-LEVEL COHORT
###############################################################################
# The final cohort requires nonzero ticagrelor/prasugrel claims in both:
# - baseline period: 2020/2021
# - outcome period: 2022/2023

analytic_cohort_all_providers <- cohort_npis %>%
  left_join(
    baseline_summary,
    by = "Prscrbr_NPI"
  ) %>%
  left_join(
    outcome_summary,
    by = "Prscrbr_NPI"
  ) %>%
  left_join(
    baseline_payments,
    by = "Prscrbr_NPI"
  ) %>%
  left_join(
    prescriber_metadata,
    by = "Prscrbr_NPI"
  ) %>%
  mutate(
    payments_received_usd_baseline_2020_2021 =
      replace_na(payments_received_usd_baseline_2020_2021, 0),
    
    baseline_2020_2021_overall_part_d_claims =
      replace_na(baseline_2020_2021_overall_part_d_claims, 0),
    baseline_2020_2021_ticagrelor_claims =
      replace_na(baseline_2020_2021_ticagrelor_claims, 0),
    baseline_2020_2021_prasugrel_claims =
      replace_na(baseline_2020_2021_prasugrel_claims, 0),
    baseline_2020_2021_ticagrelor_prasugrel_claims =
      replace_na(baseline_2020_2021_ticagrelor_prasugrel_claims, 0),
    
    outcome_2022_2023_overall_part_d_claims =
      replace_na(outcome_2022_2023_overall_part_d_claims, 0),
    outcome_2022_2023_ticagrelor_claims =
      replace_na(outcome_2022_2023_ticagrelor_claims, 0),
    outcome_2022_2023_prasugrel_claims =
      replace_na(outcome_2022_2023_prasugrel_claims, 0),
    outcome_2022_2023_ticagrelor_prasugrel_claims =
      replace_na(outcome_2022_2023_ticagrelor_prasugrel_claims, 0),
    
    cardiologist = replace_na(cardiologist, 0L)
  ) %>%
  mutate(
    baseline_2020_2021_overall_part_d_claims_cat =
      factor(
        categorize_overall_part_d_claims(
          baseline_2020_2021_overall_part_d_claims
        ),
        levels = OVERALL_CLAIMS_LEVELS
      ),
    
    outcome_2022_2023_overall_part_d_claims_cat =
      factor(
        categorize_overall_part_d_claims(
          outcome_2022_2023_overall_part_d_claims
        ),
        levels = OVERALL_CLAIMS_LEVELS
      ),
    
    payments_received_usd_baseline_2020_2021_cat =
      factor(
        categorize_payment_amount(
          payments_received_usd_baseline_2020_2021
        ),
        levels = PAYMENT_LEVELS
      ),
    
    baseline_2020_2021_ticagrelor_share_cat =
      factor(
        categorize_ticagrelor_share(
          baseline_2020_2021_ticagrelor_share
        ),
        levels = TICAGRELOR_SHARE_LEVELS
      ),
    
    outcome_2022_2023_ticagrelor_share_cat =
      factor(
        categorize_ticagrelor_share(
          outcome_2022_2023_ticagrelor_share
        ),
        levels = TICAGRELOR_SHARE_LEVELS
      ),
    
    baseline_2020_2021_ticagrelor_prasugrel_claims_cat =
      factor(
        categorize_target_drug_claims(
          baseline_2020_2021_ticagrelor_prasugrel_claims
        ),
        levels = TARGET_DRUG_CLAIMS_LEVELS
      ),
    
    outcome_2022_2023_ticagrelor_prasugrel_claims_cat =
      factor(
        categorize_target_drug_claims(
          outcome_2022_2023_ticagrelor_prasugrel_claims
        ),
        levels = TARGET_DRUG_CLAIMS_LEVELS
      ),
    
    baseline_2020_2021_ticagrelor_claims_cat =
      factor(
        categorize_target_drug_claims(
          baseline_2020_2021_ticagrelor_claims
        ),
        levels = TARGET_DRUG_CLAIMS_LEVELS
      ),
    
    outcome_2022_2023_ticagrelor_claims_cat =
      factor(
        categorize_target_drug_claims(
          outcome_2022_2023_ticagrelor_claims
        ),
        levels = TARGET_DRUG_CLAIMS_LEVELS
      ),
    
    baseline_2020_2021_prasugrel_claims_cat =
      factor(
        categorize_target_drug_claims(
          baseline_2020_2021_prasugrel_claims
        ),
        levels = TARGET_DRUG_CLAIMS_LEVELS
      ),
    
    outcome_2022_2023_prasugrel_claims_cat =
      factor(
        categorize_target_drug_claims(
          outcome_2022_2023_prasugrel_claims
        ),
        levels = TARGET_DRUG_CLAIMS_LEVELS
      )
  ) %>%
  select(
    Prscrbr_NPI,
    
    # Baseline 2020/2021
    baseline_2020_2021_ticagrelor_claims,
    baseline_2020_2021_ticagrelor_claims_cat,
    baseline_2020_2021_prasugrel_claims,
    baseline_2020_2021_prasugrel_claims_cat,
    baseline_2020_2021_ticagrelor_prasugrel_claims,
    baseline_2020_2021_ticagrelor_prasugrel_claims_cat,
    baseline_2020_2021_overall_part_d_claims,
    baseline_2020_2021_overall_part_d_claims_cat,
    payments_received_usd_baseline_2020_2021,
    payments_received_usd_baseline_2020_2021_cat,
    baseline_2020_2021_ticagrelor_share,
    baseline_2020_2021_ticagrelor_share_cat,
    
    # Outcome 2022/2023
    outcome_2022_2023_ticagrelor_claims,
    outcome_2022_2023_ticagrelor_claims_cat,
    outcome_2022_2023_prasugrel_claims,
    outcome_2022_2023_prasugrel_claims_cat,
    outcome_2022_2023_ticagrelor_prasugrel_claims,
    outcome_2022_2023_ticagrelor_prasugrel_claims_cat,
    outcome_2022_2023_overall_part_d_claims,
    outcome_2022_2023_overall_part_d_claims_cat,
    outcome_2022_2023_ticagrelor_share,
    outcome_2022_2023_ticagrelor_share_cat,
    
    # Provider characteristics
    cardiologist,
    region
  ) %>%
  filter(
    outcome_2022_2023_ticagrelor_prasugrel_claims != 0,
    baseline_2020_2021_ticagrelor_prasugrel_claims != 0
  )



###############################################################################
# 12) EXPORT ANALYTIC COHORT
###############################################################################

readr::write_csv(
  analytic_cohort_all_providers,
  OUTPUT_ANALYTIC_COHORT_FILE
)

message("Analytic cohort written to: ", OUTPUT_ANALYTIC_COHORT_FILE)