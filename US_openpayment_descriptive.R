###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# INPUT FOLDER
# ---------------------------------------------------------------------------
# Folder containing one subfolder per year and files named GEN_filtered.csv and
# RES_filtered.csv.
OPEN_PAYMENTS_BASE_DIR <- "<OPEN_PAYMENTS_FILTERED_FILES_BASE_DIRECTORY>"

# ---------------------------------------------------------------------------
# OUTPUT FILE
# ---------------------------------------------------------------------------
OUTPUT_FILE <- "<OPEN_PAYMENTS_YEARLY_SUMMARY_EXCEL_FILE>"

# ---------------------------------------------------------------------------
# STUDY YEARS AND PAYMENT TYPES
# ---------------------------------------------------------------------------
YEARS <- 2013:2024

PAYMENT_TYPES <- c("GEN", "RES")

# ---------------------------------------------------------------------------
# THERAPEUTIC AREA
# ---------------------------------------------------------------------------
# Used only when the file contains Product_Category_or_Therapeutic_Area_1.
THERAPEUTIC_AREA <- "cardiovascular"

# ---------------------------------------------------------------------------
# PRODUCT GROUP DEFINITIONS
# ---------------------------------------------------------------------------
# Define the product names as they may appear in the Open Payments files.

PRODUCT_GROUPS <- list(
  ticagrelor_product = c("BRILINTA", "Brilinta", "brilinta"),
  prasugrel_product  = c("EFFIENT", "Effient", "effient")
)


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "readr",
  "dplyr",
  "purrr",
  "tidyr",
  "tibble",
  "writexl"
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

library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(writexl)


###############################################################################
# 3) HELPER FUNCTIONS
###############################################################################

get_payment_file <- function(year, payment_type) {
  file.path(
    OPEN_PAYMENTS_BASE_DIR,
    as.character(year),
    paste0(payment_type, "_filtered.csv")
  )
}

get_product_name_column <- function(data) {
  if ("Name_of_Drug_or_Biological_or_Device_or_Medical_Supply_1" %in% names(data)) {
    return("Name_of_Drug_or_Biological_or_Device_or_Medical_Supply_1")
  }
  
  if ("Name_of_Associated_Covered_Drug_or_Biological1" %in% names(data)) {
    return("Name_of_Associated_Covered_Drug_or_Biological1")
  }
  
  NA_character_
}

has_therapeutic_area_column <- function(data) {
  "Product_Category_or_Therapeutic_Area_1" %in% names(data)
}

read_open_payments_file <- function(file_path) {
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path)
    
    return(NULL)
  }
  
  readr::read_csv(
    file_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      Total_Amount_of_Payment_USDollars =
        readr::parse_number(
          as.character(Total_Amount_of_Payment_USDollars)
        )
    )
}


###############################################################################
# 4) IDENTIFY MANUFACTURERS ASSOCIATED WITH EACH PRODUCT GROUP
###############################################################################
# For each product group, the script first identifies manufacturers/GPOs that
# report the selected product names in the general-payment files.
#
# These manufacturers are then used to calculate broader payment totals in
# both general and research payment files.

get_relevant_companies <- function(year, product_names) {
  file_path <- get_payment_file(
    year = year,
    payment_type = "GEN"
  )
  
  data <- read_open_payments_file(file_path)
  
  if (is.null(data)) {
    return(character(0))
  }
  
  product_col <- get_product_name_column(data)
  
  if (is.na(product_col)) {
    warning("No product-name column found in: ", file_path)
    return(character(0))
  }
  
  data %>%
    filter(.data[[product_col]] %in% product_names) %>%
    distinct(Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name) %>%
    pull(Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name)
}

company_lists_by_group <- purrr::imap(
  PRODUCT_GROUPS,
  function(product_names, product_group) {
    companies_by_year <- purrr::map(
      YEARS,
      ~ get_relevant_companies(
        year = .x,
        product_names = product_names
      )
    )
    
    sort(unique(unlist(companies_by_year, use.names = FALSE)))
  }
)


###############################################################################
# 5) CALCULATE YEARLY PAYMENT SUMMARIES
###############################################################################
# Categories:
# - overall:
#     All payments from manufacturers associated with the product group.
#
# - therapeutic_area:
#     Payments from those manufacturers where the therapeutic-area column
#     contains THERAPEUTIC_AREA. Only available when the file has that column.
#
# - product_related:
#     Payments explicitly linked to the selected product names. If a therapeutic
#     area column is available, product-related payments are restricted to the
#     selected therapeutic area, matching the original script logic.

calculate_sums_one_file <- function(
    year,
    payment_type,
    companies,
    product_names,
    product_group
) {
  file_path <- get_payment_file(
    year = year,
    payment_type = payment_type
  )
  
  data <- read_open_payments_file(file_path)
  
  if (is.null(data)) {
    return(
      tibble(
        year = year,
        payment_type = payment_type,
        product_group = product_group,
        category = c("overall", "product_related"),
        sum_usd = NA_real_
      )
    )
  }
  
  product_col <- get_product_name_column(data)
  has_therapeutic_area <- has_therapeutic_area_column(data)
  
  if (is.na(product_col)) {
    warning("No product-name column found in: ", file_path)
    
    categories <- if (has_therapeutic_area) {
      c("overall", "therapeutic_area", "product_related")
    } else {
      c("overall", "product_related")
    }
    
    return(
      tibble(
        year = year,
        payment_type = payment_type,
        product_group = product_group,
        category = categories,
        sum_usd = NA_real_
      )
    )
  }
  
  data_base <- data %>%
    filter(
      Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name %in%
        companies
    )
  
  overall_sum <- data_base %>%
    summarise(
      sum_usd = sum(Total_Amount_of_Payment_USDollars, na.rm = TRUE)
    ) %>%
    pull(sum_usd)
  
  product_related_sum <- data_base %>%
    filter(.data[[product_col]] %in% product_names) %>%
    summarise(
      sum_usd = sum(Total_Amount_of_Payment_USDollars, na.rm = TRUE)
    ) %>%
    pull(sum_usd)
  
  if (has_therapeutic_area) {
    therapeutic_area_data <- data_base %>%
      filter(
        grepl(
          THERAPEUTIC_AREA,
          Product_Category_or_Therapeutic_Area_1,
          ignore.case = TRUE
        )
      )
    
    therapeutic_area_sum <- therapeutic_area_data %>%
      summarise(
        sum_usd = sum(Total_Amount_of_Payment_USDollars, na.rm = TRUE)
      ) %>%
      pull(sum_usd)
    
    product_related_sum <- therapeutic_area_data %>%
      filter(.data[[product_col]] %in% product_names) %>%
      summarise(
        sum_usd = sum(Total_Amount_of_Payment_USDollars, na.rm = TRUE)
      ) %>%
      pull(sum_usd)
    
    return(
      tibble(
        year = year,
        payment_type = payment_type,
        product_group = product_group,
        category = c("overall", "therapeutic_area", "product_related"),
        sum_usd = c(
          overall_sum,
          therapeutic_area_sum,
          product_related_sum
        )
      )
    )
  }
  
  tibble(
    year = year,
    payment_type = payment_type,
    product_group = product_group,
    category = c("overall", "product_related"),
    sum_usd = c(
      overall_sum,
      product_related_sum
    )
  )
}


###############################################################################
# 6) RUN SUMMARY ACROSS PRODUCT GROUPS, YEARS, AND PAYMENT TYPES
###############################################################################

yearly_payment_summary <- purrr::imap_dfr(
  PRODUCT_GROUPS,
  function(product_names, product_group) {
    companies <- company_lists_by_group[[product_group]]
    
    tidyr::expand_grid(
      year = YEARS,
      payment_type = PAYMENT_TYPES
    ) %>%
      purrr::pmap_dfr(
        function(year, payment_type) {
          calculate_sums_one_file(
            year = year,
            payment_type = payment_type,
            companies = companies,
            product_names = product_names,
            product_group = product_group
          )
        }
      )
  }
)



###############################################################################
# 7) EXPORT RESULTS
###############################################################################

writexl::write_xlsx(
  list(
    yearly_payment_summary = yearly_payment_summary,
    company_list_by_group = company_list_table,
    summary_check = summary_check
  ),
  path = OUTPUT_FILE
)

message("Done. Yearly Open Payments summary written to: ", OUTPUT_FILE)