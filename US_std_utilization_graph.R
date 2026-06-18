
# -----------------------------------------------------------------------------
# Load packages
# -----------------------------------------------------------------------------
# Install missing packages before running, for example:
# install.packages(c("dplyr", "ggplot2", "lubridate", "scales",
#                    "grid", "readxl", "tibble", "stringr", "svglite"))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(grid)
  library(readxl)
  library(tibble)
  library(stringr)
})


# =============================================================================
# USER CONFIGURATION
# =============================================================================

# -----------------------------------------------------------------------------
# Input file
# -----------------------------------------------------------------------------
# Replace these placeholders with your own file name and sheet name or number.

STD_FILE  <- "<STANDARDIZED_UTILIZATION_EXCEL_FILE>"
STD_SHEET <- "<STANDARDIZED_UTILIZATION_SHEET>"

# If STRATIFY_SOURCE is TRUE, this column is used to identify data sources.
# The script lowercases column names after import, so keep this lowercase.
SOURCE_COLUMN <- "source"


# -----------------------------------------------------------------------------
# Output files
# -----------------------------------------------------------------------------
# All output files are written to OUTPUT_DIR. The folder is created automatically.

OUTPUT_DIR <- "outputs"

OUT_PLOT_PNG <- file.path(OUTPUT_DIR, "standardized_utilization_plot.png")
OUT_PLOT_SVG <- file.path(OUTPUT_DIR, "standardized_utilization_plot.svg")

EXPORT_DPI <- 300


# -----------------------------------------------------------------------------
# Analysis options
# -----------------------------------------------------------------------------

# Set TRUE to include 2025 data. Set FALSE to end the plot at 2024.
SHOW_2025 <- FALSE
END_YEAR  <- if (SHOW_2025) 2025 else 2024

# Set TRUE to show Overall plus source-specific lines.
# Set FALSE to show only Overall.
#
# Note:
#   STRATIFY_SOURCE requires a source column in STD_FILE, for example:
STRATIFY_SOURCE <- FALSE

# Year range included in the analysis.
START_YEAR <- 2011


# -----------------------------------------------------------------------------
# Treatment display settings
# -----------------------------------------------------------------------------
# These names should match, or be mappable from, the index_exposure column.

DRUG_LEVELS <- c("Clopidogrel", "Prasugrel", "Ticagrelor")

# Optional harmonization of treatment names.
# Add additional aliases if your input file uses different spellings.
standardize_drug_name <- function(x) {
  x_clean <- str_trim(as.character(x))
  x_lower <- str_to_lower(x_clean)

  case_when(
    x_lower %in% c("clopidogrel", "clopi") ~ "Clopidogrel",
    x_lower %in% c("prasugrel")            ~ "Prasugrel",
    x_lower %in% c("ticagrelor")           ~ "Ticagrelor",
    TRUE                                   ~ x_clean
  )
}

# Colors used for each treatment.
# These values are not sensitive and can be changed to match a journal or
# presentation style.
DRUG_COLORS <- c(
  "Clopidogrel" = "#F09A4A",
  "Prasugrel"   = "#355360",
  "Ticagrelor"  = "#59BAED"
)


# -----------------------------------------------------------------------------
# Source display settings
# -----------------------------------------------------------------------------
# Used only when STRATIFY_SOURCE is TRUE.

SOURCE_LEVELS <- c("Overall", "Optum", "MarketScan")

SOURCE_LINETYPES <- c(
  "Overall"    = "solid",
  "Optum"      = "22",
  "MarketScan" = "42"
)


# -----------------------------------------------------------------------------
# Axis and layout settings
# -----------------------------------------------------------------------------

YEARS_TICKS <- c(2011, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

Y_BREAKS <- seq(0, 100, by = 20)
Y_LIMITS <- c(0, 100)

# Padding on the x-axis, in days.
PAD_DAYS <- 60

# Use a blank font family for maximum portability.
# To use a specific installed font, set a font name here, for example:
# BASE_FONT_FAMILY <- "Arial"
BASE_FONT_FAMILY <- ""

# Set to "none" to hide the legend.
# Alternative examples: "right", "bottom", or c(0.98, 0.99)
LEGEND_POSITION <- "none"


# =============================================================================
# VALIDATION HELPERS
# =============================================================================

is_placeholder <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^<.*>$", x)
}

check_input_file <- function(path, setting_name) {
  if (is_placeholder(path)) {
    stop(
      setting_name, " still contains a placeholder: ", path, "\n",
      "Replace it with a real file path before running the script.",
      call. = FALSE
    )
  }

  if (!file.exists(path)) {
    stop(
      "File not found for ", setting_name, ": ", path, "\n",
      "Use a valid relative or absolute path.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

require_columns <- function(data, required_columns, data_name) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      data_name, " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# =============================================================================
# READ AND PREPARE DATA
# =============================================================================

check_input_file(STD_FILE, "STD_FILE")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

raw_data <- readxl::read_excel(STD_FILE, sheet = STD_SHEET) %>%
  rename_with(tolower)

required_columns <- c("year", "half_year", "index_exposure", "std_n")
require_columns(raw_data, required_columns, "STD_FILE")

if (STRATIFY_SOURCE && !(SOURCE_COLUMN %in% names(raw_data))) {
  stop(
    "STRATIFY_SOURCE is TRUE, but STD_FILE does not contain the source column: ",
    SOURCE_COLUMN,
    call. = FALSE
  )
}

# Harmonize treatment names, apply year range, and keep selected treatments.
dat_raw <- raw_data %>%
  mutate(
    year = as.integer(year),
    half_year = as.integer(half_year),
    index_exposure = standardize_drug_name(index_exposure),
    std_n = as.numeric(std_n)
  ) %>%
  filter(
    year >= START_YEAR,
    year <= END_YEAR,
    index_exposure %in% DRUG_LEVELS
  )


# =============================================================================
# CALCULATE STANDARDIZED PROPORTIONS
# =============================================================================

# Overall half-yearly proportions.
overall_dat <- dat_raw %>%
  group_by(year, half_year, index_exposure) %>%
  summarise(
    n = sum(std_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, half_year) %>%
  mutate(
    denom = sum(n, na.rm = TRUE),
    pct = if_else(denom > 0, n * 100 / denom, NA_real_),
    source_plot = "Overall"
  ) %>%
  ungroup()


# Optional source-specific half-yearly proportions.
# This object is only created when STRATIFY_SOURCE is TRUE.
if (STRATIFY_SOURCE) {
  strata_dat <- dat_raw %>%
    mutate(source_plot = as.character(.data[[SOURCE_COLUMN]])) %>%
    group_by(source_plot, year, half_year, index_exposure) %>%
    summarise(
      n = sum(std_n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(source_plot, year, half_year) %>%
    mutate(
      denom = sum(n, na.rm = TRUE),
      pct = if_else(denom > 0, n * 100 / denom, NA_real_)
    ) %>%
    ungroup()

  plot_dat <- bind_rows(overall_dat, strata_dat)
} else {
  plot_dat <- overall_dat
}


# Convert variables to ordered factors and create a date for each half-year.
plot_dat <- plot_dat %>%
  mutate(
    index_exposure = factor(index_exposure, levels = DRUG_LEVELS),
    source_plot = factor(source_plot, levels = SOURCE_LEVELS),
    x_date = make_date(
      year = year,
      month = if_else(half_year == 1L, 1L, 7L),
      day = 1L
    )
  )


# =============================================================================
# BUILD AXIS BREAKS
# =============================================================================

year_breaks <- tibble(
  year = YEARS_TICKS,
  x_break = make_date(YEARS_TICKS, 1, 1)
) %>%
  filter(year <= END_YEAR)

x_min <- min(plot_dat$x_date, na.rm = TRUE) - PAD_DAYS
x_max <- max(plot_dat$x_date, na.rm = TRUE) + PAD_DAYS


# =============================================================================
# CREATE PLOT
# =============================================================================

if (STRATIFY_SOURCE) {

  utilization_plot <- ggplot(
    plot_dat,
    aes(
      x = x_date,
      y = pct,
      color = index_exposure,
      linetype = source_plot,
      group = interaction(index_exposure, source_plot)
    )
  ) +
    geom_line(
      linewidth = 1,
      na.rm = TRUE
    ) +
    scale_color_manual(
      values = DRUG_COLORS,
      breaks = DRUG_LEVELS,
      drop = FALSE,
      name = NULL
    ) +
    scale_linetype_manual(
      values = SOURCE_LINETYPES,
      breaks = SOURCE_LEVELS,
      drop = FALSE,
      name = NULL
    )

} else {

  utilization_plot <- ggplot(
    plot_dat,
    aes(
      x = x_date,
      y = pct,
      color = index_exposure,
      group = index_exposure
    )
  ) +
    geom_line(
      linewidth = 1,
      na.rm = TRUE
    ) +
    scale_color_manual(
      values = DRUG_COLORS,
      breaks = DRUG_LEVELS,
      drop = FALSE,
      name = NULL
    )
}


utilization_plot <- utilization_plot +
  scale_y_continuous(
    breaks = Y_BREAKS,
    expand = expansion(mult = c(0, 0)),
    minor_breaks = NULL
  ) +
  scale_x_date(
    breaks = year_breaks$x_break,
    labels = year_breaks$year,
    expand = expansion(mult = c(0, 0), add = c(0, 0))
  ) +
  coord_cartesian(
    xlim = c(x_min, x_max),
    ylim = Y_LIMITS,
    clip = "off"
  ) +
  theme_minimal(base_size = 12, base_family = BASE_FONT_FAMILY) +
  theme(
    plot.margin = margin(10, 20, 10, 10),

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      colour = scales::alpha("grey30", 0.3)
    ),

    axis.line = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks.length = unit(4, "pt"),

    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title.x = element_text(size = 12, colour = "#000000", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, colour = "#000000"),
    axis.title.y = element_text(size = 12, colour = "#000000", margin = margin(r = 10)),

    legend.position = LEGEND_POSITION,
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.85),
      colour = scales::alpha("grey30", 0.3),
      linewidth = 0.2
    ),
    legend.margin = margin(6, 8, 6, 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width = unit(0.70, "cm")
  ) +
  labs(
    x = "Year",
    y = expression("Age- and sex-standardized proportion of initiators, %")
  )


# Print the plot in an interactive R session.
print(utilization_plot)


# =============================================================================
# EXPORT PLOT
# =============================================================================

ggsave(
  filename = OUT_PLOT_PNG,
  plot = utilization_plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = EXPORT_DPI
)

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    filename = OUT_PLOT_SVG,
    plot = utilization_plot,
    device = svglite::svglite,
    width = 7,
    height = 5,
    units = "in"
  )
} else {
  warning("Package 'svglite' is not installed. SVG export was skipped.")
}
