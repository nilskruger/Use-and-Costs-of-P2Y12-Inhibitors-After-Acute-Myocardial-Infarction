# -----------------------------------------------------------------------------
# Load packages
# -----------------------------------------------------------------------------
# Install missing packages before running, for example:
# install.packages(c("readxl", "dplyr", "stringr", "tidyr",
#                    "ggplot2", "grid", "scales", "svglite"))

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(grid)
  library(scales)
})


# =============================================================================
# USER CONFIGURATION
# =============================================================================

# -----------------------------------------------------------------------------
# Input file
# -----------------------------------------------------------------------------
# Replace these placeholders with your own file name and sheet name or number.

PAYMENTS_FILE  <- "<OPEN_PAYMENTS_EXCEL_FILE>"
PAYMENTS_SHEET <- "<OPEN_PAYMENTS_SHEET>"


# -----------------------------------------------------------------------------
# Output files
# -----------------------------------------------------------------------------
# Output files are written to OUTPUT_DIR. The folder is created automatically.

OUTPUT_DIR <- "outputs"

OUT_PLOT_PNG <- file.path(
  OUTPUT_DIR,
  "cumulative_drug_related_payments.png"
)

OUT_PLOT_SVG <- file.path(
  OUTPUT_DIR,
  "cumulative_drug_related_payments.svg"
)

EXPORT_DPI <- 300


# -----------------------------------------------------------------------------
# Analysis filters
# -----------------------------------------------------------------------------

PAYMENT_CATEGORY_KEEP <- "drug_related"

# Optional type filter.
# Set to NULL to keep all payment types.
# Example:
# TYPE_KEEP <- "GEN"
TYPE_KEEP <- NULL


# -----------------------------------------------------------------------------
# Drug display settings
# -----------------------------------------------------------------------------

DRUG_LEVELS <- c("Prasugrel", "Ticagrelor")

DRUG_COLORS <- c(
  "Prasugrel"  = "#355360",
  "Ticagrelor" = "#59BAED"
)


# -----------------------------------------------------------------------------
# Mapping from input company/product labels to plot drug names
# -----------------------------------------------------------------------------
# Edit these aliases if your input file uses different spellings.

standardize_drug_name <- function(company_group) {
  company_group_lower <- str_to_lower(str_trim(as.character(company_group)))
  
  case_when(
    company_group_lower %in% c("bri", "brilinta", "ticagrelor") ~ "Ticagrelor",
    company_group_lower %in% c("efi", "effient", "prasugrel")   ~ "Prasugrel",
    TRUE                                                        ~ NA_character_
  )
}


# -----------------------------------------------------------------------------
# Bar layout settings
# -----------------------------------------------------------------------------

BAR_WIDTH <- 0.34

X_OFFSET <- c(
  "Prasugrel"  = -0.20,
  "Ticagrelor" =  0.20
)


# -----------------------------------------------------------------------------
# Axis settings
# -----------------------------------------------------------------------------
# Payments are plotted in millions of USD.

PAYMENT_SCALE <- 1e6

# Set USE_DYNAMIC_Y_AXIS to TRUE to calculate the y-axis limit automatically.
# Set it to FALSE to use the manual values below.
USE_DYNAMIC_Y_AXIS <- FALSE

Y_LIMITS_MANUAL <- c(0, 350)
Y_BREAKS_MANUAL <- seq(0, 350, by = 50)

# Used only when USE_DYNAMIC_Y_AXIS is TRUE.
Y_ROUND_TO <- 50


# -----------------------------------------------------------------------------
# Theme settings
# -----------------------------------------------------------------------------
# Use an empty font family for portability.
# To use a specific installed font, change this, for example:
# BASE_FONT_FAMILY <- "Arial"

BASE_FONT_FAMILY <- ""

LEGEND_POSITION <- c(0.25, 0.98)


# =============================================================================
# HELPER FUNCTIONS
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
# READ INPUT DATA
# =============================================================================

check_input_file(PAYMENTS_FILE, "PAYMENTS_FILE")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

payments_raw <- readxl::read_excel(
  PAYMENTS_FILE,
  sheet = PAYMENTS_SHEET
) %>%
  rename_with(tolower)


required_payment_columns <- c(
  "year",
  "company_group",
  "category",
  "sum_usd"
)

require_columns(
  payments_raw,
  required_payment_columns,
  "PAYMENTS_FILE"
)


if (!is.null(TYPE_KEEP) && !"type" %in% names(payments_raw)) {
  stop(
    "TYPE_KEEP is set, but PAYMENTS_FILE does not contain a 'type' column.",
    call. = FALSE
  )
}


# =============================================================================
# PREPARE DATA
# =============================================================================

payments_clean <- payments_raw %>%
  mutate(
    year = as.integer(year),
    sum_usd = as.numeric(sum_usd),
    category = str_trim(as.character(category)),
    drug = standardize_drug_name(company_group)
  ) %>%
  filter(
    !is.na(drug),
    category == PAYMENT_CATEGORY_KEEP
  )


# Apply optional type filter only if TYPE_KEEP is not NULL.
if (!is.null(TYPE_KEEP)) {
  payments_clean <- payments_clean %>%
    filter(type == TYPE_KEEP)
}


# Sum yearly drug-related payments by drug.
plot_df <- payments_clean %>%
  group_by(year, drug) %>%
  summarise(
    drug_related_usd = sum(sum_usd, na.rm = TRUE),
    .groups = "drop"
  )


# Fill missing year-drug combinations with zero so cumulative sums are correct.
x_breaks <- sort(unique(plot_df$year))

plot_df <- plot_df %>%
  complete(
    year = x_breaks,
    drug = DRUG_LEVELS,
    fill = list(drug_related_usd = 0)
  ) %>%
  mutate(
    drug = factor(drug, levels = DRUG_LEVELS)
  ) %>%
  arrange(drug, year) %>%
  group_by(drug) %>%
  mutate(
    cumulative_drug_related_usd = cumsum(drug_related_usd)
  ) %>%
  ungroup()


# =============================================================================
# CREATE BAR POSITIONS
# =============================================================================
# Each year contains two bars, one per drug.

plot_df <- plot_df %>%
  mutate(
    xpos = year + unname(X_OFFSET[as.character(drug)])
  )


rect_df <- plot_df %>%
  transmute(
    xmin = xpos - BAR_WIDTH / 2,
    xmax = xpos + BAR_WIDTH / 2,
    ymin = 0,
    ymax = cumulative_drug_related_usd / PAYMENT_SCALE,
    fill_key = factor(drug, levels = DRUG_LEVELS)
  )

outline_df <- rect_df


# =============================================================================
# Y-AXIS LIMITS
# =============================================================================

if (USE_DYNAMIC_Y_AXIS) {
  y_max <- max(rect_df$ymax, na.rm = TRUE)
  y_max <- ceiling(y_max / Y_ROUND_TO) * Y_ROUND_TO
  
  Y_LIMITS <- c(0, y_max)
  Y_BREAKS <- seq(0, y_max, by = Y_ROUND_TO)
} else {
  Y_LIMITS <- Y_LIMITS_MANUAL
  Y_BREAKS <- Y_BREAKS_MANUAL
}


# =============================================================================
# CREATE PLOT
# =============================================================================

payments_plot <- ggplot() +
  geom_rect(
    data = rect_df,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill_key
    ),
    colour = NA
  ) +
  geom_rect(
    data = outline_df,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = NA,
    colour = "black",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = DRUG_COLORS[DRUG_LEVELS],
    breaks = DRUG_LEVELS,
    labels = DRUG_LEVELS,
    drop = FALSE,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_breaks,
    minor_breaks = NULL,
    expand = expansion(mult = c(0, 0), add = c(0, 0)),
    limits = c(min(x_breaks) - 0.5, max(x_breaks) + 0.5)
  ) +
  scale_y_continuous(
    limits = Y_LIMITS,
    breaks = Y_BREAKS,
    expand = expansion(mult = c(0, 0)),
    minor_breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  guides(
    fill = guide_legend(
      override.aes = list(
        colour = "black",
        linewidth = 0.3
      )
    )
  ) +
  theme_minimal(
    base_size = 12,
    base_family = BASE_FONT_FAMILY
  ) +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      colour = scales::alpha("grey30", 0.3)
    ),
    panel.grid.minor.y = element_blank(),
    
    axis.line = element_line(
      linewidth = 0.3,
      colour = "black"
    ),
    axis.ticks = element_line(
      linewidth = 0.3,
      colour = "black"
    ),
    axis.ticks.length = unit(4, "pt"),
    axis.ticks.x = element_blank(),
    
    axis.text.x = element_text(
      size = 12,
      colour = "#000000",
      margin = margin(t = 8)
    ),
    axis.title.x = element_text(
      size = 12,
      colour = "#000000",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      colour = "#000000"
    ),
    axis.title.y = element_text(
      size = 12,
      colour = "#000000",
      margin = margin(r = 10)
    ),
    
    legend.position = LEGEND_POSITION,
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.key.spacing.y = unit(0.15, "cm"),
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.85),
      colour = "black",
      linewidth = 0.3
    ),
    legend.margin = margin(6, 8, 6, 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.55, "cm"),
    legend.key.width = unit(0.55, "cm")
  ) +
  labs(
    x = "Year",
    y = "Cumulative drug-related industry payments, $ million"
  )


# Print the plot in an interactive R session.
print(payments_plot)


# =============================================================================
# EXPORT PLOT
# =============================================================================

ggsave(
  filename = OUT_PLOT_PNG,
  plot = payments_plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = EXPORT_DPI
)


if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    filename = OUT_PLOT_SVG,
    plot = payments_plot,
    device = svglite::svglite,
    width = 7,
    height = 5,
    units = "in"
  )
} else {
  warning("Package 'svglite' is not installed. SVG export was skipped.")
}