# -----------------------------------------------------------------------------
# Load packages
# -----------------------------------------------------------------------------
# Install missing packages before running, for example:
# install.packages(c("dplyr", "stringr", "readxl", "ggplot2",
#                    "mgcv", "grid", "scales", "svglite"))

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(ggplot2)
  library(mgcv)
  library(grid)
  library(scales)
})


# =============================================================================
# USER CONFIGURATION
# =============================================================================

# -----------------------------------------------------------------------------
# Input files
# -----------------------------------------------------------------------------
# Replace these placeholders with your own file names or relative paths.

NET_PRICE_FILE  <- "<NET_PRICE_EXCEL_FILE>"
NET_PRICE_SHEET <- "<NET_PRICE_SHEET>"

CORRECTION_FACTOR_FILE  <- "<CORRECTION_FACTOR_EXCEL_FILE>"
CORRECTION_FACTOR_SHEET <- "<CORRECTION_FACTOR_SHEET>"


# -----------------------------------------------------------------------------
# Output files
# -----------------------------------------------------------------------------
# Output files are written to OUTPUT_DIR. The folder is created automatically.

OUTPUT_DIR <- "outputs"

OUT_PLOT_PNG <- file.path(
  OUTPUT_DIR,
  "net_annual_treatment_cost_quarterly_smooth.png"
)

OUT_PLOT_SVG <- file.path(
  OUTPUT_DIR,
  "net_annual_treatment_cost_quarterly_smooth.svg"
)

EXPORT_DPI <- 300


# -----------------------------------------------------------------------------
# Drug display switches
# -----------------------------------------------------------------------------
# Set TRUE to include a drug in the plot and FALSE to hide it.

SHOW_CLOPIDOGREL <- TRUE
SHOW_TICAGRELOR  <- TRUE
SHOW_PRASUGREL   <- TRUE


# -----------------------------------------------------------------------------
# Smoother settings
# -----------------------------------------------------------------------------
# GAM smoother complexity:
#   smaller k = smoother curve
#   larger k  = more flexible curve

GAM_K <- 6


# -----------------------------------------------------------------------------
# Legend order
# -----------------------------------------------------------------------------
# These labels are created from generic_brand + drug.
# They should match the standardized values produced below.

LEGEND_LEVELS <- c(
  "Brand-name clopidogrel",
  "Generic clopidogrel",
  "Brand-name prasugrel",
  "Generic prasugrel",
  "Brand-name ticagrelor"
)


# -----------------------------------------------------------------------------
# Plot colors and line types
# -----------------------------------------------------------------------------

DRUG_COLORS <- c(
  "clopidogrel" = "#F09A4A",
  "ticagrelor"  = "#59BAED",
  "prasugrel"   = "#355360"
)

# Generic products are shown with dashed lines; brand-name products are solid.
LINE_TYPES <- c(
  "Brand-name" = "solid",
  "Generic"    = "31"
)


# -----------------------------------------------------------------------------
# Axis settings
# -----------------------------------------------------------------------------

Y_LIMITS <- c(0, 4000)
Y_BREAKS <- seq(0, 4000, by = 1000)

YEAR_TICKS <- c(2011, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

X_AXIS_PADDING <- 0.38


# -----------------------------------------------------------------------------
# Theme settings
# -----------------------------------------------------------------------------

BASE_FONT_FAMILY <- ""

# Set to "none" to hide the legend.
# Other options: "right", "bottom", or c(0.98, 0.99)
LEGEND_POSITION <- "none"


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


standardize_drug_name <- function(x) {
  x_clean <- str_trim(as.character(x))
  x_lower <- str_to_lower(x_clean)
  
  case_when(
    x_lower %in% c("clopidogrel", "clopi") ~ "clopidogrel",
    x_lower %in% c("prasugrel")            ~ "prasugrel",
    x_lower %in% c("ticagrelor")           ~ "ticagrelor",
    TRUE                                   ~ x_lower
  )
}


standardize_brand_generic <- function(x) {
  x_clean <- str_trim(as.character(x))
  x_lower <- str_to_lower(x_clean)
  
  case_when(
    x_lower %in% c("brand", "brand-name", "brand name", "branded") ~ "Brand-name",
    x_lower %in% c("generic")                                      ~ "Generic",
    TRUE                                                           ~ x_clean
  )
}


# =============================================================================
# READ INPUT DATA
# =============================================================================

check_input_file(NET_PRICE_FILE, "NET_PRICE_FILE")
check_input_file(CORRECTION_FACTOR_FILE, "CORRECTION_FACTOR_FILE")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


net_price_raw <- readxl::read_excel(
  NET_PRICE_FILE,
  sheet = NET_PRICE_SHEET
) %>%
  rename_with(tolower)


correction_factors <- readxl::read_excel(
  CORRECTION_FACTOR_FILE,
  sheet = CORRECTION_FACTOR_SHEET
) %>%
  rename_with(tolower)


required_net_price_columns <- c(
  "year",
  "quarter",
  "generic_brand",
  "drug",
  "net_per_unit",
  "daily_doses"
)

required_correction_columns <- c(
  "year",
  "quarter",
  "correction_factor"
)

require_columns(
  net_price_raw,
  required_net_price_columns,
  "NET_PRICE_FILE"
)

require_columns(
  correction_factors,
  required_correction_columns,
  "CORRECTION_FACTOR_FILE"
)


# =============================================================================
# PREPARE DATA
# =============================================================================

# Standardize variable formats before joining.
net_price_clean <- net_price_raw %>%
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter),
    generic_brand = standardize_brand_generic(generic_brand),
    drug = standardize_drug_name(drug),
    net_per_unit = as.numeric(net_per_unit),
    daily_doses = as.numeric(daily_doses)
  )


correction_factors_clean <- correction_factors %>%
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter),
    correction_factor = as.numeric(correction_factor)
  )


# Join correction factors and calculate annual treatment cost.
# Formula:
#   net annual treatment cost =
#     net price per unit * correction factor * 365 * daily doses
net_price_data <- net_price_clean %>%
  inner_join(
    correction_factors_clean,
    by = c("quarter", "year")
  ) %>%
  mutate(
    net_annual_treatment_cost =
      net_per_unit * correction_factor * 365 * daily_doses
  )


# Create quarter labels and numeric x-axis positions.
df_base <- net_price_data %>%
  mutate(
    x = paste0(year, "-Q", quarter),
    combo = paste(generic_brand, drug)
  ) %>%
  arrange(year, quarter) %>%
  mutate(
    x = factor(x, levels = unique(x)),
    x_id = as.integer(x)
  )


# =============================================================================
# FILTER DRUGS TO DISPLAY
# =============================================================================

show_drugs <- c(
  "clopidogrel" = SHOW_CLOPIDOGREL,
  "ticagrelor"  = SHOW_TICAGRELOR,
  "prasugrel"   = SHOW_PRASUGREL
)

keep_drugs <- names(show_drugs)[show_drugs]

if (length(keep_drugs) == 0) {
  stop("At least one drug must be selected for display.", call. = FALSE)
}


df_plot <- df_base %>%
  filter(drug %in% keep_drugs) %>%
  mutate(combo = factor(combo, levels = LEGEND_LEVELS)) %>%
  filter(!is.na(combo))


active_levels <- LEGEND_LEVELS[
  LEGEND_LEVELS %in% unique(as.character(df_plot$combo))
]

df_plot <- df_plot %>%
  mutate(combo = factor(combo, levels = active_levels))


# =============================================================================
# COLOR AND LINE-TYPE MAPS
# =============================================================================

# Extract the drug name from each legend label.
drug_part <- sub(".*\\s", "", LEGEND_LEVELS)

color_map <- setNames(
  unname(DRUG_COLORS[drug_part]),
  LEGEND_LEVELS
)

# Extract brand/generic status from each legend label.
brand_generic_part <- sub("\\s.*$", "", LEGEND_LEVELS)

linetype_map <- setNames(
  ifelse(
    brand_generic_part == "Generic",
    LINE_TYPES["Generic"],
    LINE_TYPES["Brand-name"]
  ),
  LEGEND_LEVELS
)


# =============================================================================
# X-AXIS BREAKS
# =============================================================================

year_breaks <- df_base %>%
  distinct(year, quarter, x_id) %>%
  filter(
    quarter == 1,
    year %in% YEAR_TICKS
  ) %>%
  arrange(year) %>%
  transmute(
    year,
    x_break = x_id
  )

n_quarters <- nlevels(df_base$x)


# =============================================================================
# SMOOTHING LAYERS
# =============================================================================
# Clopidogrel is drawn after the other drugs so that it remains visible
# if curves overlap.

smooth_non_clopidogrel <- geom_smooth(
  data = df_plot %>%
    filter(!str_detect(as.character(combo), "clopidogrel")),
  aes(
    x = x_id,
    y = net_annual_treatment_cost,
    color = combo,
    linetype = combo,
    group = combo
  ),
  method = "gam",
  formula = y ~ s(x, bs = "cs", k = GAM_K),
  se = FALSE,
  linewidth = 1,
  na.rm = TRUE
)

smooth_clopidogrel <- geom_smooth(
  data = df_plot %>%
    filter(str_detect(as.character(combo), "clopidogrel")),
  aes(
    x = x_id,
    y = net_annual_treatment_cost,
    color = combo,
    linetype = combo,
    group = combo
  ),
  method = "gam",
  formula = y ~ s(x, bs = "cs", k = GAM_K),
  se = FALSE,
  linewidth = 1,
  na.rm = TRUE
)


# =============================================================================
# CREATE PLOT
# =============================================================================

cost_plot <- ggplot() +
  smooth_non_clopidogrel +
  smooth_clopidogrel +
  scale_y_continuous(
    breaks = Y_BREAKS,
    expand = expansion(mult = c(0, 0), add = c(0, 0))
  ) +
  scale_color_manual(
    values = color_map[active_levels],
    breaks = active_levels,
    name = NULL
  ) +
  scale_linetype_manual(
    values = linetype_map[active_levels],
    breaks = active_levels,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = year_breaks$x_break,
    labels = year_breaks$year,
    expand = expansion(mult = c(0, 0), add = c(0, 0))
  ) +
  coord_cartesian(
    xlim = c(1 - X_AXIS_PADDING, n_quarters + X_AXIS_PADDING),
    ylim = Y_LIMITS,
    clip = "off"
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
    
    axis.text.x = element_text(
      size = 12,
      colour = "#000000"
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
    
    legend.position = LEGEND_POSITION
  ) +
  labs(
    x = "Year",
    y = "Net annual treatment cost, $"
  )


# Print the plot in an interactive R session.
print(cost_plot)


# =============================================================================
# EXPORT PLOT
# =============================================================================

ggsave(
  filename = OUT_PLOT_PNG,
  plot = cost_plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = EXPORT_DPI
)


if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    filename = OUT_PLOT_SVG,
    plot = cost_plot,
    device = svglite::svglite,
    width = 7,
    height = 5,
    units = "in"
  )
} else {
  warning("Package 'svglite' is not installed. SVG export was skipped.")
}