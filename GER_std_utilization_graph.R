###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# INPUT FILE
# ---------------------------------------------------------------------------
# This file is usually the output of the half-yearly standardization script.
#
# Example expected columns:
#   year | half_year | index_exposure | std_n
UTILIZATION_FILE  <- "<STANDARDIZED_UTILIZATION_EXCEL_FILE>"
UTILIZATION_SHEET <- "<STANDARDIZED_UTILIZATION_SHEET>"

# ---------------------------------------------------------------------------
# OUTPUT FILES
# ---------------------------------------------------------------------------
OUTPUT_PNG_FILE <- "<UTILIZATION_FIGURE_PNG_FILE>"
OUTPUT_SVG_FILE <- "<UTILIZATION_FIGURE_SVG_FILE>"

# ---------------------------------------------------------------------------
# STUDY PERIOD
# ---------------------------------------------------------------------------
START_YEAR <- 2011
END_YEAR   <- 2024

# Set to TRUE only if 2025 data should be included.
SHOW_2025 <- FALSE

# ---------------------------------------------------------------------------
# EXPOSURE GROUPS AND COLORS
# ---------------------------------------------------------------------------
# These are not sensitive and can be adapted to journal or project style.
DRUG_ORDER <- c("Clopidogrel", "Prasugrel", "Ticagrelor")

DRUG_COLORS <- c(
  "Clopidogrel" = "#F09A4A",
  "Prasugrel"   = "#355360",
  "Ticagrelor"  = "#59BAED"
)

# ---------------------------------------------------------------------------
# PLOT APPEARANCE
# ---------------------------------------------------------------------------
YEAR_TICKS <- c(2011, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

PLOT_WIDTH_IN  <- 7
PLOT_HEIGHT_IN <- 5
PLOT_DPI       <- 300


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "dplyr",
  "ggplot2",
  "lubridate",
  "scales",
  "grid",
  "readxl",
  "svglite"
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
library(ggplot2)
library(lubridate)
library(scales)
library(grid)
library(readxl)
library(svglite)


###############################################################################
# 3) READ INPUT DATA
###############################################################################

utilization_raw <- readxl::read_excel(
  UTILIZATION_FILE,
  sheet = UTILIZATION_SHEET
)

required_columns <- c("year", "half_year", "index_exposure", "std_n")
missing_columns <- setdiff(required_columns, names(utilization_raw))

if (length(missing_columns) > 0) {
  stop(
    "The utilization file is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(utilization_raw), collapse = ", ")
  )
}


###############################################################################
# 4) PREPARE DATA FOR PLOTTING
###############################################################################

analysis_end_year <- if (SHOW_2025) {
  2025
} else {
  END_YEAR
}

plot_data <- utilization_raw %>%
  mutate(
    year = as.integer(year),
    half_year = as.integer(half_year),
    index_exposure = as.character(index_exposure),
    std_n = as.numeric(std_n)
  ) %>%
  filter(
    year >= START_YEAR,
    year <= analysis_end_year,
    half_year %in% c(1L, 2L),
    !is.na(index_exposure),
    !is.na(std_n)
  ) %>%
  group_by(year, half_year, index_exposure) %>%
  summarise(
    standardized_n = sum(std_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, half_year) %>%
  mutate(
    denominator = sum(standardized_n, na.rm = TRUE),
    standardized_percent = if_else(
      denominator > 0,
      100 * standardized_n / denominator,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    half_year_label = paste0(year, "-H", half_year),
    
    index_exposure = factor(
      index_exposure,
      levels = DRUG_ORDER
    ),
    
    # H1 is plotted at January 1; H2 is plotted at July 1.
    x_date = lubridate::make_date(
      year,
      if_else(half_year == 1L, 1L, 7L),
      1L
    )
  ) %>%
  arrange(year, half_year, index_exposure)

if (nrow(plot_data) == 0) {
  stop("The plotting dataset is empty after applying filters.")
}


###############################################################################
# 5) CREATE X-AXIS BREAKS
###############################################################################

year_breaks <- tibble(
  year = YEAR_TICKS,
  x_break = lubridate::make_date(YEAR_TICKS, 1, 1)
) %>%
  filter(
    year >= START_YEAR,
    year <= analysis_end_year
  )

# Add some visual padding to the left and right of the plotted time axis.
pad_days <- 60

x_min <- min(plot_data$x_date, na.rm = TRUE) - pad_days
x_max <- max(plot_data$x_date, na.rm = TRUE) + pad_days


###############################################################################
# 6) CREATE UTILIZATION PLOT
###############################################################################

utilization_plot <- ggplot(
  plot_data,
  aes(
    x = x_date,
    y = standardized_percent,
    color = index_exposure,
    group = index_exposure
  )
) +
  
  # Draw non-clopidogrel lines first.
  # Clopidogrel is drawn last so that it remains visible if lines overlap.
  geom_line(
    data = plot_data %>%
      filter(index_exposure != "Clopidogrel"),
    linewidth = 1,
    na.rm = TRUE
  ) +
  
  geom_line(
    data = plot_data %>%
      filter(index_exposure == "Clopidogrel"),
    linewidth = 1,
    na.rm = TRUE
  ) +
  
  scale_color_manual(
    values = DRUG_COLORS,
    breaks = DRUG_ORDER,
    drop = FALSE,
    name = NULL
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
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
    ylim = c(0, 100),
    clip = "off"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.margin = margin(10, 20, 10, 10),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      colour = scales::alpha("grey30", 0.3)
    ),
    panel.grid.minor.y = element_blank(),
    
    axis.line = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks.length = grid::unit(4, "pt"),
    
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title.x = element_text(
      size = 12,
      colour = "#000000",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(size = 12, colour = "#000000"),
    axis.title.y = element_text(
      size = 12,
      colour = "#000000",
      margin = margin(r = 10)
    ),
    
    legend.position = c(0.98, 0.99),
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.85),
      colour = scales::alpha("grey30", 0.3),
      linewidth = 0.2
    ),
    legend.margin = margin(6, 8, 6, 8),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.height = grid::unit(0.45, "cm"),
    legend.key.width = grid::unit(0.70, "cm")
  ) +
  
  labs(
    x = "Year",
    y = "Age- and sex-standardized proportion of initiators, %"
  )

print(utilization_plot)


###############################################################################
# 7) OPTIONAL DATA CHECK
###############################################################################
# This table can be useful for checking later-period prasugrel and ticagrelor
# proportions before publication. It is not required for plotting.

later_period_check <- plot_data %>%
  filter(
    index_exposure %in% c("Ticagrelor", "Prasugrel"),
    year > 2017
  ) %>%
  arrange(index_exposure, year, half_year) %>%
  select(
    year,
    half_year,
    index_exposure,
    standardized_percent
  )

print(later_period_check)


###############################################################################
# 8) SAVE FIGURE
###############################################################################

ggsave(
  filename = OUTPUT_PNG_FILE,
  plot = utilization_plot,
  width = PLOT_WIDTH_IN,
  height = PLOT_HEIGHT_IN,
  units = "in",
  dpi = PLOT_DPI
)

ggsave(
  filename = OUTPUT_SVG_FILE,
  plot = utilization_plot,
  device = svglite::svglite,
  width = PLOT_WIDTH_IN,
  height = PLOT_HEIGHT_IN,
  units = "in"
)

message("Done. Utilization figure written to: ", OUTPUT_PNG_FILE, " and ", OUTPUT_SVG_FILE)
