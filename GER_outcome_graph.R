###############################################################################
# 1) USER SETTINGS - EDIT HERE
###############################################################################

# ---------------------------------------------------------------------------
# INPUT FILE
# ---------------------------------------------------------------------------
# Expected columns in INPUT_SHEET:
# - cohort_q
# - n
# - standardized_risk_365d
#
# Optional columns:
# - standardized_risk_365d_lcl
# - standardized_risk_365d_ucl
# - se_standardized_risk_365d
# - crude_risk_365d
# - events_by_365d
INPUT_FILE  <- "<QUARTERLY_STANDARDIZED_OUTCOME_EXCEL_FILE>"
INPUT_SHEET <- "quarterly_standardized_risk"

# ---------------------------------------------------------------------------
# OUTPUT FILES
# ---------------------------------------------------------------------------
OUTPUT_PNG_FILE <- "<SEGMENTED_TREND_PLOT_PNG_FILE>"
OUTPUT_SVG_FILE <- "<SEGMENTED_TREND_PLOT_SVG_FILE>"

# ---------------------------------------------------------------------------
# OUTCOME AND PLOT LABELS
# ---------------------------------------------------------------------------
OUTCOME_LABEL <- "<OUTCOME_LABEL>"  # Example: "Bleeding", "MACE"

Y_AXIS_LABEL <- "Age- and sex-standardized 1-year cumulative incidence, %"

# ---------------------------------------------------------------------------
# COLUMN NAMES
# ---------------------------------------------------------------------------
QUARTER_COL <- "cohort_q"
N_COL       <- "n"
Y_COL       <- "standardized_risk_365d"
SE_COL      <- "se_standardized_risk_365d"
LCL_COL     <- "standardized_risk_365d_lcl"
UCL_COL     <- "standardized_risk_365d_ucl"

# ---------------------------------------------------------------------------
# TIME WINDOWS FOR SEGMENTED TRENDS
# ---------------------------------------------------------------------------
# The transition period between PRE_PERIOD_END and POST_PERIOD_START is excluded.
TIME_ORIGIN_YEAR <- 2011

PRE_PERIOD_END   <- "2019-Q3"
POST_PERIOD_START <- "2020-Q4"

PLOT_START <- "2011-Q1"
PLOT_END   <- "2023-Q4"

# ---------------------------------------------------------------------------
# PLOT APPEARANCE
# ---------------------------------------------------------------------------
PLOT_COLOR <- "#355360"
  
Y_AXIS_MIN <- 0
Y_AXIS_MAX <- 0.15
Y_AXIS_BREAKS <- seq(0, 0.15, by = 0.05)

PLOT_WIDTH_IN  <- 7
PLOT_HEIGHT_IN <- 5
PLOT_DPI       <- 300


###############################################################################
# 2) LOAD PACKAGES
###############################################################################

required_packages <- c(
  "dplyr",
  "stringr",
  "readxl",
  "lubridate",
  "ggplot2",
  "scales",
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
library(stringr)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(svglite)


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
    x_date = lubridate::make_date(year, (quarter - 1L) * 3L + 1L, 1L),
    time = (year - TIME_ORIGIN_YEAR) * 4L + (quarter - 1L)
  )
}

quarter_to_time <- function(quarter_label) {
  parse_quarter(quarter_label)$time[[1]]
}


###############################################################################
# 4) READ EXPORTED STANDARDIZED OUTCOME TABLE
###############################################################################

dat_raw <- read_excel(
  INPUT_FILE,
  sheet = INPUT_SHEET
)

required_cols <- c(QUARTER_COL, N_COL, Y_COL)

missing_required_cols <- setdiff(required_cols, names(dat_raw))

if (length(missing_required_cols) > 0) {
  stop(
    "The input table is missing required columns: ",
    paste(missing_required_cols, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(dat_raw), collapse = ", ")
  )
}


###############################################################################
# 5) PREPARE DATA FOR PLOTTING AND MODELING
###############################################################################

quarter_info <- parse_quarter(dat_raw[[QUARTER_COL]])

dat_all <- dat_raw %>%
  mutate(
    cohort_q = as.character(.data[[QUARTER_COL]]),
    n = as.numeric(.data[[N_COL]]),
    y = as.numeric(.data[[Y_COL]])
  ) %>%
  bind_cols(
    quarter_info %>%
      select(year, quarter, x_date, time)
  )

# Build standard errors and model weights.
#
# Preferred order:
# 1. Use SE directly if available.
# 2. Derive SE from 95% CI if lower/upper confidence limits are available.
# 3. Use n as pragmatic approximate weights if SE/CI columns are unavailable.
if (SE_COL %in% names(dat_all)) {
  dat_all <- dat_all %>%
    mutate(
      se = as.numeric(.data[[SE_COL]]),
      model_weight = 1 / (se^2)
    )
  
} else if (all(c(LCL_COL, UCL_COL) %in% names(dat_all))) {
  dat_all <- dat_all %>%
    mutate(
      se = (
        as.numeric(.data[[UCL_COL]]) -
          as.numeric(.data[[LCL_COL]])
      ) / (2 * 1.96),
      model_weight = 1 / (se^2)
    )
  
} else {
  dat_all <- dat_all %>%
    mutate(
      se = NA_real_,
      model_weight = n
    )
  
  message("No SE or CI columns found. Using n as approximate model weights.")
}

plot_start_time <- quarter_to_time(PLOT_START)
plot_end_time <- quarter_to_time(PLOT_END)

dat_all <- dat_all %>%
  mutate(
    model_weight = if_else(
      is.finite(model_weight) & model_weight > 0,
      model_weight,
      NA_real_
    )
  ) %>%
  filter(
    !is.na(year),
    !is.na(quarter),
    !is.na(x_date),
    !is.na(y),
    !is.na(model_weight),
    time >= plot_start_time,
    time <= plot_end_time
  )

if (nrow(dat_all) == 0) {
  stop("No valid rows remain after data preparation and plot-period filtering.")
}


###############################################################################
# 6) DEFINE PRE, GAP, AND POST PERIODS
###############################################################################

pre_period_end_time <- quarter_to_time(PRE_PERIOD_END)
post_period_start_time <- quarter_to_time(POST_PERIOD_START)

dat_model <- dat_all %>%
  mutate(
    period = case_when(
      time <= pre_period_end_time ~ "pre",
      time >= post_period_start_time ~ "post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

if (sum(dat_model$period == "pre") < 2) {
  stop("The pre period has fewer than two observations.")
}

if (sum(dat_model$period == "post") < 2) {
  stop("The post period has fewer than two observations.")
}


###############################################################################
# 7) FIT WEIGHTED LINEAR TRENDS
###############################################################################

model_pre <- lm(
  y ~ time,
  data = dat_model %>% filter(period == "pre"),
  weights = model_weight
)

model_post <- lm(
  y ~ time,
  data = dat_model %>% filter(period == "post"),
  weights = model_weight
)


###############################################################################
# 8) CREATE PREDICTIONS WITH 95% CONFIDENCE INTERVALS
###############################################################################

prediction_data <- bind_rows(
  dat_model %>%
    filter(period == "pre") %>%
    distinct(time, x_date) %>%
    arrange(time) %>%
    mutate(period = "pre"),
  
  dat_model %>%
    filter(period == "post") %>%
    distinct(time, x_date) %>%
    arrange(time) %>%
    mutate(period = "post")
) %>%
  group_by(period) %>%
  group_modify(~ {
    model <- if (.y$period == "pre") model_pre else model_post
    
    prediction <- predict(
      model,
      newdata = .x,
      se.fit = TRUE
    )
    
    .x %>%
      mutate(
        fitted = prediction$fit,
        fitted_lcl = prediction$fit - 1.96 * prediction$se.fit,
        fitted_ucl = prediction$fit + 1.96 * prediction$se.fit
      )
  }) %>%
  ungroup()


###############################################################################
# 9) CREATE PLOT
###############################################################################

year_ticks <- seq(
  from = min(dat_all$year, na.rm = TRUE),
  to = max(dat_all$year, na.rm = TRUE) + 1L,
  by = 2L
)

x_min <- min(dat_all$x_date, na.rm = TRUE) - 60
x_max <- max(dat_all$x_date, na.rm = TRUE) + 60

plot_object <- ggplot() +
  geom_ribbon(
    data = prediction_data,
    aes(
      x = x_date,
      ymin = fitted_lcl,
      ymax = fitted_ucl,
      group = period
    ),
    fill = PLOT_COLOR,
    alpha = 0.18,
    show.legend = FALSE
  ) +
  geom_point(
    data = dat_model,
    aes(
      x = x_date,
      y = y,
      colour = "Observed"
    )
  ) +
  geom_line(
    data = prediction_data,
    aes(
      x = x_date,
      y = fitted,
      group = period,
      colour = "Modeled"
    ),
    linewidth = 1
  ) +
  scale_x_date(
    breaks = lubridate::make_date(year_ticks, 1, 1),
    labels = year_ticks,
    expand = expansion(mult = c(0.005, 0), add = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = Y_AXIS_BREAKS,
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(Y_AXIS_MIN, Y_AXIS_MAX),
    expand = expansion(mult = c(0, 0)),
    minor_breaks = NULL
  ) +
  coord_cartesian(
    xlim = c(x_min, x_max),
    clip = "off"
  ) +
  scale_colour_manual(
    limits = c("Observed", "Modeled"),
    values = c(
      "Observed" = PLOT_COLOR,
      "Modeled" = PLOT_COLOR
    )
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
    axis.ticks.length = unit(4, "pt"),
    
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
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.85),
      colour = scales::alpha("grey30", 0.3),
      linewidth = 0.2
    ),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(4, 6, 4, 6),
    legend.text = element_text(size = 11, colour = "#000000")
  ) +
  labs(
    title = OUTCOME_LABEL,
    x = "Year",
    y = Y_AXIS_LABEL,
    colour = NULL
  )

print(plot_object)


###############################################################################
# 10) SAVE PLOT
###############################################################################

ggsave(
  filename = OUTPUT_PNG_FILE,
  plot = plot_object,
  width = PLOT_WIDTH_IN,
  height = PLOT_HEIGHT_IN,
  units = "in",
  dpi = PLOT_DPI
)

ggsave(
  filename = OUTPUT_SVG_FILE,
  plot = plot_object,
  device = svglite::svglite,
  width = PLOT_WIDTH_IN,
  height = PLOT_HEIGHT_IN,
  units = "in"
)

message("Done. Plot written to: ", OUTPUT_PNG_FILE, " and ", OUTPUT_SVG_FILE)
