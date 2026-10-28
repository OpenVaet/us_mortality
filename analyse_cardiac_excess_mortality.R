#!/usr/bin/env Rscript
# analyse_cardiac_excess_mortality.R
# CDC WONDER MMWR weekly UCD deaths, ages 5-14 and 15-24 combined.
# Usage (from project root):
#   Rscript analyse_cardiac_excess_mortality.R
# Optional:
#   Rscript analyse_cardiac_excess_mortality.R "path/to/input.csv" "analysis"
# Required packages: install.packages(c("ggplot2", "ggrepel"))
#
# IMPORTANT: This export uses SELECTED ICD-10 underlying-cause codes including R99;
# these are not exclusively cardiac diagnoses. These are counts, NOT age-adjusted
# mortality rates. Incomplete/provisional data should not be interpreted as final.

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1L) args[[1L]] else
  file.path("data", "Provisional Mortality Statistics, 2018 through Last Week Cardiac.csv")
output_dir <- if (length(args) >= 2L) args[[2L]] else "analysis"

# Adjust these options if desired.
baseline_years <- 2018:2020
study_years <- 2021:2026
seasonal_bandwidth_weeks <- 4.0  # circular Gaussian kernel around each MMWR week
annotation_step <- 10L           # label weeks 10, 20, 30, 40, 50 of each study year
extreme_min_abs_sigma <- 2.0    # minimum absolute historical-standardized deviation
extreme_min_prominence <- 1.3   # contrast to neighboring weeks (in sigma units)
width_px <- 2400
height_px <- 1200
image_dpi <- 150

for (pkg in c("ggplot2", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Missing package '%s'. Run install.packages(c('ggplot2','ggrepel')).", pkg))
  }
}
if (!file.exists(input_file)) stop("Input file does not exist: ", input_file)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Parse only the weekly records, not the WONDER footer / query information / total.
raw <- utils::read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
needed <- c("MMWR Week Code", "Deaths")
if (!all(needed %in% names(raw))) {
  stop("CSV is missing: ", paste(setdiff(needed, names(raw)), collapse = ", "))
}
code <- trimws(as.character(raw[["MMWR Week Code"]]))
is_week <- !is.na(code) & grepl("^20[0-9]{2}/[0-9]{2}$", code)
rows <- raw[is_week, , drop = FALSE]
if (nrow(rows) == 0L) stop("No weekly rows of the form YYYY/WW were found.")

weekly_in <- data.frame(
  year = as.integer(substr(rows[["MMWR Week Code"]], 1, 4)),
  week = as.integer(substr(rows[["MMWR Week Code"]], 6, 7)),
  deaths = suppressWarnings(as.numeric(rows[["Deaths"]]))
)
if (anyDuplicated(weekly_in[c("year", "week")])) {
  stop("Duplicate year/week rows found; this script expects an already aggregated CSV.")
}
if (any(is.na(weekly_in$deaths))) {
  warning("Some weeks have nonnumeric/hidden death counts. Keeping them as missing.")
}
# A WONDER export can contain a grand total including weeks omitted from the
# displayed table. Check it, but do not impute an individual hidden week.
footer_idx <- which(trimws(as.character(raw[["Notes"]])) == "Total")
footer_total <- if (length(footer_idx) == 1L) {
  suppressWarnings(as.numeric(raw[["Deaths"]][footer_idx]))
} else NA_real_
visible_total <- sum(weekly_in$deaths, na.rm = TRUE)

# MMWR weeks start Sunday; week 1 is the week containing January 4.
# MMWR dates are the week-ending Saturdays. This avoids locale-dependent
# parsing of English month names on a French Windows installation.
mmwr_week1_end <- function(year) {
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  jan4 - as.POSIXlt(jan4)$wday + 6L
}
mmwr_nweeks <- function(year) {
  as.integer((mmwr_week1_end(year + 1L) - mmwr_week1_end(year)) / 7)
}
weekly_in$date <- as.Date(
  vapply(seq_len(nrow(weekly_in)), function(i) {
    as.numeric(mmwr_week1_end(weekly_in$year[i]) + 7L * (weekly_in$week[i] - 1L))
  }, numeric(1)), origin = "1970-01-01"
)

# Re-create all scheduled weeks. A week absent from a WONDER export may have
# been suppressed; it must NOT silently become a zero-death observation.
years <- seq.int(min(c(baseline_years, study_years)),
                 max(c(baseline_years, study_years)))
grid <- do.call(rbind, lapply(years, function(y) {
  n <- if (y == max(study_years)) {
    max(weekly_in$week[weekly_in$year == y])
  } else {
    mmwr_nweeks(y)
  }
  ww <- seq_len(n)
  data.frame(year = y, week = ww,
             date = mmwr_week1_end(y) + 7L * (ww - 1L))
}))
if (anyNA(grid$week)) stop("The input must contain the last study year.")
weekly <- merge(grid, weekly_in[c("year", "week", "deaths")],
                by = c("year", "week"), all.x = TRUE, sort = FALSE)
weekly <- weekly[order(weekly$date), ]
rownames(weekly) <- NULL
weekly$code <- sprintf("%04d/%02d", weekly$year, weekly$week)
weekly$period <- ifelse(weekly$year %in% baseline_years, "baseline", "study")

baseline <- weekly[weekly$year %in% baseline_years & !is.na(weekly$deaths), ]
if (length(unique(baseline$year)) != length(baseline_years) || nrow(baseline) < 100L) {
  stop("The baseline has insufficient weekly information for 2018-2020.")
}

# Nonparametric seasonally smoothed EXPECTED DEATHS for MMWR weeks 1..53.
# Each week pools all 3 baseline years and nearby weeks, using circular
# Gaussian weights (week 53 wraps toward week 1). No post-2020 outcome data
# enter the baseline. No trend is extrapolated into 2026.
cyclic_distance <- function(a, b) {
  d <- abs(a - b)
  pmin(d, 52 - d)
}
seasonal_mean <- vapply(1:53, function(w) {
  d <- cyclic_distance(baseline$week, w)
  weight <- exp(-0.5 * (d / seasonal_bandwidth_weeks)^2)
  sum(weight * baseline$deaths) / sum(weight)
}, numeric(1))

# Descriptive historical residual SD: typical week-to-week variability in
# 2018-2020 around the seasonal baseline. It is NOT the standard error of the
# baseline mean and the resulting values are NOT formal hypothesis-test z scores.
baseline_residuals <- baseline$deaths - seasonal_mean[baseline$week]
historical_sd <- stats::sd(baseline_residuals)
if (!is.finite(historical_sd) || historical_sd <= 0) {
  stop("Cannot estimate positive baseline residual standard deviation.")
}
weekly$expected <- seasonal_mean[weekly$week]
weekly$lower_2sd <- pmax(0, weekly$expected - 2 * historical_sd)
weekly$upper_2sd <- weekly$expected + 2 * historical_sd
weekly$excess_deaths <- weekly$deaths - weekly$expected
weekly$excess_pct <- 100 * weekly$excess_deaths / weekly$expected
weekly$sigma <- weekly$excess_deaths / historical_sd
weekly$reference_sd <- historical_sd

# Ensure missing weekly counts make actual gaps in ggplot's observation lines.
weekly$run_id <- cumsum(is.na(weekly$deaths))

# Annotation rules: (a) every 10 MMWR weeks within each study year; and
# (b) prominent local maxima/minima, using actual *unsmoothed* weekly sigma.
# A symmetric neighborhood prevents labeling every small saw-tooth fluctuation.
weekly$annotation_type <- NA_character_
base_label <- weekly$year %in% study_years & !is.na(weekly$deaths) &
  (weekly$week %% annotation_step == 0L)
weekly$annotation_type[base_label] <- "10-week interval"

for (y in study_years) {
  idx <- which(weekly$year == y)
  if (length(idx) < 5L) next
  for (j in seq_along(idx)) {
    if (j <= 2L || j >= length(idx) - 1L) next
    neighbors_local <- seq.int(max(1L, j - 3L), min(length(idx), j + 3L))
    candidate <- idx[j]
    other <- idx[setdiff(neighbors_local, j)]
    z <- weekly$sigma[candidate]
    surrounding <- weekly$sigma[other]
    if (!is.finite(z) || any(!is.finite(surrounding))) next
    reference <- stats::median(surrounding)
    peak <- z > max(surrounding) && z >= extreme_min_abs_sigma &&
      (z - reference) >= extreme_min_prominence
    trough <- z < min(surrounding) && z <= -extreme_min_abs_sigma &&
      (reference - z) >= extreme_min_prominence
    if (peak) weekly$annotation_type[candidate] <- "local peak"
    if (trough) weekly$annotation_type[candidate] <- "local trough"
  }
}

# Keep the recurring labels even if they are not peaks/troughs.
labels <- weekly[!is.na(weekly$annotation_type), , drop = FALSE]
labels$label <- sprintf("%+.1f\u03c3", labels$sigma)
labels$direction <- ifelse(labels$sigma >= 0, "above", "below")

# Convert to a tidy, shareable dataset and year summary.
export <- weekly[c("year", "week", "code", "date", "deaths", "expected",
                   "excess_deaths", "excess_pct", "sigma", "reference_sd",
                   "lower_2sd", "upper_2sd", "period", "annotation_type")]
utils::write.csv(export, file.path(output_dir, "cardiac_excess_weekly_2018_2026.csv"),
                 row.names = FALSE, na = "")

summary_years <- do.call(rbind, lapply(study_years, function(y) {
  x <- weekly[weekly$year == y, ]
  ok <- !is.na(x$deaths)
  observed <- sum(x$deaths[ok])
  expected <- sum(x$expected[ok])
  data.frame(year = y, weeks_available = sum(ok), missing_weeks = sum(!ok),
             last_observed_week = if (any(ok)) max(x$week[ok]) else NA_integer_,
             observed_deaths = observed, expected_deaths = expected,
             excess_deaths = observed - expected,
             excess_percent = 100 * (observed / expected - 1))
}))
utils::write.csv(summary_years,
                 file.path(output_dir, "cardiac_excess_annual_summary.csv"),
                 row.names = FALSE)
utils::write.csv(labels[c("code", "date", "deaths", "expected", "sigma",
                           "annotation_type", "label")],
                 file.path(output_dir, "cardiac_excess_annotations.csv"),
                 row.names = FALSE)

# X / Twitter landscape aspect ratio 2:1, 2400 x 1200 pixels by default.
# The same chart is also exported as a zoom (2021-2025) because the exceptional
# 2026 rise otherwise compresses the visible variability in earlier years.
library(ggplot2)
make_plot <- function(data, year_from, year_to, title, filename, label_data = labels) {
  x <- data[data$year >= year_from & data$year <= year_to, , drop = FALSE]
  ann <- label_data[label_data$year >= max(2021L, year_from) &
                      label_data$year <= year_to, , drop = FALSE]
  pre <- x[x$year %in% baseline_years, , drop = FALSE]
  post <- x[x$year %in% study_years, , drop = FALSE]
  y_upper <- max(c(x$deaths, x$upper_2sd), na.rm = TRUE)

  p <- ggplot() +
    geom_ribbon(data = x,
                aes(x = date, ymin = lower_2sd, ymax = upper_2sd),
                fill = "#D9E6F0", alpha = 0.62) +
    geom_line(data = x, aes(x = date, y = expected),
              linewidth = 0.95, color = "#285476", linetype = "dashed") +
    geom_line(data = pre, aes(x = date, y = deaths, group = run_id),
              linewidth = 0.7, color = "#748393", alpha = 0.95) +
    geom_line(data = post, aes(x = date, y = deaths, group = run_id),
              linewidth = 0.95, color = "#C44536") +
    geom_point(data = ann, aes(x = date, y = deaths),
               shape = 21, fill = "white", color = "#C44536", stroke = 0.8,
               size = 2.5) +
    ggrepel::geom_label_repel(
      data = ann,
      aes(x = date, y = deaths, label = label, color = direction),
      seed = 726, size = 3.5, fontface = "bold", fill = "white",
      label.size = 0.12, label.padding = grid::unit(0.12, "lines"),
      box.padding = 0.3, point.padding = 0.12,
      min.segment.length = 0,
      segment.color = "#AAB5BD", segment.alpha = 0.8,
      max.overlaps = Inf, max.time = 3.5, max.iter = 15000,
      show.legend = FALSE) +
    scale_color_manual(values = c(above = "#AC332F", below = "#126F98"),
                       guide = "none") +
    scale_x_date(date_breaks = if (year_to - year_from > 2L) "1 year" else "2 months",
                 date_labels = if (year_to - year_from > 2L) "%Y" else "%b %Y",
                 expand = expansion(mult = c(0.016, 0.06))) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 7),
                       expand = expansion(mult = c(0.045, 0.13))) +
    coord_cartesian(ylim = c(0, y_upper * 1.20), clip = "off") +
    labs(
      title = title,
      subtitle = "Ages 5-24  |  Weekly underlying-cause deaths  |  Seasonal baseline: 2018-2020",
      x = NULL, y = "Deaths per week",
      caption = paste0(
        "Gray = observed 2018-2020  |  Red = observed 2021-2026  |  Dashed blue = expected baseline  |  Blue band = baseline +/- 2 historical SD\n",
        "Labels: (observed - baseline) / 2018-2020 residual SD; descriptive sigma, not p-values.  ",
        "Source: CDC WONDER export (28 Oct 2026); includes R99. 2025-26 provisional. Counts are not population-adjusted."
      )
    ) +
    theme_minimal(base_size = 17) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 22, color = "#1B2835",
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = 13.7, color = "#405467",
                                    margin = margin(b = 17)),
      axis.title.y = element_text(face = "bold", size = 14,
                                  margin = margin(r = 12)),
      axis.text = element_text(color = "#435667", size = 12.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#E9EDF0", linewidth = 0.25),
      panel.grid.major.y = element_line(color = "#E5E9ED", linewidth = 0.3),
      plot.caption = element_text(size = 9.4, color = "#5A6875", hjust = 0,
                                  lineheight = 1.15, margin = margin(t = 14)),
      plot.margin = margin(t = 27, r = 36, b = 16, l = 25)
    )

  ggplot2::ggsave(file.path(output_dir, filename), plot = p,
                  width = width_px / image_dpi,
                  height = height_px / image_dpi,
                  dpi = image_dpi, units = "in", bg = "white", limitsize = FALSE)
  invisible(p)
}

# Main shareable figure and two useful detailed figures.
make_plot(weekly, 2018L, 2026L,
          "Selected cardiac + unspecified causes | US, 2018-2026",
          "cardiac_excess_2018_2026_X.png")
make_plot(weekly, 2021L, 2025L,
          "Selected cardiac + unspecified causes | 2021-2025 detail",
          "cardiac_excess_2021_2025_X.png")
make_plot(weekly, 2026L, 2026L,
          "Selected cardiac + unspecified causes | 2026 detail",
          "cardiac_excess_2026_X.png")

cat(sprintf("Historical baseline residual SD = %.3f deaths/week\n", historical_sd))
if (is.finite(footer_total)) {
  cat(sprintf("CDC WONDER footer total: %.0f; sum of displayed weeks: %.0f; difference: %.0f\n",
              footer_total, visible_total, footer_total - visible_total))
}
cat("Weeks absent from CSV (left missing, NOT zero):\n")
missing <- weekly[is.na(weekly$deaths), c("code", "date")]
if (nrow(missing)) print(missing, row.names = FALSE) else cat("  None\n")
cat("\n2021-2026 excess summary (available reported weeks only):\n")
print(summary_years, row.names = FALSE, digits = 5)
cat("\nOutputs written to: ", normalizePath(output_dir, winslash = "/"), "\n", sep = "")
