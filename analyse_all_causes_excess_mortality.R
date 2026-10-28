#!/usr/bin/env Rscript
# CDC WONDER: all-cause mortality, United States, ages 5-14 + 15-24.
# Baseline 2018-2020, observation 2021-2026. X-ready 2400 x 1200 figures.
# Same Gaussian seasonal baseline and descriptive residual SD used in the
# original selected-cardiac analysis; 10-week and major-extremum sigma labels.
#
# From the project root (CSV files in data/):
#   Rscript analyse_all_causes_excess_mortality.R
# Optional input / output overrides:
#   Rscript analyse_all_causes_excess_mortality.R "data/my_file.csv" "analysis/all_causes"
#
# Install once: install.packages(c("ggplot2", "ggrepel"))
#
# IMPORTANT:
# * These are REPORTED deaths, not a reporting-delay-corrected mortality series.
# * 2025 and 2026 are provisional and likely incomplete, especially late 2026.
# * 2018-2020 includes the COVID-affected year 2020; sensitivity is exported.
# * Sigma = (observed - expected)/SD(2018-2020 residuals). This is a
#   DESCRIPTIVE standardized deviation, NOT a significance test / p-value.
# * Week 99 = unknown time of occurrence; excluded from dated curves/totals.

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1L) args[[1L]] else
  file.path("data", "Provisional Mortality Statistics, 2018 through Last Week All Causes.csv")
output_dir <- if (length(args) >= 2L) args[[2L]] else
  file.path("analysis", "all_causes")
r99_file <- file.path(dirname(input_file),
  "Provisional Mortality Statistics, 2018 through Last Week R99 Only.csv")

baseline_years <- 2018:2020
study_years <- 2021:2026
seasonal_bandwidth_weeks <- 4.0
annotation_step <- 10L
extreme_min_abs_sigma <- 2.0
extreme_min_prominence <- 1.3
recent_caution_weeks <- 6L     # visual caution, NOT a CDC completeness threshold
width_px <- 2400L
height_px <- 1200L
image_dpi <- 150L

for (pkg in c("ggplot2", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Missing package '", pkg, "'. Run install.packages(c('ggplot2','ggrepel')).")
}
if (!file.exists(input_file)) stop("Missing CSV: ", input_file)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Parse CDC WONDER export, excluding its totals/notes and Week 99 ----
read_wonder <- function(path) {
  raw <- utils::read.csv(path, check.names = FALSE, fill = TRUE,
                         stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                         na.strings = c("", "NA"))
  required <- c("MMWR Week Code", "Deaths")
  if (!all(required %in% names(raw)))
    stop("Missing WONDER columns in ", path)
  code <- trimws(as.character(raw[["MMWR Week Code"]]))
  valid_week <- !is.na(code) & grepl("^20[0-9]{2}/(0[1-9]|[1-4][0-9]|5[0-3])$", code)
  data <- data.frame(
    year = as.integer(substr(code[valid_week], 1L, 4L)),
    week = as.integer(substr(code[valid_week], 6L, 7L)),
    deaths = suppressWarnings(as.numeric(raw[["Deaths"]][valid_week]))
  )
  if (!nrow(data)) stop("No dated MMWR weeks found: ", path)
  if (anyDuplicated(data[c("year", "week")]))
    stop("Duplicate MMWR week. Group all selected age groups into one series.")
  if (any(!is.finite(data$deaths))) stop("Non-numeric visible weekly count: ", path)
  total <- NA_real_
  if ("Notes" %in% names(raw)) {
    trow <- which(trimws(as.character(raw[["Notes"]])) == "Total")
    if (length(trow) == 1L)
      total <- suppressWarnings(as.numeric(raw[["Deaths"]][trow]))
  }
  other_week <- !is.na(code) & grepl("^20[0-9]{2}/99$", code)
  undated <- sum(suppressWarnings(as.numeric(raw[["Deaths"]][other_week])),
                 na.rm = TRUE)
  list(data = data, grand_total = total, undated = undated)
}

# MMWR week 1 is the Sunday-Saturday week containing January 4.
# Calculate dates without parsing English month names (works on French Windows).
mmwr_week1_end <- function(year) {
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  jan4 - as.POSIXlt(jan4)$wday + 6L
}
mmwr_nweeks <- function(year) {
  as.integer((mmwr_week1_end(year + 1L) - mmwr_week1_end(year)) / 7)
}

find_source_end <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
               collapse = " ")
  hit <- regmatches(txt, regexpr(
    "Deaths occurring through [A-Za-z]+ [0-9]{1,2}, [0-9]{4}", txt))
  if (!length(hit) || identical(hit, "")) return(as.Date(NA))
  a <- strsplit(sub("^Deaths occurring through ", "", hit), " +")[[1L]]
  mm <- match(a[1L], month.name)
  dd <- suppressWarnings(as.integer(gsub(",", "", a[2L], fixed = TRUE)))
  yy <- suppressWarnings(as.integer(a[3L]))
  if (anyNA(c(mm, dd, yy))) return(as.Date(NA))
  as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
}

export <- read_wonder(input_file)
observed <- export$data
source_end <- find_source_end(input_file)
latest_year <- max(study_years)
last_displayed_week <- max(observed$week[observed$year == latest_year])
if (is.na(source_end)) {
  warning("Source cutoff date unavailable; using latest visible 2026 week.")
  last_week <- last_displayed_week
} else {
  last_week <- as.integer((source_end - mmwr_week1_end(latest_year)) / 7) + 1L
  if (!is.finite(last_week) || last_week < last_displayed_week ||
      last_week > mmwr_nweeks(latest_year))
    stop("Source date and week codes disagree: ", source_end)
}

# Complete calendar grid: absent all-cause weekly cells would be suspicious.
years <- min(baseline_years):max(study_years)
grid <- do.call(rbind, lapply(years, function(y) {
  n <- if (y == latest_year) last_week else mmwr_nweeks(y)
  data.frame(year = y, week = seq_len(n),
    date = mmwr_week1_end(y) + 7L * (seq_len(n) - 1L))
}))
weekly <- merge(grid, observed, by = c("year", "week"), all.x = TRUE,
                sort = FALSE)
weekly <- weekly[order(weekly$date), , drop = FALSE]
rownames(weekly) <- NULL
weekly$code <- sprintf("%04d/%02d", weekly$year, weekly$week)
weekly$period <- ifelse(weekly$year %in% baseline_years, "baseline", "study")
missing_week <- weekly[is.na(weekly$deaths), c("code", "date")]
if (nrow(missing_week)) {
  utils::write.csv(missing_week, file.path(output_dir, "all_causes_missing_weeks.csv"),
                   row.names = FALSE)
  stop("Dated weeks absent from all-cause export: ",
       paste(missing_week$code, collapse = ", "),
       ". Check the age selection and CDC suppression settings; do not fill zero.")
}
if (any(weekly$deaths < 10))
  warning("Found visible counts < 10; check WONDER suppression settings.")
if (is.finite(export$grand_total) &&
    abs(sum(weekly$deaths) + export$undated - export$grand_total) > 0.5)
  warning("Grand total is not reconciled by weekly + Week 99 counts; ",
          "check whether some suppressed rows are excluded.")

# ---- Seasonal baseline: 2018-2020, cyclic Gaussian smoother ----
# Week 53 is intentionally adjacent to week 1; same method as the original.
cyclic_distance <- function(a, b) {
  d <- abs(a - b)
  pmin(d, 52 - d)
}
seasonal_fit <- function(baseline_rows) {
  mu <- vapply(1:53, function(w) {
    dist <- cyclic_distance(baseline_rows$week, w)
    weight <- exp(-0.5 * (dist / seasonal_bandwidth_weeks)^2)
    sum(weight * baseline_rows$deaths) / sum(weight)
  }, numeric(1))
  sd <- stats::sd(baseline_rows$deaths - mu[baseline_rows$week])
  if (!is.finite(sd) || sd <= 0) stop("Invalid historical residual SD.")
  list(expected_by_week = mu, historical_sd = sd)
}

baseline <- weekly[weekly$year %in% baseline_years, , drop = FALSE]
fit <- seasonal_fit(baseline)
weekly$expected <- fit$expected_by_week[weekly$week]
weekly$historical_sd <- fit$historical_sd
weekly$lower_2sd <- pmax(0, weekly$expected - 2 * fit$historical_sd)
weekly$upper_2sd <- weekly$expected + 2 * fit$historical_sd
weekly$excess_deaths <- weekly$deaths - weekly$expected
weekly$excess_pct <- 100 * weekly$excess_deaths / weekly$expected
weekly$sigma <- weekly$excess_deaths / fit$historical_sd
weekly$provisional <- weekly$year >= 2025
weekly$recent_caution <- weekly$year == latest_year &
  weekly$week >= (last_week - recent_caution_weeks + 1L)

# Optional sensitivity: exclude 2020 from the baseline (not the main result).
fit_18_19 <- seasonal_fit(weekly[weekly$year %in% 2018:2019, , drop = FALSE])
weekly$expected_2018_2019 <- fit_18_19$expected_by_week[weekly$week]
weekly$excess_2018_2019 <- weekly$deaths - weekly$expected_2018_2019
weekly$sigma_2018_2019 <- weekly$excess_2018_2019 / fit_18_19$historical_sd

# ---- Label each 10th MMWR week and prominent local max/min ----
weekly$annotation_type <- NA_character_
regular <- weekly$year %in% study_years &
  weekly$week %% annotation_step == 0L
weekly$annotation_type[regular] <- "10-week interval"
for (y in study_years) {
  idx <- which(weekly$year == y)
  if (length(idx) < 5L) next
  for (j in 3L:(length(idx) - 2L)) {
    k <- idx[j]
    z <- weekly$sigma[k]
    near <- weekly$sigma[idx[setdiff((j - 2L):(j + 2L), j)]]
    middle <- stats::median(near)
    if (z > max(near) && z >= extreme_min_abs_sigma &&
        z - middle >= extreme_min_prominence)
      weekly$annotation_type[k] <- "local peak"
    if (z < min(near) && z <= -extreme_min_abs_sigma &&
        middle - z >= extreme_min_prominence)
      weekly$annotation_type[k] <- "local trough"
  }
}
# Last provisional week often has the worst reporting lag; explicitly label it,
# instead of pretending it represents a genuine decline.
last_idx <- which(weekly$year == latest_year & weekly$week == last_week)
if (length(last_idx) == 1L && abs(weekly$sigma[last_idx]) >= 2)
  weekly$annotation_type[last_idx] <- "latest week (incomplete)"
labels <- weekly[!is.na(weekly$annotation_type), , drop = FALSE]
labels$label <- sprintf("%+.1f\u03c3", labels$sigma)
labels$direction <- ifelse(labels$sigma >= 0, "above", "below")

utils::write.csv(weekly[c("year", "week", "code", "date", "deaths", "expected",
  "excess_deaths", "excess_pct", "sigma", "historical_sd", "lower_2sd",
  "upper_2sd", "period", "provisional", "recent_caution",
  "expected_2018_2019", "excess_2018_2019", "sigma_2018_2019",
  "annotation_type")],
  file.path(output_dir, "all_causes_weekly_2018_2026.csv"), row.names = FALSE)
utils::write.csv(labels[c("code", "date", "deaths", "expected", "sigma",
  "annotation_type", "label")],
  file.path(output_dir, "all_causes_annotations.csv"), row.names = FALSE)

summary_years <- do.call(rbind, lapply(study_years, function(y) {
  x <- weekly[weekly$year == y, , drop = FALSE]
  expected <- sum(x$expected)
  observed <- sum(x$deaths)
  expected_alt <- sum(x$expected_2018_2019)
  data.frame(
    year = y, weeks = nrow(x), last_week = max(x$week),
    observed_deaths = observed, expected_2018_2020 = expected,
    excess_deaths = observed - expected,
    excess_pct = 100 * (observed / expected - 1),
    expected_2018_2019 = expected_alt,
    excess_2018_2019 = observed - expected_alt,
    excess_pct_2018_2019 = 100 * (observed / expected_alt - 1),
    status = if (y >= 2025) "provisional, not corrected for reporting delay"
             else "final"
  )
}))
utils::write.csv(summary_years,
  file.path(output_dir, "all_causes_annual_summary.csv"), row.names = FALSE)

# ---- 2400 x 1200 charts for X ----
last6 <- weekly[weekly$recent_caution, , drop = FALSE]
recent_window_start <- min(last6$date) - 3.5
recent_window_end <- max(last6$date) + 3.5
make_plot <- function(year_start, year_end, suffix, heading) {
  x <- weekly[weekly$year >= year_start & weekly$year <= year_end, , drop = FALSE]
  ann <- labels[labels$year >= year_start & labels$year <= year_end, , drop = FALSE]
  pre <- x[x$year %in% baseline_years, , drop = FALSE]
  post <- x[x$year %in% study_years, , drop = FALSE]
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = x,
      ggplot2::aes(x = date, ymin = lower_2sd, ymax = upper_2sd),
      fill = "#DAE8F2", alpha = 0.75) +
    ggplot2::geom_line(data = x, ggplot2::aes(x = date, y = expected),
      color = "#285476", linetype = "dashed", linewidth = 0.95) +
    ggplot2::geom_line(data = pre, ggplot2::aes(x = date, y = deaths),
      color = "#778593", linewidth = 0.68) +
    ggplot2::geom_line(data = post, ggplot2::aes(x = date, y = deaths),
      color = "#C44536", linewidth = 0.9) +
    ggplot2::geom_point(data = ann, ggplot2::aes(x = date, y = deaths),
      shape = 21, fill = "white", color = "#C44536", size = 2.4,
      stroke = 0.8) +
    ggrepel::geom_label_repel(data = ann,
      ggplot2::aes(x = date, y = deaths, label = label, color = direction),
      seed = 726, size = 3.1, fontface = "bold", fill = "white",
      label.size = 0.12, label.padding = grid::unit(0.12, "lines"),
      box.padding = 0.28, point.padding = 0.12,
      max.overlaps = Inf, min.segment.length = 0,
      segment.color = "#AAB5BD", max.time = 3.0,
      max.iter = 15000, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c(above = "#AC332F",
                                           below = "#126F98"), guide = "none") +
    ggplot2::scale_x_date(
      date_breaks = if (year_end - year_start > 2L) "1 year" else "2 months",
      date_labels = if (year_end - year_start > 2L) "%Y" else "%b %Y",
      expand = ggplot2::expansion(mult = c(0.015, 0.055))) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 7),
      expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::labs(
      title = heading,
      subtitle = "Ages 5-24 | Weekly all-cause deaths | Seasonal baseline: 2018-2020",
      x = NULL, y = "Reported deaths per week",
      caption = paste0(
        "Gray = 2018-20 | Red = 2021-26 | Dashed blue = expected | Blue band = +/- 2 historical residual SD\n",
        "Labels: (observed - expected) / historical residual SD; descriptive, not p-values. ",
        "2025-26 provisional.\n",
        "Last 6 weeks shaded = extra caution only (not completeness-corrected). ",
        "CDC WONDER, 28 Oct 2026; data through 17 Oct. Not population-adjusted.")) +
    ggplot2::theme_minimal(base_size = 17) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 22,
        color = "#1B2835", margin = ggplot2::margin(b = 8)),
      plot.subtitle = ggplot2::element_text(size = 13.3,
        color = "#405467", margin = ggplot2::margin(b = 16)),
      axis.title.y = ggplot2::element_text(face = "bold", size = 14,
        margin = ggplot2::margin(r = 12)),
      axis.text = ggplot2::element_text(color = "#435667", size = 12.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#E9EDF0", linewidth = 0.25),
      panel.grid.major.y = ggplot2::element_line(color = "#E5E9ED", linewidth = 0.3),
      plot.caption = ggplot2::element_text(size = 9, color = "#5A6875",
        hjust = 0, lineheight = 1.13, margin = ggplot2::margin(t = 14)),
      plot.margin = ggplot2::margin(t = 25, r = 35, b = 17, l = 24)
    )
  # Position the caution band beneath the curves. Explanatory, not a cutoff.
  if (year_end >= latest_year) {
    p <- p + ggplot2::geom_rect(
      data = data.frame(xmin = recent_window_start,
                        xmax = recent_window_end),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = "#E8AA85", alpha = 0.10)
  }
  outfile <- file.path(output_dir, paste0("all_causes_excess_", suffix, "_X.png"))
  ggplot2::ggsave(outfile, p, width = width_px / image_dpi,
    height = height_px / image_dpi, units = "in", dpi = image_dpi,
    bg = "white", limitsize = FALSE)
  cat("Written: ", outfile, "\n", sep = "")
  invisible(p)
}

make_plot(2018, 2026, "2018_2026", "All-cause mortality | US, 2018-2026")
make_plot(2021, 2025, "2021_2025", "All-cause excess mortality | US, 2021-2025")
make_plot(2026, 2026, "2026", "All-cause mortality | US, 2026 (provisional)")

# ---- Optional: quantify the R99 coding effect in 2026 ----
if (file.exists(r99_file)) {
  rr <- read_wonder(r99_file)$data
  names(rr)[names(rr) == "deaths"] <- "r99_deaths"
  compare <- merge(weekly[weekly$year == latest_year,
    c("year", "week", "code", "date", "deaths", "expected")],
    rr, by = c("year", "week"), all.x = TRUE, sort = FALSE)
  compare <- compare[order(compare$week), , drop = FALSE]
  compare$r99_suppressed_or_zero <- is.na(compare$r99_deaths)
  compare$r99_share_pct <- 100 * compare$r99_deaths / compare$deaths
  compare$r99_min <- ifelse(compare$r99_suppressed_or_zero, 0, compare$r99_deaths)
  compare$r99_max <- ifelse(compare$r99_suppressed_or_zero, 9, compare$r99_deaths)
  compare$r99_share_min <- 100 * compare$r99_min / compare$deaths
  compare$r99_share_max <- 100 * compare$r99_max / compare$deaths
  utils::write.csv(compare,
    file.path(output_dir, "all_causes_vs_R99_weekly_2026.csv"),
    row.names = FALSE, na = "")
  known <- compare[!compare$r99_suppressed_or_zero, , drop = FALSE]
  end_ann <- known[known$week %in% c(30L, 35L, 39L, 40L, last_week), , drop = FALSE]
  end_ann$label <- sprintf("%.1f%%", end_ann$r99_share_pct)
  # A missing R99 week is 0..9; it is a gap, not a zero on the plot.
  compare$run_id <- cumsum(compare$r99_suppressed_or_zero)
  p2 <- ggplot2::ggplot(compare,
      ggplot2::aes(x = date, y = r99_share_pct)) +
    ggplot2::geom_line(ggplot2::aes(group = run_id), color = "#C44536",
                       linewidth = 1.2, na.rm = TRUE) +
    ggplot2::geom_point(color = "#C44536", size = 2.1, na.rm = TRUE) +
    ggrepel::geom_label_repel(data = end_ann,
      ggplot2::aes(label = label), fill = "white", color = "#AC332F",
      fontface = "bold", seed = 726, size = 4, max.overlaps = Inf) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b",
      expand = ggplot2::expansion(mult = c(0.03, 0.06))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
      limits = c(0, max(70, ceiling(max(compare$r99_share_max) / 10) * 10)),
      breaks = scales::breaks_pretty(n = 7)) +
    ggplot2::labs(
      title = "Unspecified (R99) share of reported deaths | US, 2026",
      subtitle = "Ages 5-24 | R99 underlying cause divided by all-cause deaths",
      x = NULL, y = "R99 share of reported all-cause deaths",
      caption = paste0(
        "Gaps: R99 weeks omitted by WONDER (0-9 deaths, not known zeros). ",
        "All-cause counts include R99.\n",
        "Recent 2026 counts provisional and incomplete; increasing R99 share ",
        "does not show an increase in actual cardiac deaths.")) +
    ggplot2::theme_minimal(base_size = 17) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 22),
      plot.subtitle = ggplot2::element_text(color = "#405467"),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 35, b = 15, l = 20))
  f <- file.path(output_dir, "all_causes_R99_share_2026_X.png")
  ggplot2::ggsave(f, p2, width = width_px / image_dpi,
    height = height_px / image_dpi, units = "in", dpi = image_dpi,
    bg = "white", limitsize = FALSE)
  cat("Written: ", f, "\n", sep = "")
} else {
  cat("Optional R99 comparison skipped (not in same folder): ", r99_file, "\n")
}

report <- c(
  paste0("Input: ", normalizePath(input_file, winslash = "/")),
  "Cohort: US ages 5-14 + 15-24, weekly underlying-cause all-cause deaths",
  paste0("Source end date: ", source_end),
  paste0("Latest included week: ", latest_year, "/", sprintf("%02d", last_week)),
  paste0("Number of dated weekly rows: ", nrow(weekly)),
  paste0("Dated weekly total: ", sum(weekly$deaths)),
  paste0("Unknown-week (Week 99) total: ", export$undated),
  paste0("CDC reported grand total: ", export$grand_total),
  paste0("Baseline SD (2018-2020): ", signif(fit$historical_sd, 7)),
  paste0("Sensitivity baseline SD (2018-2019): ",
         signif(fit_18_19$historical_sd, 7)),
  "Annual outputs use dated weeks only. No population adjustment.",
  "2025-2026 figures are provisional, not completeness-corrected.",
  "2020 is included in the primary baseline; sensitivity excludes it.",
  "Sigma values are descriptive standardizations, NOT p-values."
)
writeLines(report, file.path(output_dir, "all_causes_diagnostics.txt"))
cat(paste(report, collapse = "\n"), "\n\n")
print(summary_years, digits = 5, row.names = FALSE)
cat("\nCompleted. Files in: ",
    normalizePath(output_dir, winslash = "/"), "\n", sep = "")
