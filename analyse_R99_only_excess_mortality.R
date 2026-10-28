#!/usr/bin/env Rscript
# Standalone CDC WONDER weekly mortality analysis, ages 5-24.
# Original research question: baseline 2018-2020, excess 2021-2026,
# sigma annotations every 10 MMWR weeks and at prominent extrema.
#
# Use from the project root, with input in data/:
#   Rscript analyse_R99_only_excess_mortality.R
# Optional overrides:
#   Rscript analyse_R99_only_excess_mortality.R "path/to/input.csv" "analysis/my_output"
#
# Dependencies: install.packages(c("ggplot2", "ggrepel"))
# Figures: 2400 x 1200 pixels (2:1), designed for X.
#
# IMPORTANT: CDC WONDER hides zero and suppressed (<10) weekly cells.
# Missing weekly rows here are 0..9, NEVER known zeros. The seasonal
# baseline is estimated via interval-censored Poisson imputation using ONLY
# 2018-2020; its residual SD provides DESCRIPTIVE sigma, not a test statistic.
# Counts are not age-adjusted or population-adjusted. 2025-2026 provisional.

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1L) args[[1L]] else
  file.path("data", "Provisional Mortality Statistics, 2018 through Last Week R99 Only.csv")
output_dir <- if (length(args) >= 2L) args[[2L]] else
  file.path("analysis", "R99_only")

series_title <- "Unspecified causes of mortality (R99)"
series_subtitle <- "ICD-10 R99 only"
series_slug <- "R99_only"
source_note <- "R99 is not a cardiac diagnosis."

baseline_years <- 2018:2020
study_years <- 2021:2026
seasonal_bandwidth_weeks <- 4.0
annotation_step <- 10L
extreme_min_abs_sigma <- 2.0
extreme_min_prominence <- 1.3
width_px <- 2400L
height_px <- 1200L
image_dpi <- 150L

for (pkg in c("ggplot2", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Missing R package '", pkg,
         "'. Install with install.packages(c('ggplot2','ggrepel')).")
}
if (!file.exists(input_file)) stop("File not found: ", input_file)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Read the whole WONDER file, keeping only actual YYYY/WW data rows.
raw <- utils::read.csv(input_file, check.names = FALSE, fill = TRUE,
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                       na.strings = c("", "NA"))
required <- c("MMWR Week Code", "Deaths")
if (!all(required %in% names(raw)))
  stop("Missing CSV column(s): ",
       paste(setdiff(required, names(raw)), collapse = ", "))
code <- trimws(as.character(raw[["MMWR Week Code"]]))
is_week <- !is.na(code) & grepl("^20[0-9]{2}/[0-9]{2}$", code)
source <- raw[is_week, , drop = FALSE]
if (!nrow(source)) stop("No weekly rows in YYYY/WW format were found.")
observed <- data.frame(
  year = as.integer(substr(source[["MMWR Week Code"]], 1L, 4L)),
  week = as.integer(substr(source[["MMWR Week Code"]], 6L, 7L)),
  deaths = suppressWarnings(as.numeric(source[["Deaths"]]))
)
if (anyDuplicated(observed[c("year", "week")]))
  stop("Duplicate year/week rows. Export one combined 5-24 age group.")
if (any(!is.finite(observed$deaths)))
  stop("A visible weekly Deaths cell is not a valid numeric value.")
if (min(observed$deaths) < 10)
  warning("At least one displayed cell is below 10; check suppression settings.")

footer_total <- NA_real_
if ("Notes" %in% names(raw)) {
  ids <- which(trimws(as.character(raw[["Notes"]])) == "Total")
  if (length(ids) == 1L)
    footer_total <- suppressWarnings(as.numeric(raw[["Deaths"]][ids]))
}
visible_total <- sum(observed$deaths)

# MMWR weeks end on Saturday. Week 1 includes 4 January.
# Independent of French/English Windows locale settings.
mmwr_week1_end <- function(year) {
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  jan4 - as.POSIXlt(jan4)$wday + 6L
}
mmwr_nweeks <- function(year) {
  as.integer((mmwr_week1_end(year + 1L) - mmwr_week1_end(year)) / 7)
}

# Critical: in a file with suppressed tail weeks, max(displayed week)
# is NOT the last week covered by the WONDER export. Use its own footer:
# "Deaths occurring through October 17, 2026 as of ...".
find_source_end <- function(filepath) {
  lines <- paste(readLines(filepath, warn = FALSE, encoding = "UTF-8"),
                 collapse = " ")
  hit <- regmatches(lines, regexpr(
    "Deaths occurring through [A-Za-z]+ [0-9]{1,2}, [0-9]{4}", lines))
  if (!length(hit) || identical(hit, "")) return(as.Date(NA))
  clean <- sub("^Deaths occurring through ", "", hit)
  a <- strsplit(clean, " +")[[1L]]
  month_no <- match(a[1L], month.name)
  day_no <- as.integer(gsub(",", "", a[2L], fixed = TRUE))
  year_no <- as.integer(a[3L])
  if (is.na(month_no) || is.na(day_no) || is.na(year_no)) return(as.Date(NA))
  as.Date(sprintf("%04d-%02d-%02d", year_no, month_no, day_no))
}
source_end <- find_source_end(input_file)
last_year <- max(study_years)
last_displayed_week <- max(observed$week[observed$year == last_year])
if (is.na(source_end)) {
  warning("WONDER end date not found in footer; using latest displayed week. ",
          "Some suppressed final weeks may be missing from the result!")
  last_week <- last_displayed_week
} else {
  last_week <- as.integer((source_end - mmwr_week1_end(last_year)) / 7) + 1L
  if (!is.finite(last_week) || last_week < last_displayed_week ||
      last_week > mmwr_nweeks(last_year))
    stop("Footer end date conflicts with weekly rows: ", source_end)
}

# Construct the complete expected week grid, including all hidden weeks.
years <- seq.int(min(baseline_years), max(study_years))
grid <- do.call(rbind, lapply(years, function(y) {
  n <- if (y == last_year) last_week else mmwr_nweeks(y)
  data.frame(year = y, week = seq_len(n),
             date = mmwr_week1_end(y) + 7L * (seq_len(n) - 1L))
}))
weekly <- merge(grid, observed, by = c("year", "week"), all.x = TRUE,
                sort = FALSE)
weekly <- weekly[order(weekly$date), , drop = FALSE]
rownames(weekly) <- NULL
weekly$code <- sprintf("%04d/%02d", weekly$year, weekly$week)
weekly$suppressed_or_zero <- is.na(weekly$deaths)
weekly$observed_min <- ifelse(weekly$suppressed_or_zero, 0, weekly$deaths)
weekly$observed_max <- ifelse(weekly$suppressed_or_zero, 9, weekly$deaths)
weekly$period <- ifelse(weekly$year %in% baseline_years,
                        "baseline", "study")

# Interval-censored, cyclically smoothed seasonal 2018-2020 baseline.
# For an omitted weekly row, fit Y ~ Poisson(mu_week) conditional on Y <= 9.
# This is an explicit modeling assumption, not an assertion that Y == E[Y].
base <- weekly[weekly$year %in% baseline_years, , drop = FALSE]
if (!all(baseline_years %in% base$year)) stop("Missing baseline years.")
if (sum(!base$suppressed_or_zero) < 20L)
  stop("Too few visible historical cells to fit a defensible baseline.")
cyclic_distance <- function(a, b) {
  d <- abs(a - b)
  pmin(d, 52 - d)
}
W <- vapply(1:53, function(w) {
  d <- cyclic_distance(base$week, w)
  exp(-0.5 * (d / seasonal_bandwidth_weeks)^2)
}, numeric(nrow(base)))
weighted_weekly_mean <- function(y) {
  as.numeric(crossprod(W, y) / colSums(W))
}

truncated_moments <- function(lambda) {
  lambda <- max(lambda, 1e-8)
  k <- 0:9
  prob <- stats::dpois(k, lambda)
  prob <- prob / sum(prob)
  c(mean = sum(k * prob), second = sum(k^2 * prob))
}
censored <- base$suppressed_or_zero
imputed <- base$deaths
imputed[censored] <- 5
mu <- weighted_weekly_mean(imputed)
iter <- 0L
for (iteration in 1:300) {
  iter <- iteration
  if (any(censored)) {
    imputed[censored] <- vapply(mu[base$week[censored]], function(lambda)
      truncated_moments(lambda)[1L], numeric(1))
  }
  mu_next <- weighted_weekly_mean(imputed)
  if (max(abs(mu_next - mu)) < 1e-8) {
    mu <- mu_next
    break
  }
  mu <- mu_next
}
if (iter == 300L) warning("Censored baseline model did not fully converge.")

# Descriptive SD of historical weekly deviations, including the *expected
# squared residuals* of the censored weeks rather than treating their imputed
# means as if they had no sampling variation.
mu_at_base <- mu[base$week]
resid_first <- numeric(nrow(base))
resid_second <- numeric(nrow(base))
if (any(!censored)) {
  k <- which(!censored)
  resid_first[k] <- base$deaths[k] - mu_at_base[k]
  resid_second[k] <- resid_first[k]^2
}
if (any(censored)) {
  k <- which(censored)
  moments <- vapply(mu_at_base[k], truncated_moments,
                    c(mean = 0, second = 0))
  resid_first[k] <- moments["mean", ] - mu_at_base[k]
  resid_second[k] <- moments["second", ] -
    2 * mu_at_base[k] * moments["mean", ] + mu_at_base[k]^2
}
n_baseline <- nrow(base)
r_center <- mean(resid_first)
historical_sd <- sqrt(max(0, (sum(resid_second) -
                             n_baseline * r_center^2) / (n_baseline - 1L)))
if (!is.finite(historical_sd) || historical_sd <= 0)
  stop("Cannot estimate a positive historical residual standard deviation.")

weekly$expected <- mu[weekly$week]
weekly$lower_2sd <- pmax(0, weekly$expected - 2 * historical_sd)
weekly$upper_2sd <- weekly$expected + 2 * historical_sd
weekly$excess_deaths <- weekly$deaths - weekly$expected
weekly$excess_pct <- 100 * weekly$excess_deaths / weekly$expected
weekly$sigma <- weekly$excess_deaths / historical_sd
weekly$sigma_min <- (weekly$observed_min - weekly$expected) / historical_sd
weekly$sigma_max <- (weekly$observed_max - weekly$expected) / historical_sd
weekly$reference_sd <- historical_sd
weekly$run_id <- cumsum(weekly$suppressed_or_zero)

# Label every 10th MMWR week when actually shown, plus major local extrema.
# Suppressed weeks are not assigned a fabricated sigma value or a point label.
weekly$annotation_type <- NA_character_
base_label <- weekly$year %in% study_years &
  !weekly$suppressed_or_zero & weekly$week %% annotation_step == 0L
weekly$annotation_type[base_label] <- "10-week interval"
for (y in study_years) {
  idx <- which(weekly$year == y)
  if (length(idx) < 5L) next
  for (j in seq_along(idx)) {
    if (j <= 2L || j > length(idx) - 2L) next
    cidx <- idx[j]
    z <- weekly$sigma[cidx]
    other <- idx[setdiff(seq.int(j - 2L, j + 2L), j)]
    zs <- weekly$sigma[other]
    if (!is.finite(z) || any(!is.finite(zs))) next
    local_median <- stats::median(zs)
    if (z > max(zs) && z >= extreme_min_abs_sigma &&
        z - local_median >= extreme_min_prominence)
      weekly$annotation_type[cidx] <- "local peak"
    if (z < min(zs) && z <= -extreme_min_abs_sigma &&
        local_median - z >= extreme_min_prominence)
      weekly$annotation_type[cidx] <- "local trough"
  }
}
labels <- weekly[!is.na(weekly$annotation_type), , drop = FALSE]
labels$label <- sprintf("%+.1f\u03c3", labels$sigma)
labels$direction <- ifelse(labels$sigma >= 0, "above", "below")

# Machine-readable outputs: exact counts for visible weeks, ranges for all.
weekly_out <- weekly[c("year", "week", "code", "date", "deaths",
  "suppressed_or_zero", "observed_min", "observed_max", "expected",
  "excess_deaths", "excess_pct", "sigma", "sigma_min", "sigma_max",
  "reference_sd", "lower_2sd", "upper_2sd", "period", "annotation_type")]
utils::write.csv(weekly_out,
  file.path(output_dir, paste0(series_slug, "_weekly_2018_2026.csv")),
  row.names = FALSE, na = "")

summary_years <- do.call(rbind, lapply(study_years, function(y) {
  x <- weekly[weekly$year == y, , drop = FALSE]
  expected <- sum(x$expected)
  lower <- sum(x$observed_min)
  upper <- sum(x$observed_max)
  data.frame(
    year = y, weeks_total = nrow(x), weeks_visible = sum(!x$suppressed_or_zero),
    weeks_hidden_0_to_9 = sum(x$suppressed_or_zero),
    last_week_in_scope = max(x$week),
    visible_reported_deaths = sum(x$deaths, na.rm = TRUE),
    total_deaths_lower_bound = lower,
    total_deaths_upper_bound = upper,
    expected_deaths = expected,
    excess_lower_bound = lower - expected,
    excess_upper_bound = upper - expected,
    excess_pct_lower_bound = 100 * (lower / expected - 1),
    excess_pct_upper_bound = 100 * (upper / expected - 1)
  )
}))
utils::write.csv(summary_years,
  file.path(output_dir, paste0(series_slug, "_annual_summary.csv")),
  row.names = FALSE)
utils::write.csv(labels[c("code", "date", "deaths", "expected", "sigma",
                            "annotation_type", "label")],
  file.path(output_dir, paste0(series_slug, "_annotations.csv")),
  row.names = FALSE)

# Save 2:1, X-ready plot at 2400 x 1200 px.
# Narrow vertical ticks at heights 0..9 indicate hidden/suppressed weeks.
# They are NOT observed death counts or zero values.
make_plot <- function(year_start, year_end, suffix, detail_title) {
  x <- weekly[weekly$year >= year_start & weekly$year <= year_end, , drop = FALSE]
  ann <- labels[labels$year >= year_start & labels$year <= year_end, , drop = FALSE]
  pre <- x[x$year %in% baseline_years, , drop = FALSE]
  post <- x[x$year %in% study_years, , drop = FALSE]
  cens <- x[x$suppressed_or_zero, , drop = FALSE]
  y_top <- max(c(x$deaths, x$upper_2sd, 9), na.rm = TRUE)
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = x,
      ggplot2::aes(x = date, ymin = lower_2sd, ymax = upper_2sd),
      fill = "#D9E6F0", alpha = 0.65) +
    ggplot2::geom_line(data = x, ggplot2::aes(x = date, y = expected),
      color = "#285476", linetype = "dashed", linewidth = 0.95) +
    ggplot2::geom_segment(data = cens,
      ggplot2::aes(x = date, xend = date, y = 0, yend = 9),
      color = "#8B9CA9", alpha = 0.42, linewidth = 2) +
    ggplot2::geom_line(data = pre,
      ggplot2::aes(x = date, y = deaths, group = run_id),
      linewidth = 0.7, color = "#758391") +
    ggplot2::geom_line(data = post,
      ggplot2::aes(x = date, y = deaths, group = run_id),
      linewidth = 0.95, color = "#C44536") +
    ggplot2::geom_point(data = post[!post$suppressed_or_zero, , drop = FALSE],
      ggplot2::aes(x = date, y = deaths), size = 1.1, color = "#C44536") +
    ggplot2::geom_point(data = ann,
      ggplot2::aes(x = date, y = deaths),
      shape = 21, fill = "white", color = "#C44536",
      stroke = 0.8, size = 2.5) +
    ggrepel::geom_label_repel(data = ann,
      ggplot2::aes(x = date, y = deaths, label = label, color = direction),
      seed = 726, size = 3.4, fontface = "bold", fill = "white",
      label.size = 0.12, label.padding = grid::unit(0.12, "lines"),
      box.padding = 0.28, point.padding = 0.12, max.overlaps = Inf,
      min.segment.length = 0, segment.color = "#AAB5BD",
      max.time = 3.0, max.iter = 15000, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c(above = "#AC332F", below = "#126F98"),
                                guide = "none") +
    ggplot2::scale_x_date(
      date_breaks = if (year_end - year_start > 2L) "1 year" else "2 months",
      date_labels = if (year_end - year_start > 2L) "%Y" else "%b %Y",
      expand = ggplot2::expansion(mult = c(0.016, 0.06))) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 7),
      expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::coord_cartesian(ylim = c(0, y_top * 1.20), clip = "off") +
    ggplot2::labs(
      title = detail_title,
      subtitle = paste0("Ages 5-24 | Weekly UCD deaths | ", series_subtitle,
                        " | Baseline 2018-2020"),
      x = NULL, y = "Deaths per week",
      caption = paste0(
        "Gray = observed 2018-20 | Red = observed 2021-26 | Dashed blue = baseline | Blue band = +/- 2 historical SD\n",
        "Gray ticks at 0-9 = suppressed/zero weeks (not zeros). Sigma: (observed - expected)/historical SD; descriptive only.\n",
        "CDC WONDER export, 28 Oct 2026; 2025-26 provisional. ", source_note,
        " Not population-adjusted.")) +
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
      plot.caption = ggplot2::element_text(size = 9.1, color = "#5A6875",
        hjust = 0, lineheight = 1.1, margin = ggplot2::margin(t = 13)),
      plot.margin = ggplot2::margin(t = 27, r = 36, b = 16, l = 25)
    )
  outfile <- file.path(output_dir, paste0(series_slug, "_excess_", suffix, "_X.png"))
  ggplot2::ggsave(outfile, p,
    width = width_px / image_dpi, height = height_px / image_dpi,
    units = "in", dpi = image_dpi, bg = "white", limitsize = FALSE)
  cat("  ", outfile, "\n", sep = "")
  invisible(p)
}

make_plot(2018, 2026, "2018_2026", paste0(series_title, " | US, 2018-2026"))
make_plot(2021, 2025, "2021_2025", paste0(series_title, " | 2021-2025 detail"))
make_plot(2026, 2026, "2026", paste0(series_title, " | 2026 detail"))

report_lines <- c(
  paste0("INPUT: ", normalizePath(input_file, winslash = "/")),
  paste0("STUDY: ", series_title),
  paste0("REFERENCE END DATE: ", if (is.na(source_end)) "not found" else source_end),
  paste0("FINAL WEEK IN SCOPE: ", last_year, "/", sprintf("%02d", last_week)),
  paste0("WEEKS IN GRID: ", nrow(weekly)),
  paste0("VISIBLE WEEKS: ", sum(!weekly$suppressed_or_zero)),
  paste0("HIDDEN WEEKS (0 to 9): ", sum(weekly$suppressed_or_zero)),
  paste0("VISIBLE DEATH TOTAL: ", visible_total),
  paste0("WONDER ALL-WEEK TOTAL: ", footer_total),
  paste0("HIDDEN DEATH TOTAL ACROSS 2018-2026 (exact, from footer): ",
         if (is.finite(footer_total)) footer_total - visible_total else "unknown"),
  paste0("BASELINE VISIBLE WEEKS: ", sum(!base$suppressed_or_zero), "/", nrow(base)),
  paste0("BASELINE SEASONAL MODEL: censored Poisson with Gaussian weekly kernel, bandwidth ",
         seasonal_bandwidth_weeks),
  paste0("BASELINE MODEL ITERATIONS: ", iter),
  paste0("HISTORICAL RESIDUAL SD (DESCRIPTIVE): ", signif(historical_sd, 6)),
  "IMPORTANT: 0-9 is a bound for absent cells, not an imputed observed value.",
  "IMPORTANT: baseline and sigma depend on a Poisson censoring model; no p-values.",
  "IMPORTANT: recent 2025-2026 counts are provisional and subject to revision."
)
writeLines(report_lines,
  file.path(output_dir, paste0(series_slug, "_diagnostics.txt")))
cat(paste(report_lines, collapse = "\n"), "\n\n")
print(summary_years, row.names = FALSE, digits = 5)
cat("\nCompleted. All output files in: ",
    normalizePath(output_dir, winslash = "/"), "\n", sep = "")
