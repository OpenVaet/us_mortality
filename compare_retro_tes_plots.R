# install.packages(c("data.table","MMWRweek","ggplot2","scales")) # if needed
library(data.table)
library(MMWRweek)
library(ggplot2)
library(scales)

# ---- 1) Load data ----
# File 1: digitized weekly excess mortality (date in "week", value in "excess")
f1 <- fread("data/tes_fakes/digitized_weekly_excess_mortality.csv")
# File 2: henjin values (MMWR year/week with columns: year, week, dead, excess)
f2 <- fread("data/tes_fakes/henjin_values.csv")

# ---- 2) Convert File 1 date -> MMWR year/week ----
f1[, week_date := as.Date(week)]   # Keep this original date
mmwr <- MMWRweek(f1$week_date)
f1[, `:=`(
  year = mmwr$MMWRyear,
  week = mmwr$MMWRweek
)]
# Keep original date in the data table
f1 <- f1[, .(week_date, year, week, excess_digitized = excess)]

# ---- 3) Clean File 2 ----
f2[, week := as.integer(week)]
f2[, year := as.integer(year)]
# Coerce 'excess' to numeric (empty cells -> NA)
f2[, excess_henjin := suppressWarnings(as.numeric(excess))]
f2 <- f2[, .(year, week, dead, excess_henjin)]

# ---- 4) Filter to requested years & merge on (year, week) ----
yrs <- 2019:2024
f1 <- f1[year %in% yrs]
f2 <- f2[year %in% yrs]

dt <- merge(f1, f2, by = c("year","week"), all = FALSE)

# If there are multiple rows per (year,week), collapse (rare but safe)
if (dt[, any(duplicated(dt, by = c("year","week")))]) {
  dt <- dt[, .(
    excess_digitized = mean(excess_digitized, na.rm = TRUE),
    excess_henjin    = mean(excess_henjin,    na.rm = TRUE),
    dead             = mean(dead,             na.rm = TRUE)
  ), by = .(year, week)]
}

# ---- 5) Differences & significance ----
dt[, diff := excess_digitized - excess_henjin]

med_diff <- dt[, median(diff, na.rm = TRUE)]
mad_raw  <- dt[, mad(diff, constant = 1, na.rm = TRUE)]  # raw MAD
use_mad  <- is.finite(mad_raw) && mad_raw > 0

if (use_mad) {
  thr <- 2.5 * mad_raw
  thr_note <- sprintf("2.5 × MAD (MAD=%.3f)", mad_raw)
} else {
  sd_diff <- dt[, sd(diff, na.rm = TRUE)]
  thr <- 2 * sd_diff
  thr_note <- sprintf("2 × SD (SD=%.3f)", sd_diff %||% NA_real_)
  if (!is.finite(thr) || is.na(thr)) thr <- 0
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

dt[, significant := abs(diff - med_diff) > thr]


# ---- 6) Display table of significant weeks ----
dt[, mmwr_label := sprintf("%d-W%02d", year, week)]
sig <- dt[significant == TRUE][order(year, week)]

# Keep original date for readability
sig_out <- sig[, .(
  week_date,         # <-- original date from file 1
  mmwr_label, year, week,
  excess_digitized, excess_henjin, diff
)]

print(sig_out)
fwrite(sig_out, "significant_variations_2019_2024.csv")


# ---- 7) Plot with significant weeks highlighted ----
# Build an MMWR mid-week date for spacing
dt[, mmwr_date := MMWRweek2Date(year, week, 4)]

p <- ggplot(dt, aes(x = mmwr_date, y = diff)) +
  geom_hline(yintercept = med_diff, linetype = "dashed") +
  { if (thr > 0) geom_hline(yintercept = c(med_diff - thr, med_diff + thr),
                            linetype = "dotted") } +
  geom_point(aes(shape = significant), size = 2) +
  scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y\n%b",
               limits = range(dt$mmwr_date, na.rm = TRUE)) +
  labs(
    x = NULL,
    y = "Difference (excess_digitized − excess_henjin)",
    title = "Significant variations between digitized excess and henjin excess (MMWR weeks)",
    subtitle = sprintf("Years 2019–2024 | Significant if |diff − median| > %s", thr_note)
  ) +
  theme_minimal(base_size = 11)

p
ggsave("significant_variations_2019_2024.png", p, width = 10, height = 5, dpi = 200)

# ---- 8) Summary by year (optional) ----
year_summary <- dt[, .(
  n_weeks  = .N,
  n_sig    = sum(significant, na.rm = TRUE),
  share_sig = percent(mean(significant, na.rm = TRUE))
), by = year][order(year)]
print(year_summary)
