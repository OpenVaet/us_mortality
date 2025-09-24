# install.packages(c("data.table","MMWRweek"))  # if needed
library(data.table)
library(MMWRweek)

# ---- paths ----
file_deaths <- "data/0-4 - Provisional Mortality Statistics, 2018 through Last Week.csv"

# ---- 1) read & clean ----
d <- fread(
  file_deaths,
  sep = ",", quote = "\"", fill = TRUE,
  encoding = "UTF-8",
  na.strings = c("Not Applicable", "NA", "")
)

# Strip BOM & trim headers
names(d) <- sub("^\ufeff", "", names(d))
setnames(d, old = names(d), new = trimws(names(d)))

# Locate columns
nm_code   <- grep("^MMWR\\s*Week\\s*Code$", names(d), value = TRUE)
nm_deaths <- grep("^Deaths$", names(d), value = TRUE)
stopifnot(length(nm_code) == 1L, length(nm_deaths) == 1L)

# ---- 2) parse Year/Week and compute week-ending Date ----
code_vec <- trimws(d[[nm_code]])
year_chr <- sub("^.*?(\\d{4})/(\\d{1,2})$", "\\1", code_vec)
week_chr <- sub("^.*?(\\d{4})/(\\d{1,2})$", "\\2", code_vec)

d[, Year := suppressWarnings(as.integer(year_chr))]
d[, Week := suppressWarnings(as.integer(week_chr))]
d[, Date := as.Date(NA)]
d[!is.na(Year) & !is.na(Week) & Week >= 1 & Week <= 53,
  Date := MMWRweek2Date(Year, Week, 7)]

# Keep only valid weekly rows, order, and truncate last 5 weeks
deaths_weekly <- d[!is.na(Date) & !is.na(get(nm_deaths)),
                   .(Date, Deaths = as.integer(get(nm_deaths)))][order(Date)]
deaths_weekly <- deaths_weekly[1:(.N - 5)]   # drop last 5 rows

# ---- 3) plot ----
op <- par(
  mar = c(6, 5, 3, 2),
  cex = 1.0
)

# Choose every 10th point for markers/labels
idx <- seq(1, nrow(deaths_weekly), by = 10)

# Explicit weeks to mark on X axis
special_weeks <- data.table(
  Year = c(2018, 2019, 2020, 2020, 2021, 2022, 2023, 2024),
  Week = c(  1,    1,    1,   53,   52,   52,   52,   52)
)
special_weeks[, Date := MMWRweek2Date(Year, Week, 7)]  # Saturday ending date

plot(
  deaths_weekly$Date, deaths_weekly$Deaths,
  type = "l",
  xlab = "Week ending (MMWR, Saturday)",
  ylab = "Deaths (0–4 years)",
  main = "Raw Weekly Deaths (0–4) — Last 3 Weeks Removed",
  ylim = c(200, 550),
  lwd = 2.4,
  cex.lab = 1.3,
  cex.main = 1.4,
  cex.axis = 1.0,
  xaxt = "n"  # we'll add custom X axis
)

grid(nx = NA, ny = NULL, lty = "dotted")

# Custom X axis with chosen weeks, vertical labels
axis(1,
     at = special_weeks$Date,
     labels = paste0(special_weeks$Year, "/", sprintf("%02d", special_weeks$Week)),
     las = 2, cex.axis = 0.9)

# Add markers every 10 points
points(deaths_weekly$Date[idx], deaths_weekly$Deaths[idx],
       pch = 16, col = "black", cex = 0.8)

# Add numeric labels slightly above each marker
text(deaths_weekly$Date[idx], deaths_weekly$Deaths[idx] + 5,
     labels = deaths_weekly$Deaths[idx],
     cex = 0.7, col = "black", pos = 3)

par(op)


