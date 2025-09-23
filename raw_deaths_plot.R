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

# ---- 3) plot raw weekly deaths ----
op <- par(mar = c(4, 4, 2, 1))
plot(deaths_weekly$Date, deaths_weekly$Deaths,
     type = "l",
     xlab = "Week ending (MMWR, Saturday)",
     ylab = "Deaths (0–4 years)",
     main = "Raw Weekly Deaths (0–4) — Last 3 Weeks Removed",
     ylim = c(200, 550))
grid()
par(op)
