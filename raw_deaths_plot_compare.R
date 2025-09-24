# ---- packages ----
# install.packages(c("data.table","MMWRweek"))  # if needed
library(data.table)
library(MMWRweek)

# ---- paths (edit if needed) ----
file_deaths <- "data/0-4 - Provisional Mortality Statistics, 2018 through Last Week.csv"
file_new    <- "data/tes_fakes/raw_data_digitized.csv"

# ---- cutoff ----
cutoff_date <- as.Date("2024-12-31")

# ---- helpers ----
pick_date_column <- function(DT) {
  cn <- names(DT)
  cand <- cn[grepl("\\bdate\\b|day", tolower(cn))]
  if (length(cand) == 0L) cand <- cn
  for (x in cand) {
    px <- suppressWarnings(as.IDate(DT[[x]]))
    if (any(!is.na(px))) return(x)
  }
  stop("Couldn't find a parsable date column in the second dataset.")
}

pick_value_column <- function(DT) {
  cn <- names(DT)
  cand <- cn[ tolower(cn) %in% c("deaths","value","count","n") ]
  cand <- unique(c(cand, setdiff(cn, cand)))  # fallback: try others
  for (x in cand) {
    vx <- suppressWarnings(as.numeric(DT[[x]]))
    if (any(!is.na(vx))) return(x)
  }
  stop("Couldn't find a numeric value column in the second dataset.")
}

# ---- 1) read & clean original weekly file ----
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

# Parse Year/Week and compute week-ending Date (Saturday)
code_vec <- trimws(d[[nm_code]])
year_chr <- sub("^.*?(\\d{4})/(\\d{1,2})$", "\\1", code_vec)
week_chr <- sub("^.*?(\\d{4})/(\\d{1,2})$", "\\2", code_vec)

d[, Year := suppressWarnings(as.integer(year_chr))]
d[, Week := suppressWarnings(as.integer(week_chr))]
d[, Date := as.Date(NA)]
d[!is.na(Year) & !is.na(Week) & Week >= 1 & Week <= 53,
  Date := MMWRweek2Date(Year, Week, 7)]  # 7 = Saturday

# Keep only valid weekly rows, order, and drop last 5 weeks
deaths_weekly <- d[!is.na(Date) & !is.na(get(nm_deaths)),
                   .(Date, Deaths = as.integer(get(nm_deaths)))][order(Date)]
if (nrow(deaths_weekly) > 5L) {
  deaths_weekly <- deaths_weekly[1:(.N - 5L)]
}

# *** CAP the CDC series at 2024-12-31 ***
deaths_weekly <- deaths_weekly[Date <= cutoff_date]

# ---- 2) read & weekly-select (no summing) the second file ----
nd <- fread(file_new)

# pick columns
col_date  <- pick_date_column(nd)
col_value <- pick_value_column(nd)

# parse and compute MMWR year/week for each day
nd[, date_parsed := suppressWarnings(as.IDate(get(col_date)))]
nd <- nd[!is.na(date_parsed)]

mw <- MMWRweek::MMWRweek(nd$date_parsed)
nd[, `:=`(MMWRyear = mw$MMWRyear, MMWRweek = mw$MMWRweek)]

# derive the MMWR week-ending Saturday from year+week
nd[, Week_Ending := as.IDate(MMWRweek2Date(MMWRyear, MMWRweek, 7))]

# numeric values
nd[, val_num := suppressWarnings(as.numeric(get(col_value)))]

# For each MMWR week, pick the exact Week_Ending value if present,
# otherwise the closest date within that week to the Week_Ending
nearest_per_week <- nd[!is.na(Week_Ending) & !is.na(val_num),
                       .SD[ which.min(abs(as.integer(date_parsed - Week_Ending))) ],
                       by = .(Date = Week_Ending)
]

# One value per MMWR week-ending Saturday
new_weekly <- nearest_per_week[, .(Date = as.Date(Date), Deaths_new = val_num)][order(Date)]

# *** CAP the 'new' series at 2024-12-31 ***
new_weekly <- new_weekly[Date <= cutoff_date]

# ---- 3) align and plot (prettier) ----
op <- par(
  mar = c(7, 5, 3, 2),   # room for vertical x labels + y label
  cex = 1.0
)

# Ticks every 5th week AFTER capping
all_dates <- sort(unique(c(deaths_weekly$Date, new_weekly$Date)))
ticks <- if (length(all_dates)) all_dates[seq(1, length(all_dates), by = 5)] else as.Date(character())

# Indices for markers/labels every 10th point
pts_cdc <- if (nrow(deaths_weekly)) seq(1, nrow(deaths_weekly), by = 10) else integer()
pts_new <- if (nrow(new_weekly))    seq(1, nrow(new_weekly),    by = 10) else integer()

plot(
  deaths_weekly$Date, deaths_weekly$Deaths,
  type = "l",
  xlab = "", ylab = "Deaths (0–4 years)",
  main = "Weekly Deaths (0–4): CDC vs TES Fake Values",
  ylim = range(c(deaths_weekly$Deaths, new_weekly$Deaths_new), na.rm = TRUE),
  xlim = range(all_dates),
  col = "black", lwd = 2.4, xaxt = "n",
  cex.lab = 1.25, cex.main = 1.35, cex.axis = 0.95
)

# Light grid
grid(nx = NA, ny = NULL, lty = "dotted")

# X axis: every 5th week, vertical labels
if (length(ticks)) {
  axis(1, at = ticks, labels = format(ticks, "%Y-%m-%d"), las = 2, cex.axis = 0.9)
}
mtext("Week ending (MMWR, Saturday)", side = 1, line = 5)

# Red series (dotted, thicker)
lines(new_weekly$Date, new_weekly$Deaths_new, lty = 3, col = "red", lwd = 2.4)

# Markers every 10th point
if (length(pts_cdc)) points(deaths_weekly$Date[pts_cdc], deaths_weekly$Deaths[pts_cdc], pch = 16, cex = 0.9, col = "black")
if (length(pts_new)) points(new_weekly$Date[pts_new],    new_weekly$Deaths_new[pts_new],  pch = 16, cex = 0.9, col = "red")

# Numeric labels on those markers
if (length(pts_cdc)) {
  text(deaths_weekly$Date[pts_cdc], deaths_weekly$Deaths[pts_cdc] + 5,
       labels = deaths_weekly$Deaths[pts_cdc], cex = 0.7, col = "black", pos = 3)
}
if (length(pts_new)) {
  text(new_weekly$Date[pts_new], new_weekly$Deaths_new[pts_new] + 5,
       labels = round(new_weekly$Deaths_new[pts_new]),  # rounded red labels
       cex = 0.7, col = "red", pos = 3)
}

# Legend on the RIGHT
legend(
  "topright",
  legend = c("CDC weekly deaths (0–4)", "TES Fake Values"),
  lty = c(1, 3), col = c("black", "red"),
  lwd = 2.4, pch = c(16, 16), pt.cex = 0.9,
  bty = "n", cex = 1.0, inset = 0.01
)

par(op)
