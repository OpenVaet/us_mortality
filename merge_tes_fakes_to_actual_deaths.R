# install.packages(c("data.table","MMWRweek")) # if needed
library(data.table)
library(MMWRweek)

# -------- paths --------
file_deaths <- "data/0-4 - Provisional Mortality Statistics, 2018 through Last Week.csv"
file_digit  <- "data/tes_fakes/digitized_weekly_excess_mortality.csv"
out_file    <- "data/tes_fakes/weekly_0_4_deaths_with_tes_fake_excess.csv"

# -------- 1) read deaths (robust) --------
d1 <- fread(
  file_deaths,
  sep = ",",
  quote = "\"",
  fill = TRUE,
  encoding = "UTF-8",
  na.strings = c("Not Applicable", "NA", "")
)
# Clean headers
names(d1) <- sub("^\ufeff", "", names(d1))
setnames(d1, old = names(d1), new = trimws(names(d1)))

# Column names (order-safe)
nm_code   <- grep("^MMWR\\s*Week\\s*Code$", names(d1), value = TRUE)
nm_deaths <- grep("^Deaths$", names(d1), value = TRUE)
stopifnot(length(nm_code) == 1L, length(nm_deaths) == 1L)

# Extract Year and Week from "YYYY/WW" (strict at line end)
code_vec <- trimws(d1[[nm_code]])
year_chr <- sub("^.*?(\\d{4})/(\\d{1,2})$", "\\1", code_vec)
week_chr <- sub("^.*?(\\d{4})/(\\d{1,2})$", "\\2", code_vec)

d1[, Year := suppressWarnings(as.integer(year_chr))]
d1[, Week := suppressWarnings(as.integer(week_chr))]

# Compute week-ending DATE from valid (Year, Week) only (MMWR ends on Saturday = 7)
d1[, Date := as.Date(NA)]
d1[!is.na(Year) & !is.na(Week) & Week >= 1 & Week <= 53,
   Date := MMWRweek2Date(Year, Week, 7)]

# Build canonical MMWRWeekCode with zero-padded week to match join key
d1[, MMWRWeekCode_std := ifelse(!is.na(Year) & !is.na(Week),
                                sprintf("%d/%02d", Year, Week), NA_character_)]

# Keep just the legit weekly rows (drop the 70 non-week lines)
deaths_weekly <- d1[!is.na(MMWRWeekCode_std) & !is.na(Deaths),
                    .(Year, Week, MMWRWeekCode = MMWRWeekCode_std, Date,
                      Deaths = as.integer(get(nm_deaths)))]

# -------- 2) read digitized & convert to MMWR --------
d2 <- fread(file_digit)
setnames(d2, old = names(d2), new = trimws(names(d2)))  # expect: week, excess
d2[, week := as.Date(week)]
d2[, excess := as.numeric(excess)]

mw <- MMWRweek(d2$week)
d2[, `:=`(Year = mw$MMWRyear, Week = mw$MMWRweek)]
d2[, MMWRWeekCode := sprintf("%d/%02d", Year, Week)]
digitized_excess <- d2[, .(MMWRWeekCode, Excess = excess)]

# Deduplicate if needed
if (digitized_excess[, any(duplicated(MMWRWeekCode))]) {
  digitized_excess <- digitized_excess[
    , .(Excess = mean(Excess, na.rm = TRUE)), by = MMWRWeekCode
  ]
}

# -------- 3) join & order --------
out <- merge(deaths_weekly, digitized_excess, by = "MMWRWeekCode", all.x = TRUE)
setorder(out, Year, Week)

# Final column order (includes your requested Week column)
setcolorder(out, c("Year", "Week", "MMWRWeekCode", "Date", "Deaths", "Excess"))

# -------- 4) write & quick checks --------
fwrite(out, out_file)

cat("Rows written:", nrow(out), "\n")
cat("Weeks with Excess filled:", sum(!is.na(out$Excess)), "\n")
print(head(out, 10))
