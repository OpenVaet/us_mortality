# Packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
})

# Source: National "alldata" (single-year-of-age) files, 2000–2009
# Directory listing with files nc-est2009-alldata-*-file01.csv ... file29.csv
# https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/national/asrh/
base_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/national/asrh/"
prefix   <- "nc-est2009-alldata-r-file"  # R = Resident population (see file layout PDF)
files    <- sprintf("%s%02d.csv", prefix, 1:29)

# Helper to safely read one file (skip silently if not found)
read_one <- function(fname) {
  url <- paste0(base_url, fname)
  tryCatch(
    readr::read_csv(url, show_col_types = FALSE),
    error = function(e) {
      message("Skipping (not found or unreadable): ", fname)
      NULL
    }
  )
}

# Read and bind everything that exists
raw <- files %>% map(read_one) %>% compact() %>% bind_rows()

# Keep resident universe, July points, and 2000–2009, exclude the total-age rollup (AGE=999)
# File layout (variables like UNIVERSE, YEAR, MONTH, AGE, TOT_POP) documented at:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2009/nc-est2009-alldata.pdf
tidy <- raw %>%
  filter(
    UNIVERSE == "R",
    YEAR >= 2000, YEAR <= 2009,
    MONTH == 7,
    AGE != 999
  ) %>%
  transmute(
    Year = YEAR,
    Age = AGE,                  # single-year of age (0,1,2,…,100+ per layout; may appear as 100 for 100+)
    Population = TOT_POP        # total (both sexes)
  ) %>%
  arrange(Year, Age)

# Write output
out_path <- "data/us_population_by_age_2000_2009_single_year.csv"
write_csv(tidy, out_path)
cat("✅ Wrote:", out_path, "with", nrow(tidy), "rows\n")
