# Intercensal 2000–2010 (national, single-year age), write Year,Age,Population

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# Choose your annual reference date:
# - July 1 (official annual point used most often), or
# - April 1 (Census Day), useful for direct comparisons to 2010 Census.
USE_APRIL <- FALSE
target_month <- if (USE_APRIL) 4 else 7

url <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/national/us-est00int-alldata.csv"

intercensal <- read_csv(url, show_col_types = FALSE)

out <- intercensal %>%
  # Resident population, national totals by single year of age
  filter(YEAR >= 2000, YEAR <= 2010,
         MONTH == target_month,
         AGE != 999) %>%                # drop all-ages total
  transmute(
    Year       = as.integer(YEAR),
    Age        = as.integer(AGE),      # 100 represents 100+
    Population = as.numeric(TOT_FEMALE)   # Female only
  ) %>%
  arrange(Year, Age)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write_csv(out, "data/us_intercensal_female_2000_2010_single_year.csv")

cat("Wrote: data/us_intercensal_female_2000_2010_single_year.csv (",
    nrow(out), " rows)\n", sep = "")
