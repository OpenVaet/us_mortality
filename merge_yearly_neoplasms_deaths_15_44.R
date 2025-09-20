# Merge C00-D48 (ages 15–44) into 2010–2023 file
# Output: data/C00-D48 - 15-44 - 2010-to-2023.csv

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

in1 <- "data/C00-D48 - 15-44 - Underlying Cause of Death, 1999-2020.csv"
in2 <- "data/C00-D48 - 15-44 - Underlying Cause of Death, 2018-2023, Single Race.csv"
out <- "data/C00-D48 - 15-44 - 2010-to-2023.csv"

# Helper: standardize one file
std <- function(path, src_label) {
  df <- read_csv(path, col_types = cols(.default = col_character()))

  # Prefer "Year", fall back to "Year Code"
  year_raw <- if ("Year" %in% names(df)) df$Year else df$`Year Code`
  # Prefer long age label, fall back to code
  age_raw  <- if ("Ten-Year Age Groups" %in% names(df))
                df$`Ten-Year Age Groups` else df$`Ten-Year Age Groups Code`

  tibble(
    Notes      = df$Notes,
    Year       = suppressWarnings(as.integer(parse_number(year_raw))),
    AgeGroup10 = str_trim(str_replace(age_raw, "\\s*years$", "")),
    Deaths     = parse_number(df$Deaths),
    Population = parse_number(df$Population),
    CrudeRate  = parse_number(df$`Crude Rate`),
    .source    = src_label
  ) %>%
    # keep requested window and drop totals
    filter(Year >= 2010, Year <= 2023) %>%
    filter(is.na(Notes) | Notes != "Total") %>%
    filter(!is.na(AgeGroup10), AgeGroup10 != "")
}

d1 <- std(in1, "1999-2020")
d2 <- std(in2, "2018-2023 Single Race")

# Prefer Single Race (newer) where years overlap
priority <- c("1999-2020" = 1L, "2018-2023 Single Race" = 2L)

merged <- bind_rows(d1, d2) %>%
  mutate(.prio = priority[.source]) %>%
  arrange(Year, AgeGroup10, desc(.prio)) %>%
  distinct(Year, AgeGroup10, .keep_all = TRUE) %>%
  select(Year, AgeGroup10, Deaths, Population, CrudeRate) %>%
  arrange(Year, AgeGroup10)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv(merged, out)

cat("Wrote:", out, "with", nrow(merged), "rows\n")
