library(tidyverse)

# --- read source files ---
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/nc-est2019-agesex-res.csv
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/national/asrh/nc-est2024-agesex-res.csv
v19 <- read_csv("data/nc-est2019-agesex-res.csv", show_col_types = FALSE)
v24 <- read_csv("data/nc-est2024-agesex-res.csv", show_col_types = FALSE)

# 2000–2009 single-year-of-age (Year,Age,Population) - assumes that get_alt_2010_data.R has been executed first.
pop_2000_2009 <- read_csv("data/us_intercensal_2000_2010_single_year.csv",
                          show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Age = as.integer(Age),
    Population = as.numeric(Population)
  )

# helper: keep SEX==0, pivot POPESTIMATE columns to Year/Population
to_long <- function(df) {
  df %>%
    filter(SEX == 0, AGE != 999) %>%
    select(AGE, starts_with("POPESTIMATE")) %>%
    pivot_longer(
      cols = starts_with("POPESTIMATE"),
      names_to = "year_col",
      values_to = "Population"
    ) %>%
    mutate(
      Year = as.integer(stringr::str_extract(year_col, "\\d{4}")),
      Age = as.integer(AGE),
      Population = as.numeric(Population)
    ) %>%
    select(Year, Age, Population)
}

# 2010–2019 from the 2019 file; 2020–2024 from the 2024 file
pop_2011_2019 <- to_long(v19) %>% filter(Year >= 2011, Year <= 2019)
pop_2020_2024 <- to_long(v24) %>% filter(Year >= 2020, Year <= 2024)

# merge all periods, dedupe, sort
merged <- bind_rows(
  pop_2000_2009,
  pop_2011_2019,
  pop_2020_2024
) %>%
  distinct(Year, Age, .keep_all = TRUE) %>%
  arrange(Year, Age)

# write output
write_csv(merged, "data/merged_yearly_population_by_age_2010_correction.csv")
