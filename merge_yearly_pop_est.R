library(tidyverse)

# --- read source files ---
v19 <- read_csv("data/nc-est2019-agesex-res.csv", show_col_types = FALSE)
v24 <- read_csv("data/nc-est2024-agesex-res.csv", show_col_types = FALSE)

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
pop_2010_2019 <- to_long(v19) %>% filter(Year >= 2010, Year <= 2019)
pop_2020_2024 <- to_long(v24) %>% filter(Year >= 2020, Year <= 2024)

# merge, dedupe, sort
merged <- bind_rows(pop_2010_2019, pop_2020_2024) %>%
  distinct(Year, Age, .keep_all = TRUE) %>%
  arrange(Year, Age)

# write output
write_csv(merged, "data/merged_yearly_population_by_age.csv")
