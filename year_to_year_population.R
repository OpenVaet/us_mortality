#########################################################
## Year-to-year population table (wide layout)         ##
## Columns: Age Group | 2015 | 2016 | 2016-2015 | …    ##
## Data file expected at: "data/merged_deaths.csv"     ##
#########################################################

## 1 ── Packages ───────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)    # dplyr, tidyr, readr …
  library(knitr)        # kable()
  library(kableExtra)   # HTML / LaTeX styling
})

## 2 ── Read the monthly file ──────────────────────────
pop_raw <- read_csv(
  "data/merged_deaths.csv",
  col_types = cols(
    year          = col_integer(),
    month         = col_character(),
    age_group_5   = col_character(),
    deaths        = col_double(),
    covid_deaths  = col_double(),
    population    = col_double()
  )
) %>% 
  mutate(month = as.integer(month))

## 3 ── Average population per (year, age group) ──────
pop_year_age <- pop_raw %>% 
  group_by(year, age_group_5) %>% 
  summarise(population = mean(population, na.rm = TRUE),
            .groups   = "drop")

## 4 ── Add an “All ages” total row ────────────────────
pop_year_total <- pop_year_age %>% 
  group_by(year) %>% 
  summarise(population = sum(population), .groups = "drop") %>% 
  mutate(age_group_5 = "All ages")

pop_year <- bind_rows(pop_year_age, pop_year_total)

## 5 ── Pivot wider so years are columns ───────────────
pop_wide <- pop_year %>% 
  pivot_wider(
    names_from  = year,
    values_from = population
  ) %>% 
  arrange(match(age_group_5, c("All ages", sort(unique(pop_year$age_group_5)))))

## 6 ── Compute year-on-year differences ───────────────
# helper to make Δ columns
make_delta <- function(df, from_yr, to_yr) {
  df[[paste0(to_yr, "-", from_yr)]] <- df[[as.character(to_yr)]] - df[[as.character(from_yr)]]
  df
}

for (yr in 2016:2023) {
  pop_wide <- make_delta(pop_wide, yr - 1, yr)
}

## 7 ── Select & order the columns exactly as requested ─
col_order <- c(
  "age_group_5",
  "2015", "2016", "2016-2015",
  "2017", "2017-2016",
  "2018", "2018-2017",
  "2019", "2019-2018",
  "2020", "2020-2019",
  "2021", "2021-2020",
  "2022", "2022-2021",
  "2023", "2023-2022"
)

pop_wide <- pop_wide[, col_order]

## 8 ── Round & prettify ───────────────────────────────
pop_wide <- pop_wide %>% 
  mutate(across(-age_group_5, ~round(.x))) %>% 
  rename(`Age Group` = age_group_5)

## 9 ── Print with kableExtra ──────────────────────────
kable(pop_wide,
      caption   = "Year-to-year change in average monthly population",
      align     = "r",
      format.args = list(big.mark = ",")) |>
  kable_styling(full_width = FALSE, position = "center") |>
  row_spec(0, bold = TRUE) |>
  collapse_rows(columns = 1, valign = "top")
