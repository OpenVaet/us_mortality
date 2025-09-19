#########################################################
## Cohort aging table by single age (HTML with highlights)
## Δ at (Year N, Age A) = Pop[N, A] − Pop[N−1, A−1]
## Source: data/merged_yearly_population_by_age.csv
#########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

# --- Parameters ---
TARGET_DELTA <- 50000L   # expected cohort change
TOL          <- 0L       # tolerance around TARGET_DELTA (e.g., 2500L)

# --- Load ---
pop <- read_csv("data/merged_yearly_population_by_age.csv", show_col_types = FALSE) %>%
  transmute(Year = as.integer(Year),
            Age  = as.integer(Age),
            Population = as.numeric(Population)) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

yrs <- sort(unique(pop$Year))

# --- Wide matrix of populations (Age rows, Year cols); drop Age 0 in the output
pop_wide_years <- pop %>%
  pivot_wider(id_cols = Age, names_from = Year, values_from = Population) %>%
  arrange(Age) %>%
  filter(Age >= 1)  # “starting in 2010, we never care about Age 0”

# --- Cohort deltas: (N,A) minus (N-1,A-1) ---
delta_long <- pop %>%
  mutate(Year_prev = Year - 1, Age_prev = Age - 1) %>%
  left_join(pop %>% select(Year, Age, Population) %>%
              rename(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
            by = c("Year_prev", "Age_prev")) %>%
  filter(!is.na(Pop_prev), Age >= 1) %>%
  transmute(Age, Year, delta = Population - Pop_prev)

delta_wide <- delta_long %>%
  mutate(delta_col = paste0(Year, "-", Year - 1)) %>%
  select(Age, delta_col, delta) %>%
  pivot_wider(names_from = delta_col, values_from = delta)

# --- Combine absolute years + cohort deltas ---
out <- pop_wide_years %>% left_join(delta_wide, by = "Age")

# --- Column order: Age, 1st year, then (Y, Y-(Y-1)) pairs ---
col_order <- c("Age", as.character(yrs[1]))
for (i in 2:length(yrs)) {
  y <- yrs[i]
  col_order <- c(col_order, as.character(y), paste0(y, "-", y - 1))
}
out <- out[, intersect(col_order, names(out))]

# --- Format & highlight (HTML) ---
HIGHLIGHT_THRESHOLD <- 50000L  # highlight when |delta| > 50k

year_cols  <- grep("^\\d{4}$", names(out), value = TRUE)
delta_cols <- grep("^\\d{4}-\\d{4}$", names(out), value = TRUE)

out_fmt <- out %>%
  # pretty years
  mutate(across(all_of(year_cols),
                ~ ifelse(is.na(.), "", scales::comma(round(.))))) %>%
  # deltas with conditional background
  mutate(across(all_of(delta_cols), \(x) {
    vals <- ifelse(is.na(x), "", scales::comma(round(x)))
    bg   <- ifelse(is.na(x), "transparent",
                   ifelse(abs(x) > HIGHLIGHT_THRESHOLD, "#ffcccc", "transparent"))
    kableExtra::cell_spec(vals, color = "black", background = bg)
  }))

kableExtra::kbl(out_fmt, format = "html", escape = FALSE,
                caption = "Cohort aging: Δ vs prior-year younger cohort; highlighted when |Δ| > 50,000") |>
  kableExtra::kable_styling(full_width = FALSE, position = "center") |>
  kableExtra::row_spec(0, bold = TRUE)
