###############################################################
## Cohort aging by 5-year age groups (keeps Age 0)
## Δ at year N for a group = sum_A_in_group [ Pop(N,A) - Pop(N-1,A-1) ]
## Source: data/merged_yearly_population_by_age_2020_smoothed.csv
## Requires : smooth_2020_correction.R to run first
###############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

# ---- Params ----
HIGHLIGHT_THRESHOLD <- 250000L  # highlight when |Δ| > 50,000

# ---- Load ----
pop <- read_csv("data/merged_yearly_female_population_by_age_2020_smoothed.csv", show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

yrs     <- sort(unique(pop$Year))
max_age <- max(pop$Age, na.rm = TRUE)

# ---- Collapse ages BEFORE deltas: top-code at 85 (≥85 -> 85) ----
AGE_TOP <- 85L  # *** was 89L ***

pop_cap <- pop %>%
  mutate(Age = pmin(Age, AGE_TOP)) %>%
  group_by(Year, Age) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# ---- Cohort deltas on the COLLAPSED series ----
delta_long <- pop_cap %>%
  mutate(Year_prev = Year - 1L, Age_prev = pmax(Age - 1L, 0L)) %>%
  left_join(
    pop_cap %>% transmute(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
    by = c("Year_prev", "Age_prev")
  ) %>%
  transmute(Year, Age, delta = Population - Pop_prev)

# ---- 5-year groups with top bin fixed to 85+ (label it clearly) ----
mk_group5_capped <- function(A) {
  gs <- (A %/% 5) * 5
  gs <- ifelse(gs >= 85, 85, gs)   # force top bin start to 85
  ge <- ifelse(gs == 85, 89, gs + 4)
  sprintf("%02d–%02d", gs, ge)     # this is effectively "85+" when gs==85
}

pop_group_year <- pop_cap %>%
  mutate(grp_start = ifelse(Age >= 85, 85L, (Age %/% 5) * 5L),
         AgeGroup5 = mk_group5_capped(Age)) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

delta_group_year <- delta_long %>%
  mutate(grp_start = ifelse(Age >= 85, 85L, (Age %/% 5) * 5L),
         AgeGroup5 = mk_group5_capped(Age)) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Delta = sum(delta, na.rm = TRUE), .groups = "drop")

# ---- Wide-style table with Δ shown under totals in the same cell ----

# 1) Keep long forms for Year x AgeGroup5 (totals and cohort deltas)
pop_long_5y <- pop_group_year %>% select(AgeGroup5, grp_start, Year, Population)
delta_long_5y <- delta_group_year %>% select(AgeGroup5, grp_start, Year, Delta)

# 2) Build a per-cell HTML with:
#    - Row 1: total population (black)
#    - Row 2: Δ vs prior-year cohort (colored; bg highlight if |Δ| > threshold)
combined_cells <- pop_long_5y %>%
  left_join(delta_long_5y, by = c("AgeGroup5", "grp_start", "Year")) %>%
  mutate(
    pop_txt = ifelse(is.na(Population), "", scales::comma(round(Population))),
    # Style Δ line
    delta_color = case_when(
      is.na(Delta)        ~ "black",
      Delta > 0           ~ "#006400",  # dark green
      TRUE                ~ "#8B0000"   # dark red
    ),
    delta_bg = case_when(
      is.na(Delta)                    ~ "transparent",
      Delta >  HIGHLIGHT_THRESHOLD    ~ "#d8f3dc",  # light green for large positive
      Delta < -HIGHLIGHT_THRESHOLD    ~ "#ffcccc",  # light red for large negative
      TRUE                            ~ "transparent"
    ),
    delta_txt_raw = ifelse(is.na(Delta), "", scales::comma(round(Delta))),
    delta_html = ifelse(
      delta_txt_raw == "",
      "",
      kableExtra::cell_spec(
        delta_txt_raw,
        color = delta_color,
        background = delta_bg,
        escape = FALSE,
        extra_css = "font-size:0.9em;"
      )
    ),
    cell_html = ifelse(
      pop_txt == "",
      "",
      paste0(pop_txt, ifelse(delta_html == "", "", "<br/>"), delta_html)
    )
  ) %>%
  select(AgeGroup5, grp_start, Year, cell_html)

# 3) Pivot to wide: one column per Year, rows ordered youngest -> oldest
out <- combined_cells %>%
  tidyr::pivot_wider(
    id_cols = c(AgeGroup5, grp_start),
    names_from = Year, values_from = cell_html
  ) %>%
  arrange(grp_start) %>%
  select(-grp_start) %>%
  rename(`Age Group (5y)` = AgeGroup5)

# 4) Render (HTML)
kbl(out,
    format  = "html",
    escape  = FALSE,
    caption = paste0(
      "Cohort aging by 5-year groups: totals with Δ below each year. ",
      "Δ(N) sums [Pop(N,A) − Pop(N−1,A−1)] over ages in group. ",
      "Δ colored dark green/red; highlighted when |Δ| > ",
      scales::comma(HIGHLIGHT_THRESHOLD)
    ),
    align   = c("l", rep("r", ncol(out) - 1))
) |>
  kable_styling(full_width = FALSE, position = "center") |>
  row_spec(0, bold = TRUE)
