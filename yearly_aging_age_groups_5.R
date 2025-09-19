###############################################################
## Cohort aging by 5-year age groups (keeps Age 0)
## Δ at year N for a group = sum_A_in_group [ Pop(N,A) - Pop(N-1,A-1) ]
## Source: data/merged_yearly_population_by_age.csv
###############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

# ---- Params ----
HIGHLIGHT_THRESHOLD <- 250000L  # highlight when |Δ| > 50,000

# ---- Load ----
pop <- read_csv("data/merged_yearly_population_by_age.csv", show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

yrs     <- sort(unique(pop$Year))
max_age <- max(pop$Age, na.rm = TRUE)

# ---- Cohort deltas at single ages: Δ(N,A) = Pop(N,A) - Pop(N-1,A-1) ----
delta_long <- pop %>%
  mutate(Year_prev = Year - 1, Age_prev = Age - 1) %>%
  left_join(pop %>% select(Year, Age, Population) %>%
              rename(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
            by = c("Year_prev", "Age_prev")) %>%
  transmute(Year, Age, delta = Population - Pop_prev)  # NA for first year or Age==0

# ---- Build 5-year age groups (00–04, 05–09, …) ----
mk_group5 <- function(A) {
  gs <- (A %/% 5) * 5
  ge <- pmin(gs + 4, max_age)
  sprintf("%02d–%02d", gs, ge)
}

# Grouped population by Year x AgeGroup5
pop_group_year <- pop %>%
  mutate(AgeGroup5 = mk_group5(Age),
         grp_start = (Age %/% 5) * 5) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# Grouped cohort deltas by Year x AgeGroup5 (sum single-age deltas in the group)
delta_group_year <- delta_long %>%
  mutate(AgeGroup5 = mk_group5(Age),
         grp_start  = (Age %/% 5) * 5) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Delta = sum(delta, na.rm = TRUE), .groups = "drop")

# ---- Wide tables: years as columns, plus Δ columns (Y - (Y-1) in cohort sense) ----
pop_wide <- pop_group_year %>%
  pivot_wider(id_cols = c(AgeGroup5, grp_start),
              names_from = Year, values_from = Population)

delta_wide <- delta_group_year %>%
  mutate(delta_col = paste0(Year, "-", Year - 1)) %>%
  pivot_wider(id_cols = c(AgeGroup5, grp_start),
              names_from = delta_col, values_from = Delta)

# Combine and order rows (youngest -> oldest)
out <- pop_wide %>%
  left_join(delta_wide, by = c("AgeGroup5", "grp_start")) %>%
  arrange(grp_start) %>%
  select(-grp_start)

# ---- Column order: AgeGroup5, Y1, (Y2, Y2-Y1), (Y3, Y3-Y2), … ----
col_order <- c("AgeGroup5", as.character(yrs[1]))
if (length(yrs) >= 2) {
  for (i in 2:length(yrs)) {
    y <- yrs[i]
    col_order <- c(col_order, as.character(y), paste0(y, "-", y - 1))
  }
}
out <- out[, intersect(col_order, names(out))]

# ---- Format & highlight (HTML) ----
year_cols  <- grep("^\\d{4}$", names(out), value = TRUE)
delta_cols <- grep("^\\d{4}-\\d{4}$", names(out), value = TRUE)

out_fmt <- out %>%
  mutate(across(all_of(year_cols),
                ~ ifelse(is.na(.), "", scales::comma(round(.))))) %>%
  mutate(across(all_of(delta_cols), \(x) {
    vals <- ifelse(is.na(x), "", scales::comma(round(x)))
    bg   <- ifelse(is.na(x), "transparent",
                   ifelse(abs(x) > HIGHLIGHT_THRESHOLD, "#ffcccc", "transparent"))
    kableExtra::cell_spec(vals, color = "black", background = bg)
  })) %>%
  rename(`Age Group (5y)` = AgeGroup5)

# ---- Render HTML table ----
kbl(out_fmt,
    format  = "html",
    escape  = FALSE,
    caption = "Cohort aging by 5-year groups: Δ(N) sums [Pop(N,A)−Pop(N−1,A−1)] over ages in group; highlighted when |Δ| > 50,000",
    align   = c("l", rep("r", ncol(out_fmt) - 1))) |>
  kable_styling(full_width = FALSE, position = "center") |>
  row_spec(0, bold = TRUE)
