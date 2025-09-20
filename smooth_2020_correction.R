# Smooth 2020 correction across 2011–2019 for ages <= 49
# with ages 85+ collapsed to Age=85 before all steps
# Input : data/merged_yearly_population_by_age_2010_correction.csv
# Output: data/merged_yearly_population_by_age_2020_smoothed.csv

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

IN_PATH  <- "data/merged_yearly_population_by_age_2010_correction.csv"
OUT_PATH <- "data/merged_yearly_population_by_age_2020_smoothed.csv"

AGE_MAX_SMOOTH <- 49L
AGE_CAP        <- 85L        # collapse all ages >=85 to 85
YEAR_START     <- 2011L
YEAR_TARGET    <- 2020L
YEAR_END_ADJ   <- YEAR_TARGET - 1L   # 2019
DENOM          <- YEAR_TARGET - 2010L  # 10 (steps 2010->2020)

# --- Load & CAP at 85+ first ---
pop_raw <- read_csv(IN_PATH, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

# Collapse ages >=85 into Age=85
pop_cap <- pop_raw %>%
  mutate(Age = pmin(Age, AGE_CAP)) %>%
  group_by(Year, Age) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# --- Compute the 2019→2020 same-age jump for ages <= AGE_MAX_SMOOTH (on capped data) ---
delta_by_age <- pop_cap %>%
  filter(Age <= AGE_MAX_SMOOTH, Year %in% c(YEAR_END_ADJ, YEAR_TARGET)) %>%
  pivot_wider(id_cols = Age, names_from = Year, values_from = Population) %>%
  mutate(delta2020 = .data[[as.character(YEAR_TARGET)]] -
                     .data[[as.character(YEAR_END_ADJ)]]) %>%
  select(Age, delta2020)

# --- Apply cumulative offset for 2011..2019; keep 2020 unchanged ---
pop_smoothed <- pop_cap %>%
  left_join(delta_by_age, by = "Age") %>%
  mutate(
    # Cumulative share of the 2019→2020 jump to apply by year y
    # (0 for <=2010 or >=2020; linear ramp 0.1..0.9 for 2011..2019)
    cum_share = case_when(
      Age <= AGE_MAX_SMOOTH & Year >= YEAR_START & Year <= YEAR_END_ADJ ~
        (Year - 2010) / DENOM,
      TRUE ~ 0
    ),
    adj = coalesce(delta2020, 0) * cum_share,
    Population = Population + adj
  ) %>%
  arrange(Year, Age)

# --- Sanity checks (optional) ---
# 1) 2020 values unchanged (on the capped series)
chk_2020 <- identical(
  pop_cap %>% filter(Year == YEAR_TARGET) %>% arrange(Age) %>% pull(Population),
  pop_smoothed %>% filter(Year == YEAR_TARGET) %>% arrange(Age) %>% pull(Population)
)
if (!isTRUE(chk_2020)) warning("2020 values changed unexpectedly (after capping).")

# --- Write output ---
dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
write_csv(pop_smoothed, OUT_PATH)
cat("Wrote: ", OUT_PATH, " (rows: ", nrow(pop_smoothed), ")\n", sep = "")
