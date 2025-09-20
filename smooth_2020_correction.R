###############################################################################
## Smooth 2011-2019 Population to Match 2020 Census Corrections
## 
## This script adjusts 2011-2019 population data so that cohorts age smoothly
## into the corrected 2020 census values. The 2020 values remain unchanged as
## they represent the corrected "truth" from the census revision.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

# --- Parameters ---
MAX_AGE_TO_SMOOTH <- 59L  # Only smooth ages 0-59
START_YEAR <- 2011L        # Start of adjustment period
END_YEAR <- 2019L          # End of adjustment period (2020 stays fixed)
CENSUS_YEAR <- 2020L       # The corrected census year (remains unchanged)

# --- Load original data ---
pop_raw <- read_csv("data/merged_yearly_population_by_age_2010_correction.csv",
                    show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Age = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

# --- Step 1: Calculate expected vs actual deltas for cohorts entering 2020 ---

# For each age in 2020 (up to MAX_AGE_TO_SMOOTH), calculate the discontinuity
cohort_analysis <- tibble()

for (age_2020 in 1:MAX_AGE_TO_SMOOTH) {
  birth_year <- CENSUS_YEAR - age_2020
  
  # Get the population for this cohort in 2019 and 2020
  pop_2019 <- pop_raw %>% 
    filter(Year == 2019, Age == age_2020 - 1) %>% 
    pull(Population)
  
  pop_2020 <- pop_raw %>% 
    filter(Year == 2020, Age == age_2020) %>% 
    pull(Population)
  
  if (length(pop_2019) > 0 && length(pop_2020) > 0) {
    actual_delta_2020 <- pop_2020 - pop_2019
    
    # Calculate historical average delta for this age transition
    # Look at the same age transition in previous years
    historical_deltas <- c()
    for (ref_year in 2015:2018) {
      pop_prev <- pop_raw %>% 
        filter(Year == ref_year, Age == age_2020 - 1) %>% 
        pull(Population)
      pop_curr <- pop_raw %>% 
        filter(Year == ref_year + 1, Age == age_2020) %>% 
        pull(Population)
      
      if (length(pop_prev) > 0 && length(pop_curr) > 0) {
        historical_deltas <- c(historical_deltas, pop_curr - pop_prev)
      }
    }
    
    # Expected delta is the median of historical deltas
    expected_delta <- if(length(historical_deltas) > 0) {
      median(historical_deltas)
    } else {
      # Fallback: assume small natural decrease
      -pop_2019 * 0.001  
    }
    
    excess_jump <- actual_delta_2020 - expected_delta
    
    cohort_analysis <- bind_rows(
      cohort_analysis,
      tibble(
        Birth_Year = birth_year,
        Age_2020 = age_2020,
        Pop_2019 = pop_2019,
        Pop_2020 = pop_2020,
        Actual_Delta = actual_delta_2020,
        Expected_Delta = expected_delta,
        Excess_Jump = excess_jump
      )
    )
  }
}

cat("\n=== Largest Discontinuities (2019 → 2020) ===\n")
cohort_analysis %>%
  arrange(desc(abs(Excess_Jump))) %>%
  head(15) %>%
  mutate(across(c(Pop_2019, Pop_2020, Actual_Delta, Expected_Delta, Excess_Jump), 
                ~scales::comma(round(.)))) %>%
  print()

# --- Step 2: Build cohort trajectories and apply adjustments ---

# Start with a copy of the original data
pop_smoothed <- pop_raw

# For each cohort with significant excess jump, adjust its entire history
for (i in 1:nrow(cohort_analysis)) {
  birth_year <- cohort_analysis$Birth_Year[i]
  excess <- cohort_analysis$Excess_Jump[i]
  age_2020 <- cohort_analysis$Age_2020[i]
  
  # Skip small adjustments
  if (abs(excess) < 1000) next
  
  # We need to adjust this cohort's population in years 2011-2019
  # The key insight: we want the CHANGE from 2019 to 2020 to be reasonable,
  # so we need to adjust the 2019 population (and earlier years accordingly)
  
  # Target population for 2019 that would give us the expected delta
  target_pop_2019 <- cohort_analysis$Pop_2020[i] - cohort_analysis$Expected_Delta[i]
  actual_pop_2019 <- cohort_analysis$Pop_2019[i]
  total_adjustment_needed <- target_pop_2019 - actual_pop_2019
  
  # Now we need to phase in this adjustment gradually from 2011 to 2019
  # We'll apply it progressively: small amounts early, larger amounts later
  
  adjustment_years <- c()
  adjustment_ages <- c()
  adjustment_factors <- c()
  
  for (year in START_YEAR:END_YEAR) {
    age_in_year <- year - birth_year
    if (age_in_year >= 0 && age_in_year <= MAX_AGE_TO_SMOOTH) {
      adjustment_years <- c(adjustment_years, year)
      adjustment_ages <- c(adjustment_ages, age_in_year)
      # Linear progression: each year gets more adjustment
      year_weight <- (year - START_YEAR + 1) / (END_YEAR - START_YEAR + 1)
      adjustment_factors <- c(adjustment_factors, year_weight)
    }
  }
  
  if (length(adjustment_years) > 0) {
    # Apply adjustments
    for (j in 1:length(adjustment_years)) {
      year <- adjustment_years[j]
      age <- adjustment_ages[j]
      
      # This year gets this fraction of the total adjustment
      adjustment <- total_adjustment_needed * adjustment_factors[j]
      
      idx <- which(pop_smoothed$Year == year & pop_smoothed$Age == age)
      if (length(idx) > 0) {
        pop_smoothed$Population[idx] <- pop_smoothed$Population[idx] + adjustment
      }
    }
  }
}

# --- Step 3: Validate the smoothing by checking cohort continuity ---
cat("\n=== Validation: Cohort Continuity Check ===\n")

# Function to track a cohort through time
track_cohort <- function(data, birth_year, start_year = 2010, end_year = 2021) {
  cohort_data <- tibble()
  for (year in start_year:end_year) {
    age <- year - birth_year
    if(age < 0 || age > 100) next
    
    pop_val <- data %>%
      filter(Year == year, Age == age) %>%
      pull(Population)
    
    if (length(pop_val) > 0) {
      cohort_data <- bind_rows(cohort_data, 
                                tibble(Year = year, Age = age, Population = pop_val))
    }
  }
  
  cohort_data %>%
    arrange(Year) %>%
    mutate(
      Delta = Population - lag(Population),
      Delta_Pct = Delta / lag(Population) * 100
    )
}

# Check the cohorts with the largest original discontinuities
check_cohorts <- cohort_analysis %>%
  arrange(desc(abs(Excess_Jump))) %>%
  head(5) %>%
  pull(Birth_Year)

for (birth_yr in check_cohorts) {
  age_2020 <- 2020 - birth_yr
  
  cat(sprintf("\n--- Cohort born %d (age %d in 2020) ---\n", birth_yr, age_2020))
  
  # Compare original and smoothed
  orig_cohort <- track_cohort(pop_raw, birth_yr, start_year = 2016, end_year = 2021)
  smooth_cohort <- track_cohort(pop_smoothed, birth_yr, start_year = 2016, end_year = 2021)
  
  comparison <- orig_cohort %>%
    select(Year, Age, Delta_Orig = Delta) %>%
    left_join(
      smooth_cohort %>% select(Year, Age, Delta_Smooth = Delta),
      by = c("Year", "Age")
    ) %>%
    mutate(
      Delta_Orig = round(Delta_Orig),
      Delta_Smooth = round(Delta_Smooth),
      Improvement = Delta_Smooth - Delta_Orig
    )
  
  print(comparison %>% filter(Year >= 2018))
}

# --- Step 4: Check that 2020 values remain unchanged ---
cat("\n=== Verification: 2020 Values Unchanged ===\n")

check_2020 <- pop_raw %>%
  filter(Year == 2020, Age <= MAX_AGE_TO_SMOOTH) %>%
  select(Age, Pop_Original = Population) %>%
  left_join(
    pop_smoothed %>%
      filter(Year == 2020, Age <= MAX_AGE_TO_SMOOTH) %>%
      select(Age, Pop_Smoothed = Population),
    by = "Age"
  ) %>%
  mutate(
    Difference = abs(Pop_Smoothed - Pop_Original),
    Match = Difference < 0.01
  )

if(all(check_2020$Match)) {
  cat("✓ All 2020 values remain unchanged (as expected)\n")
} else {
  cat("⚠ Warning: Some 2020 values changed (this shouldn't happen):\n")
  print(check_2020 %>% filter(!Match))
}

# --- Step 5: Summary of improvements ---
cat("\n=== Summary: Improvement in 2019→2020 Transitions ===\n")

# Calculate the deltas for all cohorts in both original and smoothed data
all_deltas_comparison <- tibble()

for (age_2020 in 1:MAX_AGE_TO_SMOOTH) {
  birth_year <- CENSUS_YEAR - age_2020
  
  # Original delta
  orig_cohort <- track_cohort(pop_raw, birth_year)
  orig_delta_2020 <- orig_cohort %>% 
    filter(Year == 2020) %>% 
    pull(Delta)
  
  # Smoothed delta
  smooth_cohort <- track_cohort(pop_smoothed, birth_year)
  smooth_delta_2020 <- smooth_cohort %>% 
    filter(Year == 2020) %>% 
    pull(Delta)
  
  if (length(orig_delta_2020) > 0 && length(smooth_delta_2020) > 0) {
    all_deltas_comparison <- bind_rows(
      all_deltas_comparison,
      tibble(
        Age_2020 = age_2020,
        Delta_Original = orig_delta_2020,
        Delta_Smoothed = smooth_delta_2020
      )
    )
  }
}

cat("\nLargest improvements in 2019→2020 deltas:\n")
all_deltas_comparison %>%
  mutate(Improvement = abs(Delta_Smoothed) - abs(Delta_Original)) %>%
  arrange(Improvement) %>%
  head(10) %>%
  mutate(across(c(Delta_Original, Delta_Smoothed), ~scales::comma(round(.)))) %>%
  print()

cat(sprintf("\nStandard deviation of 2019→2020 deltas:\n"))
cat(sprintf("  Original:  %s\n", 
            scales::comma(round(sd(all_deltas_comparison$Delta_Original, na.rm = TRUE)))))
cat(sprintf("  Smoothed:  %s\n", 
            scales::comma(round(sd(all_deltas_comparison$Delta_Smoothed, na.rm = TRUE)))))
cat(sprintf("  Reduction: %.1f%%\n", 
            (1 - sd(all_deltas_comparison$Delta_Smoothed, na.rm = TRUE) / 
               sd(all_deltas_comparison$Delta_Original, na.rm = TRUE)) * 100))

# --- Step 6: Save the smoothed data ---
output_file <- "data/merged_yearly_population_by_age_2020_smoothed.csv"
pop_smoothed %>%
  arrange(Year, Age) %>%
  write_csv(output_file)

cat(sprintf("\n=== Smoothed data saved to %s ===\n", output_file))

# --- Step 7: Create visualization ---
cat("\n=== Creating visualization ===\n")

library(ggplot2)

# Select cohorts with largest corrections for visualization
viz_cohorts <- cohort_analysis %>%
  arrange(desc(abs(Excess_Jump))) %>%
  head(4) %>%
  pull(Birth_Year)

viz_data <- tibble()

for(birth_yr in viz_cohorts) {
  orig <- track_cohort(pop_raw, birth_yr, start_year = 2010, end_year = 2021)
  smooth <- track_cohort(pop_smoothed, birth_yr, start_year = 2010, end_year = 2021)
  
  age_2020 <- 2020 - birth_yr
  
  viz_data <- bind_rows(
    viz_data,
    orig %>% mutate(Type = "Original", Cohort = paste0("Born ", birth_yr, " (age ", age_2020, " in 2020)")),
    smooth %>% mutate(Type = "Smoothed", Cohort = paste0("Born ", birth_yr, " (age ", age_2020, " in 2020)"))
  )
}

# Plot the deltas (year-to-year changes)
p1 <- ggplot(viz_data %>% filter(!is.na(Delta), Year >= 2015), 
             aes(x = Year, y = Delta, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  facet_wrap(~ Cohort, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Original" = "#e74c3c", "Smoothed" = "#27ae60")) +
  scale_linetype_manual(values = c("Original" = "dashed", "Smoothed" = "solid")) +
  labs(
    title = "Cohort Year-to-Year Changes: Original vs Smoothed",
    subtitle = "Smoothing reduces the 2019→2020 jump by adjusting 2011-2019 populations",
    x = "Year",
    y = "Annual Change in Cohort Size",
    color = "Data Type",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave("visual/cohort_deltas_comparison.png", p1, width = 12, height = 8, dpi = 150)
cat("Visualization saved to visual/cohort_deltas_comparison.png\n")

# --- Create a heatmap showing the adjustments made ---
cat("\n=== Creating adjustment heatmap ===\n")

# Calculate adjustments for all year-age combinations
adjustments <- pop_raw %>%
  filter(Year >= START_YEAR, Year <= END_YEAR, Age <= MAX_AGE_TO_SMOOTH) %>%
  select(Year, Age, Pop_Original = Population) %>%
  left_join(
    pop_smoothed %>%
      filter(Year >= START_YEAR, Year <= END_YEAR) %>%
      select(Year, Age, Pop_Smoothed = Population),
    by = c("Year", "Age")
  ) %>%
  mutate(
    Adjustment = Pop_Smoothed - Pop_Original,
    Adj_Pct = Adjustment / Pop_Original * 100
  )

p2 <- ggplot(adjustments %>% filter(abs(Adjustment) > 100), 
             aes(x = Year, y = Age, fill = Adjustment)) +
  geom_tile() +
  scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#27ae60",
                       midpoint = 0,
                       labels = scales::comma,
                       name = "Adjustment") +
  scale_x_continuous(breaks = START_YEAR:END_YEAR) +
  scale_y_continuous(breaks = seq(0, MAX_AGE_TO_SMOOTH, by = 5)) +
  labs(
    title = "Population Adjustments Applied (2011-2019)",
    subtitle = "Green = population increased, Red = population decreased, to smooth flow into 2020",
    x = "Year",
    y = "Age"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

ggsave("visual/adjustment_heatmap.png", p2, width = 10, height = 8, dpi = 150)
cat("Heatmap saved to visual/adjustment_heatmap.png\n")

cat("\n=== Processing complete ===\n")
cat("The 2020 census values remain unchanged while 2011-2019 have been adjusted\n")
cat("to ensure smooth cohort progression into the corrected 2020 values.\n")