library(dplyr)
library(readr)
library(scales)

# Files
births_m <- read_csv("data/monthly_births_2007_2025.csv", show_col_types = FALSE)
deaths_m <- read_csv("data/monthly_deaths_2007_2025.csv", show_col_types = FALSE)

# Months of interest and years range
months_q <- c("September", "October", "November", "December")
yr_min <- 2007; yr_max <- 2023

# Aggregate Sep–Dec totals by year
births_q <- births_m %>%
  filter(Year >= yr_min, Year <= yr_max, Month %in% months_q) %>%
  group_by(Year) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")

deaths_q <- deaths_m %>%
  filter(Year >= yr_min, Year <= yr_max, Month %in% months_q) %>%
  group_by(Year) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop")

# Combine, compare, and label per-year verdicts
by_year <- births_q %>%
  full_join(deaths_q, by = "Year") %>%
  mutate(
    Births = replace_na(Births, 0L),
    Deaths = replace_na(Deaths, 0L),
    Difference = Births - Deaths,
    Verdict = case_when(
      Difference > 0 ~ "Births > Deaths",
      Difference < 0 ~ "Deaths > Births",
      TRUE ~ "Tie"
    )
  ) %>%
  arrange(Year)

# Overall totals across 2007–2023 (Sep–Dec only)
overall <- by_year %>%
  summarise(
    Years = n(),
    Total_Births = sum(Births),
    Total_Deaths = sum(Deaths),
    Difference = Total_Births - Total_Deaths,
    Ratio = ifelse(Total_Deaths == 0, NA_real_, Total_Births / Total_Deaths)
  )

# Counts of years where one exceeds the other
counts <- by_year %>%
  summarise(
    Births_gt_Deaths_years = sum(Difference > 0),
    Deaths_gt_Births_years = sum(Difference < 0),
    Ties = sum(Difference == 0)
  )

# Print results
print(by_year)
print(overall)
print(counts)

# Clear textual verdict
if (overall$Difference[1] > 0) {
  cat("Over", yr_min, "to", yr_max, "(Sep–Dec), MORE BIRTHS than DEATHS by",
      comma(overall$Difference[1]), "people.\n")
} else if (overall$Difference[1] < 0) {
  cat("Over", yr_min, "to", yr_max, "(Sep–Dec), MORE DEATHS than BIRTHS by",
      comma(abs(overall$Difference[1])), "people.\n")
} else {
  cat("Over", yr_min, "to", yr_max, "(Sep–Dec), BIRTHS and DEATHS are equal overall.\n")
}

# Save per-year summary
write_csv(by_year, "data/sep_dec_births_vs_deaths_by_year_2007_2023.csv")
