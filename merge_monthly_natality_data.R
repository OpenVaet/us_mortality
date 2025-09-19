library(dplyr)

read_clean <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
  # check.names=TRUE converts "Year Code" -> Year.Code, "Month Code" -> Month.Code
  df %>%
    mutate(
      Year = suppressWarnings(as.integer(Year.Code)),
      Births = suppressWarnings(as.integer(Births)),
      Month = as.character(Month)
    ) %>%
    # Drop "Total" rows; Notes is empty/NA for monthly rows
    filter(is.na(Notes) | Notes != "Total") %>%
    select(Year, Month, Births)
}

# Read and combine
combined <- bind_rows(
  read_clean("data/Natality, 2007-2023.csv"),
  read_clean("data/Provisional Natality, 2023 through Last Month.csv")
)

# Keep only 2007–2025, remove any NAs, dedupe, and sort
result <- combined %>%
  filter(!is.na(Year), Year >= 2007, Year <= 2025) %>%
  distinct(Year, Month, .keep_all = TRUE) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  arrange(Year, Month) %>%
  mutate(Month = as.character(Month))

# Write final file
write.csv(result, "data/monthly_births_2007_2025.csv", row.names = FALSE)
