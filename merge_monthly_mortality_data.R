library(dplyr)

read_clean <- function(path, source_label) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
  # check.names=TRUE -> "Year Code" becomes Year.Code; "Month Code" becomes Month.Code
  df %>%
    # Remove total rows (Notes == "Total"); monthly rows have empty/NA Notes
    filter(is.na(Notes) | Notes != "Total") %>%
    mutate(
      Year = suppressWarnings(as.integer(Year.Code)),
      month_code = as.character(Month.Code),
      month_num = suppressWarnings(as.integer(sub(".*/", "", month_code))), # e.g., "2021/03" -> "03" -> 3
      Month = month.name[month_num],
      Deaths = suppressWarnings(as.integer(gsub(",", "", Deaths))),
      source = source_label
    ) %>%
    select(Year, Month, month_num, Deaths, source)
}

final_df <- read_clean("data/Multiple Cause of Death, 1999-2020.csv", "final") %>%
  bind_rows(read_clean("data/Provisional Mortality Statistics, 2018 through Last Week.csv", "provisional")) %>%
  # Keep only the target years
  filter(!is.na(Year), Year >= 2007, Year <= 2025, !is.na(month_num), month_num >= 1, month_num <= 12) %>%
  # Prefer provisional where both sources provide the same Year/Month
  mutate(source_priority = ifelse(source == "provisional", 1L, 2L)) %>%
  arrange(Year, month_num, source_priority) %>%
  distinct(Year, month_num, .keep_all = TRUE) %>%
  arrange(Year, month_num) %>%
  select(Year, Month, Deaths)

# Write result
write.csv(final_df, "data/monthly_deaths_2007_2025.csv", row.names = FALSE)
