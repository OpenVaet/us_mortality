library(dplyr)
library(stringr)
library(readr)

# Inputs / output
in1 <- "data/Natality, Mothers Age, 2007-2023.csv"
in2 <- "data/Provisional Natality, Mothers Age, 2023 through Last Month.csv"
out <- "data/yearly_births_by_mother_age.csv"

# Robust reader/cleaner for either file
read_clean_age <- function(path, src_label) {
  # read.csv gives "check.names=TRUE" behavior like your base script
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)

  # Column names after check.names=TRUE:
  # "Year Code" -> Year.Code ; "Age of Mother 9" -> Age.of.Mother.9 ; "Age of Mother 9 Code" -> Age.of.Mother.9.Code
  # Prefer the descriptive label; fall back to Code if necessary.
  age_col <- if ("Age.of.Mother.9" %in% names(df)) "Age.of.Mother.9" else "Age.of.Mother.9.Code"

  df %>%
    mutate(
      Year        = suppressWarnings(as.integer(Year.Code)),
      Births      = suppressWarnings(as.integer(Births)),
      MotherAgeGroup = .data[[age_col]] %>%
        as.character() %>%
        str_replace("\\s*years$", "") %>%     # "15-19 years" -> "15-19"
        str_replace("^50\\s*years.*$", "50+") %>%  # "50 years and over" -> "50+"
        str_replace("^Under\\s*15.*$", "Under 15") %>%  # "Under 15 years" -> "Under 15"
        str_replace_all("[–—]", "-") %>%       # normalize en/em dashes to hyphen
        str_squish()
    ) %>%
    # Drop "Total" rows
    filter(is.na(Notes) | Notes != "Total") %>%
    select(Year, MotherAgeGroup, Births) %>%
    mutate(.source = src_label)
}

d1 <- read_clean_age(in1, "2010-2023 main")
d2 <- read_clean_age(in2, "2018-2023 provisional")

# Prefer the newer provisional/final file where years overlap (2018–2023)
priority <- c("2010-2023 main" = 1L, "2018-2023 provisional" = 2L)

result <- bind_rows(d1, d2) %>%
  filter(!is.na(Year), Year >= 2010, Year <= 2025) %>%
  mutate(.prio = priority[.source]) %>%
  arrange(Year, MotherAgeGroup, desc(.prio)) %>%
  distinct(Year, MotherAgeGroup, .keep_all = TRUE) %>%
  select(Year, MotherAgeGroup, Births) %>%
  arrange(Year, MotherAgeGroup)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write.csv(result, out, row.names = FALSE)

cat("Wrote:", out, "rows:", nrow(result), "\n")

# ---------- Assessment: share of births by mother age group over ALL years ----------
# Order groups logically: Under 15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
order_key <- function(x) dplyr::case_when(
  x == "Under 15" ~ 0L,
  str_detect(x, "^[0-9]+-") ~ suppressWarnings(as.integer(str_extract(x, "^[0-9]+"))),
  x == "50+" ~ 50L,
  TRUE ~ 999L
)

shares <- result %>%
  group_by(MotherAgeGroup) %>%
  summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop") %>%
  mutate(Share = 100 * Births / sum(Births)) %>%
  arrange(order_key(MotherAgeGroup)) %>%
  mutate(ShareLabel = scales::percent(Share / 100, accuracy = 0.1))

# Print a neat summary to the console
cat("\nShare of births by mother age group (all years combined):\n")
print(shares %>% select(MotherAgeGroup, Births, ShareLabel), row.names = FALSE)

# Also write a CSV with numeric Share (%)
write.csv(shares %>% select(MotherAgeGroup, Births, Share), 
          "data/yearly_births_by_mother_age_shares.csv", row.names = FALSE)
cat("Wrote: data/yearly_births_by_mother_age_shares.csv\n")
