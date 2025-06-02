#########################################################
## Age-standardised mortality analysis, United States  ##
## Data file expected at:  "data/merged_deaths.csv"    ##
#########################################################

## ── 1  Load packages ─────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)    # dplyr, ggplot2, readr, tidyr …
  library(lubridate)    # convenient date helpers
  library(broom)        # tidy() / glance() for models
  library(purrr)        # map()-family
})

## ── 2  Read & prepare data ───────────────────────────
mort <- read_csv(
  "data/merged_deaths.csv",
  col_types = cols(
    year          = col_integer(),
    month         = col_character(),   # keep leading “0”
    age_group_5   = col_character(),
    deaths        = col_double(),
    covid_deaths  = col_double(),      # <-- NEW
    population    = col_double()
  )
) %>% 
  mutate(
    month        = as.integer(month),
    date         = as_date(sprintf("%d-%02d-01", year, month)),
    rate         = deaths        / population * 1e5,   # all-cause rate
    rate_covid   = covid_deaths  / population * 1e5    # COVID-only rate
  )

## ── 3  US 2000 standard population weights (5-year) ──
std_pop <- tribble(
  ~age_group_5, ~weight,
  "1",     0.013818,   # <1 year
  "1-4",   0.055317,
  "5-9",   0.072919,
  "10-14", 0.073947,
  "15-19", 0.071759,
  "20-24", 0.066950,
  "25-29", 0.064587,
  "30-34", 0.071142,
  "35-39", 0.080255,
  "40-44", 0.081400,
  "45-49", 0.072105,
  "50-54", 0.064130,
  "55-59", 0.054623,
  "60-64", 0.045284,
  "65-69", 0.035510,
  "70-74", 0.026856,
  "75-79", 0.017303,
  "80-84", 0.010042,
  "85+",   0.008551
)

## ── 4  Age-standardised mortality rate (ASMR) monthly ─
asmr_month <- mort %>% 
  left_join(std_pop, by = "age_group_5") %>% 
  filter(!is.na(weight)) %>%                 # drop groups w/out weight
  group_by(date) %>% 
  summarise(ASMR = sum(rate * weight), .groups = "drop")

## ── 4a  Side-by-side ASMR: all-cause vs COVID ────────
asmr_month_long <- mort %>% 
  left_join(std_pop, by = "age_group_5") %>% 
  filter(!is.na(weight)) %>% 
  group_by(date) %>% 
  summarise(
    ASMR_all   = sum(rate       * weight),
    ASMR_covid = sum(rate_covid * weight),
    .groups    = "drop"
  ) %>% 
  pivot_longer(cols = starts_with("ASMR_"),
               names_to  = "cause",
               values_to = "ASMR") %>% 
  mutate(cause = recode(cause,
                        ASMR_all   = "All causes",
                        ASMR_covid = "COVID-19"))

ggplot(asmr_month_long, aes(date, ASMR, fill = cause)) +
  geom_col(position = position_dodge(width = 25), width = 20) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_fill_manual(values = c("All causes" = "grey30",
                               "COVID-19"   = "steelblue")) +
  labs(title    = "Age-standardised mortality rate (US-2000 standard)",
       subtitle = "United States, monthly — side-by-side bars",
       x = NULL,  y = "ASMR per 100 000", fill = NULL) +
  theme_minimal(base_size = 11)

## ── 5  Linear trend 2015-2019 & excess 2020-2023 ─────
# add numeric time for regression (days since 2015-01-01)
origin_time <- as_date("2015-01-01")
mort <- mort %>% 
  mutate(t_num = as.numeric(date - origin_time))           # days

# function to add trend, predictions & excess to a group
add_trend <- function(df_grp) {
  trend <- lm(rate ~ t_num, data = df_grp %>% filter(year <= 2019))
  df_grp %>% 
    mutate(
      projected = predict(trend, newdata = df_grp),
      excess    = rate - projected
    )
}

mort_excess <- mort %>% 
  group_by(age_group_5) %>% 
  group_modify(~add_trend(.x)) %>% 
  ungroup()

## ── 5a  Excess summary table (optional) ───────────────
excess_summary <- mort_excess %>% 
  filter(year >= 2020, year <= 2023) %>% 
  group_by(age_group_5, year) %>% 
  summarise(
    excess_rate   = sum(excess),                          # ∑ monthly excess rate
    excess_deaths = sum(excess / 1e5 * population),       # convert to deaths
    .groups = "drop"
  )

## ── 6  Actual vs projected mortality – line chart ────
ggplot(mort_excess, aes(date)) +
  geom_line(aes(y = rate,      colour = "Actual"), size = .4) +
  geom_line(aes(y = projected, colour = "Projected"), linetype = "22", size = .4) +
  facet_wrap(~age_group_5, scales = "free_y") +
  labs(
    title   = "Actual vs projected mortality rate by age group",
    y       = "Rate per 100 000",
    colour  = NULL
  ) +
  scale_colour_manual(values = c("Actual" = "black", "Projected" = "red")) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

## ── 7a  Add yearly COVID deaths & difference ─────────
covid_year <- mort %>% 
  filter(year >= 2020 & year <= 2023) %>% 
  group_by(year) %>% 
  summarise(total_covid_deaths = round(sum(covid_deaths, na.rm = TRUE)),
            .groups = "drop")

excess_year <- mort_excess %>% 
  filter(year >= 2020 & year <= 2023) %>%       # or drop the filter if you want 2015-23
  group_by(year) %>% 
  summarise(
    total_excess_deaths = round(sum(excess / 1e5 * population, na.rm = TRUE))
  )


excess_year <- excess_year %>% 
  left_join(covid_year, by = "year") %>% 
  mutate(
    diff_excess_vs_covid = total_excess_deaths - total_covid_deaths
  )

print(excess_year)

