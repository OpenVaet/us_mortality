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
  "5-9",   0.072533,
  "10-14", 0.073032,
  "15-19", 0.072169,
  "20-24", 0.066478,
  "25-29", 0.064529,
  "30-34", 0.071044,
  "35-39", 0.080762,
  "40-44", 0.081851,
  "45-49", 0.072118,
  "50-54", 0.062716,
  "55-59", 0.048454,
  "60-64", 0.038793,
  "65-69", 0.034264,
  "70-74", 0.031773,
  "75-79", 0.026999,
  "80-84", 0.017842,
  "85+",   0.015508
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


#########################################################
## 7 b  All-cause ASMR with ESP2013 reference (YEARLY) ##
#########################################################
## ── 7b  All-cause ASMR with ESP2013 reference ─────────────────────────

# ESP2013 weights (19 × 5-year bands; sum = 1.0)
std_pop_esp2013 <- tribble(
  ~age_group_5, ~weight,
  "1",     0.010,   # <1 y   (1 000 / 100 000)
  "1-4",   0.040,   # 1-4 y (4 000)
  "5-9",   0.055,
  "10-14", 0.055,
  "15-19", 0.055,
  "20-24", 0.060,
  "25-29", 0.060,
  "30-34", 0.065,
  "35-39", 0.070,
  "40-44", 0.070,
  "45-49", 0.070,
  "50-54", 0.070,
  "55-59", 0.065,
  "60-64", 0.060,
  "65-69", 0.055,
  "70-74", 0.050,
  "75-79", 0.040,
  "80-84", 0.025,
  "85+",   0.025    # 85-89 + 90 + pooled (1 500 + 1 000)
)


## ── 7 b-1  Collapse “mort” to YEARLY totals ──────────
mort_year <- mort %>%                                     # <— uses the monthly file
  group_by(year, age_group_5) %>% 
  summarise(
    deaths     = sum(deaths,     na.rm = TRUE),           # yearly deaths
    population = mean(population, na.rm = TRUE),          # mid-year proxy
    .groups    = "drop"
  ) %>% 
  mutate(
    rate = deaths / population * 1e5,                     # refresh rate
    date = as_date(sprintf("%d-07-01", year))             # mid-year point for plotting
  )
print(mort_year, n=100)

## ── 7 b-2  Re-compute YEARLY ASMR (ESP2013 standard) ──
asmr_esp <- mort_year %>% 
  left_join(std_pop_esp2013, by = "age_group_5") %>% 
  filter(!is.na(weight)) %>% 
  group_by(year, date) %>% 
  summarise(ASMR = sum(rate * weight), .groups = "drop") %>% 
  mutate(t_num = year - 2015)                             # numeric time for regression
print(asmr_esp)

## ── 7 b-3  Fit linear trend 2015-2019 and project ─────
trend_esp <- lm(ASMR ~ t_num, data = filter(asmr_esp, year <= 2019))
asmr_esp  <- asmr_esp %>% 
  mutate(projected = predict(trend_esp, newdata = asmr_esp))

## ── 7 b-4  Labels: % excess (Actual vs Projected) ─────
label_df <- asmr_esp %>% 
  filter(year >= 2020 & year <= 2023) %>% 
  mutate(pct_excess = (ASMR - projected) / projected)     # in proportion

## ── 7 b-5  Plot ───────────────────────────────────────
ggplot(asmr_esp, aes(date)) +
  geom_line(aes(y = ASMR,      colour = "Actual"),    size = .6) +
  geom_line(aes(y = projected, colour = "Projected"), linetype = "22", size = .6) +
  geom_text(data = label_df,
            aes(date, ASMR,
                label = scales::label_percent(accuracy = 0.1)(pct_excess)),
            vjust = -0.8, size = 3) +
  scale_colour_manual(values = c("Actual" = "black", "Projected" = "red")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "All-cause age-standardised mortality rate (ESP2013)",
    subtitle = "Yearly totals; labels show % excess vs projected (2015-19 trend)",
    y        = "ASMR per 100 000 (ESP2013)",
    x        = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

#########################################################
## 7 b-6  Yearly death totals table  (knitr + kableExtra)
#########################################################

suppressPackageStartupMessages(library(kableExtra))  # <- new

# Build wide table: one column per 5-year band + Total
death_table <- mort_year %>%                         # object from §7 b-1
  select(year, age_group_5, deaths) %>% 
  pivot_wider(names_from  = age_group_5,
              values_from = deaths,
              values_fill = 0) %>% 
  mutate(Total = rowSums(across(-year))) %>% 
  arrange(year)

# Helper: span header “Deaths” across all age-group columns + Total
age_cols <- setdiff(names(death_table), c("year", "Total"))
header_vec <- c(" " = 1, "Deaths" = length(age_cols) + 1)

# Render with knitr + kableExtra
death_table %>% 
  knitr::kable(
    caption     = "Yearly deaths by 5-year age group and total",
    format.args = list(big.mark = ","),
    booktabs    = TRUE,            # nicer horizontal rules (pdf/HTML)
    align       = "r"
  ) %>% 
  kable_styling(
    full_width   = FALSE,
    position     = "center",
    latex_options = c("hold_position"),  # keeps table near code in PDF
    stripe_color = "rgba(0,0,0,0.04)"    # light zebra striping (HTML)
  ) %>% 
  add_header_above(header_vec)            # spanning header

