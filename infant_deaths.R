# Infant mortality rates from monthly deaths & births
# Annual rates per 1,000 births (2010–2025), trend on 2010–2019 + projection 2020–2025
# Monthly rates per 1,000 births, month-specific trend on 2010–2019 + projection 2020–2025

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
})

deaths_file <- "data/infant_monthly_deaths_2010_2025.csv"
births_file <- "data/monthly_births_2007_2025.csv"

out_dir_v <- "visual"
out_dir_d <- "data"
dir.create(out_dir_v, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_d, showWarnings = FALSE, recursive = TRUE)

# ---------- 1) Load & standardize ----------
# Months are written as English names; enforce ordering via month.name
read_deaths <- read_csv(deaths_file, show_col_types = FALSE) %>%
  mutate(
    Year  = as.integer(Year),
    Month = factor(as.character(Month), levels = month.name),
    Deaths = as.numeric(Deaths)
  ) %>%
  filter(!is.na(Year), Year >= 2010, Year <= 2024, !is.na(Month), !is.na(Deaths))
print(read_deaths)

read_births <- read_csv(births_file, show_col_types = FALSE) %>%
  mutate(
    Year  = as.integer(Year),
    Month = factor(as.character(Month), levels = month.name),
    Births = as.numeric(Births)
  ) %>%
  filter(!is.na(Year), Year >= 2010, Year <= 2024, !is.na(Month), !is.na(Births))

# ---------- 2) YEARLY rates (per 1,000 births) ----------
annual_deaths <- read_deaths %>%
  group_by(Year) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            n_months_deaths = n(), .groups = "drop")

annual_births <- read_births %>%
  group_by(Year) %>%
  summarise(Births = sum(Births, na.rm = TRUE),
            n_months_births = n(), .groups = "drop")

annual <- full_join(annual_deaths, annual_births, by = "Year") %>%
  filter(!is.na(Deaths), !is.na(Births)) %>%
  mutate(
    months_used = pmin(n_months_deaths, n_months_births, na.rm = TRUE),
    Rate = 1000 * Deaths / Births
  ) %>%
  arrange(Year)

# Save annual series
write_csv(annual %>% select(Year, Deaths, Births, Rate, months_used),
          file.path(out_dir_d, "infant_annual_rate_per_100k_births.csv"))

# Fit trend on complete pre-2020 years (use only years with 12 months in both series)
train_years <- 2010:2019
annual_train <- annual %>%
  filter(Year %in% train_years, months_used == 12, !is.na(Rate))

if (nrow(annual_train) < 2) stop("Not enough complete years 2010–2019 to fit annual trend.")

annual_model <- lm(Rate ~ Year, data = annual_train)

years_all <- tibble(Year = 2010:2024)
annual_pred <- as.data.frame(
  predict(annual_model, newdata = years_all, interval = "prediction", level = 0.95)
)
annual_pred$Year <- years_all$Year

annual_pred_train <- annual_pred %>% filter(Year %in% train_years)
annual_pred_proj  <- annual_pred %>% filter(Year >= 2020)

# Plot annual
col_actual <- "#222222"
col_trend  <- "#1f77b4"
col_ribbon <- "#1f77b4"

ymax_obs  <- max(annual$Rate, na.rm = TRUE)
ymax_proj <- suppressWarnings(max(annual_pred_proj$upr, na.rm = TRUE))
y_max     <- max(ymax_obs, ymax_proj, na.rm = TRUE)
pad       <- max(0.06 * y_max, 0.2)

p_year <- ggplot(annual, aes(Year, Rate)) +
  geom_ribbon(
    data = annual_pred_proj, aes(ymin = lwr, ymax = upr, x = Year),
    inherit.aes = FALSE, fill = col_ribbon, alpha = 0.12
  ) +
  geom_line(data = annual_pred_train, aes(y = fit), color = col_trend, linewidth = 1.7) +
  geom_line(data = annual_pred_proj,  aes(y = fit), color = col_trend,
            linewidth = 1.7, linetype = "dashed", lineend = "round") +
  geom_line(color = col_actual, linewidth = 1.9) +
  geom_point(color = col_actual, size = 3.3) +
  geom_text(aes(label = number(Rate, accuracy = 0.1)),
            vjust = -1.5, size = 5.4, color = col_actual) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     limits = c(NA, y_max + pad),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Infant deaths per 1,000 births — Annual",
    subtitle = "Observed vs 2010–2019 linear trend and 2020–2024 projection (95% prediction band)\nNote: years with <12 months reflect partial-year data",
    x = NULL, y = "Deaths per 1,000 births"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y   = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir_v, "infant_mortality_annual.png"),
       p_year, width = 12.5, height = 7.2, dpi = 300)
p_year

# ---------- 3) MONTHLY rates (per 1,000 births), month-specific trends ----------
# Join monthly births & deaths; compute monthly rate
monthly <- read_deaths %>%
  inner_join(read_births, by = c("Year","Month")) %>%
  transmute(
    Year, Month,
    Deaths = Deaths,
    Births = Births,
    Rate   = 1000 * Deaths / Births
  ) %>%
  arrange(Year, Month)

# Save monthly series
write_csv(monthly %>% mutate(Month = as.character(Month)),
          file.path(out_dir_d, "infant_monthly_rate_per_100k_births.csv"))

max_year <- max(monthly$Year, na.rm = TRUE)

# Fit per-month linear trends on 2010–2019, then project 2020–2024
monthly_pred <- monthly %>%
  group_by(Month) %>%
  do({
    df <- .
    train <- df %>% filter(Year %in% 2010:2019)
    if (nrow(train) < 2) return(tibble(Year = integer(), fit = numeric(), lwr = numeric(), upr = numeric()))
    m <- lm(Rate ~ Year, data = train)
    yrs <- tibble(Year = 2010:2024)
    pr  <- as.data.frame(predict(m, newdata = yrs, interval = "prediction", level = 0.95))
    tibble(Year = yrs$Year, fit = pr$fit, lwr = pr$lwr, upr = pr$upr)
  }) %>%
  ungroup()

monthly_pred_train <- monthly_pred %>% filter(Year %in% 2010:2019)
monthly_pred_proj  <- monthly_pred %>% filter(Year >= 2020)

# Plot monthly (faceted by month)
ymax_obs_m  <- max(monthly$Rate, na.rm = TRUE)
ymax_proj_m <- suppressWarnings(max(monthly_pred_proj$upr, na.rm = TRUE))
y_max_m     <- max(ymax_obs_m, ymax_proj_m, na.rm = TRUE)
pad_m       <- max(0.06 * y_max_m, 0.2)

p_month <- ggplot(monthly, aes(Year, Rate)) +
  facet_wrap(~ Month, ncol = 4) +
  geom_ribbon(
    data = monthly_pred_proj, aes(ymin = lwr, ymax = upr, x = Year),
    inherit.aes = FALSE, fill = col_ribbon, alpha = 0.10
  ) +
  geom_line(data = monthly_pred_train, aes(y = fit), color = col_trend, linewidth = 1.1) +
  geom_line(data = monthly_pred_proj,  aes(y = fit), color = col_trend,
            linewidth = 1.1, linetype = "dashed") +
  geom_line(color = col_actual, linewidth = 1.1) +
  geom_point(color = col_actual, size = 1.7) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     limits = c(NA, y_max_m + pad_m),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Infant deaths per 1,000 births — Monthly by month-of-year",
    subtitle = "Observed vs 2010–2019 month-specific linear trends and 2020–2024 projections (95% prediction bands)",
    x = NULL, y = "Deaths per 1,000 births"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir_v, "infant_mortality_monthly.png"),
       p_month, width = 13, height = 9, dpi = 300)
p_month

message("Saved:\n - visual/infant_mortality_annual.png\n - visual/infant_mortality_monthly.png\n",
        "Wrote:\n - data/infant_annual_rate_per_100k_births.csv\n - data/infant_monthly_rate_per_100k_births.csv\n")
