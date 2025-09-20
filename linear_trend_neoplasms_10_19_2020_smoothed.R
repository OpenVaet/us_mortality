# Recompute 5-year-group populations (10-14, 15-19) from smoothed single-age population
# and refresh crude rates in the Neoplasms file.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

pop_file   <- "data/merged_yearly_population_by_age_2020_smoothed.csv"
death_file <- "data/C00-D48 - 10-19 - 2010-to-2023.csv"
out_file   <- "data/C00-D48 - 10-19 - 2010-to-2023_smoothedpop.csv"

# --- 1) Load smoothed single-age population and aggregate to 10-year groups ---
pop_smoothed <- read_csv(pop_file, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

pop10 <- pop_smoothed %>%
  filter(Age >= 10, Age <= 19) %>%
  mutate(
    AgeGroup5 = case_when(
      Age <= 14 ~ "10-14",
      Age <= 19 ~ "15-19"
    )
  ) %>% 
  group_by(Year, AgeGroup5) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# --- 2) Load deaths file robustly (supports either AgeGroup5 or "Ten-Year Age Groups") ---
df_raw <- read_csv(death_file, col_types = cols(.default = col_character()))

# Choose year and age-group columns flexibly
year_vec <- if ("Year" %in% names(df_raw)) df_raw$Year else df_raw$`Year Code`
age_vec  <- if ("AgeGroup5" %in% names(df_raw)) df_raw$AgeGroup5 else df_raw$`Five-Year Age Groups`

death <- tibble(
  Year       = suppressWarnings(as.integer(readr::parse_number(year_vec))),
  AgeGroup5 = age_vec %>%
    str_replace("\\s*years$", "") %>%   # "10-14 years" -> "10-14"
    str_replace_all("[–—]", "-") %>%    # en/em dash -> hyphen
    str_trim(),
  Deaths     = suppressWarnings(as.numeric(readr::parse_number(df_raw$Deaths)))
) %>%
  filter(!is.na(Year), !is.na(AgeGroup5), AgeGroup5 %in% c("10-14","15-19")) %>%
  select(Year, AgeGroup5, Deaths)

# --- 3) Join smoothed population & recompute crude rates (per 100,000) ---
merged <- death %>%
  left_join(pop10, by = c("Year", "AgeGroup5")) %>%
  mutate(
    CrudeRate = 100000 * Deaths / Population
  ) %>%
  arrange(Year, AgeGroup5)

# Sanity check: flag any missing joins
miss <- merged %>% filter(is.na(Population))
if (nrow(miss) > 0) {
  warning("Missing smoothed population for some Year/AgeGroup5 pairs:\n",
          paste0(capture.output(print(miss %>% select(Year, AgeGroup5) %>% distinct())), collapse = "\n"))
}

# --- 4) Write output ---
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(merged %>% select(Year, AgeGroup5, Deaths, Population, CrudeRate), out_file)
cat("Wrote:", out_file, " (rows:", nrow(merged), ")\n")

# --- 5) Plot linreg ---
# Aggregate to total 10–19 across the two 5y bands
rates_10_19 <- merged %>%
  group_by(Year) %>%
  summarise(
    Deaths     = sum(Deaths, na.rm = TRUE),
    Population = sum(Population, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(Rate = 100000 * Deaths / Population) %>%
  arrange(Year)

# Fit linear trend on 2010–2019
train_years <- 2010:2019
model <- lm(Rate ~ Year, data = rates_10_19 %>% filter(Year %in% train_years))

# Predictions for 2010–2023 with 95% prediction intervals
years_all <- data.frame(Year = 2010:2023)
pred <- as.data.frame(predict(model, newdata = years_all, interval = "prediction", level = 0.95))
pred$Year <- years_all$Year

pred_train <- pred %>% filter(Year %in% train_years)
pred_proj  <- pred %>% filter(Year >= 2020)

# Optional: save a tidy CSV with observed + fitted + PI for auditing
out_diag <- "data/c00d48_10-19_rates_trend_smoothedpop_diagnostics.csv"
diag_df <- rates_10_19 %>%
  right_join(pred, by = "Year") %>%
  rename(fit = fit, lwr = lwr, upr = upr)
write_csv(diag_df, out_diag)
cat("Wrote diagnostics:", out_diag, " (rows:", nrow(diag_df), ")\n")

# ---------- Plot (same look & feel as your first script) ----------
col_actual <- "#222222"
col_trend  <- "#1f77b4"
col_ribbon <- "#1f77b4"

ymax_obs  <- max(rates_10_19$Rate, na.rm = TRUE)
ymax_proj <- suppressWarnings(max(pred_proj$upr, na.rm = TRUE))
y_max     <- max(ymax_obs, ymax_proj, na.rm = TRUE)
pad       <- max(0.05 * y_max, 0.3)

p <- ggplot(rates_10_19, aes(Year, Rate)) +
  # prediction ribbon for projection years
  geom_ribbon(
    data = pred_proj, aes(ymin = lwr, ymax = upr, x = Year),
    inherit.aes = FALSE, fill = col_ribbon, alpha = 0.12
  ) +
  # fitted trend (solid) on 2010–2019
  geom_line(data = pred_train, aes(y = fit), color = col_trend, linewidth = 1.6) +
  # projected trend (dashed) on 2020–2023
  geom_line(data = pred_proj,  aes(y = fit), color = col_trend, linewidth = 1.6, linetype = "22") +
  # actual observed rates
  geom_line(color = col_actual, linewidth = 1.6) +
  geom_point(color = col_actual, size = 3.6) +
  geom_text(aes(label = scales::number(Rate, accuracy = 0.1)),
            vjust = -1.5, size = 4.6, color = col_actual) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    limits = c(NA, y_max + pad),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Cancer (C00–D48), ages 10–19 — death rate per 100,000 (smoothed population)",
    subtitle = "Observed rates (2010–2023) vs. 2010–2019 linear trend and projection (95% prediction band)",
    x = NULL, y = "Deaths per 100,000 (ages 10–19)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 12)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y   = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

ggsave(out_img, p, width = 11.5, height = 6.8, dpi = 300)
p