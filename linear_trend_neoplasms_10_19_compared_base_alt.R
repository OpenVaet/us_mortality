# Compare official (unadjusted) vs adjusted (smoothed) 10–19 death rates
# Both: fit 2010–2019 linear trend, project 2020–2023 with 95% PI, and overlay.
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

# ---- Inputs ----
official_file <- "data/C00-D48 - 10-19 - 2010-to-2023.csv"           # official (has pop already)
smoothed_pop  <- "data/merged_yearly_population_by_age_2020_smoothed.csv" # single-age, adjusted
death_file    <- "data/C00-D48 - 10-19 - 2010-to-2023.csv"           # same deaths as official
out_img       <- "visual/c00d48_10-19_official_vs_adjusted_overlay.png"
dir.create("visual", showWarnings = FALSE, recursive = TRUE)

# ---- Helper: build 10–19 rates from an already-aggregated file (official) ----
build_official_rates <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  raw %>%
    mutate(
      Year       = as.integer(Year),
      Deaths     = as.numeric(Deaths),
      Population = as.numeric(Population)
    ) %>%
    filter(!is.na(Year)) %>%
    group_by(Year) %>%
    summarise(
      Deaths     = sum(Deaths, na.rm = TRUE),
      Population = sum(Population, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(Rate = 100000 * Deaths / Population) %>%
    arrange(Year)
}

# ---- Helper: build 10–19 rates from smoothed single-age population + deaths (adjusted) ----
build_adjusted_rates <- function(pop_path, death_path) {
  pop_sm <- read_csv(pop_path, show_col_types = FALSE) %>%
    transmute(
      Year = as.integer(Year),
      Age  = as.integer(Age),
      Population = as.numeric(Population)
    ) %>%
    filter(!is.na(Year), !is.na(Age), !is.na(Population))

  pop10 <- pop_sm %>%
    filter(Age >= 10, Age <= 19) %>%
    mutate(AgeGroup5 = ifelse(Age <= 14, "10-14", "15-19")) %>%
    group_by(Year, AgeGroup5) %>%
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

  df_raw <- read_csv(death_path, col_types = cols(.default = col_character()))
  year_vec <- if ("Year" %in% names(df_raw)) df_raw$Year else df_raw$`Year Code`
  age_vec  <- if ("AgeGroup5" %in% names(df_raw)) df_raw$AgeGroup5 else df_raw$`Five-Year Age Groups`

  death <- tibble(
    Year      = suppressWarnings(as.integer(readr::parse_number(year_vec))),
    AgeGroup5 = age_vec %>%
      str_replace("\\s*years$", "") %>%     # "10-14 years" -> "10-14"
      str_replace_all("[–—]", "-") %>%
      str_trim(),
    Deaths    = suppressWarnings(as.numeric(readr::parse_number(df_raw$Deaths)))
  ) %>%
    filter(!is.na(Year), !is.na(AgeGroup5), AgeGroup5 %in% c("10-14","15-19")) %>%
    select(Year, AgeGroup5, Deaths)

  merged <- death %>%
    left_join(pop10, by = c("Year","AgeGroup5"))

  miss <- merged %>% filter(is.na(Population))
  if (nrow(miss) > 0) {
    warning("Missing smoothed population for some Year/AgeGroup5 pairs:\n",
            paste0(capture.output(print(miss %>% select(Year, AgeGroup5) %>% distinct())), collapse = "\n"))
  }

  merged %>%
    group_by(Year) %>%
    summarise(
      Deaths     = sum(Deaths, na.rm = TRUE),
      Population = sum(Population, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(Rate = 100000 * Deaths / Population) %>%
    arrange(Year)
}

# ---- Build both series ----
rates_off <- build_official_rates(official_file) %>% mutate(series = "Official (unadjusted)")
rates_adj <- build_adjusted_rates(smoothed_pop, death_file) %>% mutate(series = "Adjusted (smoothed)")

# ---- Fit models (2010–2019) and predictions (2010–2023) for each ----
fit_and_predict <- function(rates_df, train_years = 2010:2019) {
  model <- lm(Rate ~ Year, data = filter(rates_df, Year %in% train_years))
  years_all <- data.frame(Year = 2010:2023)
  pred <- as.data.frame(predict(model, newdata = years_all, interval = "prediction", level = 0.95))
  pred$Year <- years_all$Year
  list(
    model = model,
    pred_train = pred %>% filter(Year %in% train_years),
    pred_proj  = pred %>% filter(Year >= 2020)
  )
}

fp_off <- fit_and_predict(rates_off)
fp_adj <- fit_and_predict(rates_adj)

# ---- Combine for plotting ----
rates_both <- bind_rows(rates_off, rates_adj)
ymax_obs   <- max(rates_both$Rate, na.rm = TRUE)
ymax_proj  <- max(c(fp_off$pred_proj$upr, fp_adj$pred_proj$upr), na.rm = TRUE)
y_max      <- max(ymax_obs, ymax_proj, na.rm = TRUE)
pad        <- max(0.05 * y_max, 0.3)

# Colors
col_off_line   <- "#d62728" # red
col_off_ribbon <- "#d62728"
col_adj_line   <- "#1f77b4" # blue
col_adj_ribbon <- "#1f77b4"

# ---- Plot overlay ----
p <- ggplot() +
  # ribbons first (under everything)
  geom_ribbon(
    data = fp_adj$pred_proj,
    aes(x = Year, ymin = lwr, ymax = upr),
    fill = col_adj_ribbon, alpha = 0.12
  ) +
  geom_ribbon(
    data = fp_off$pred_proj,
    aes(x = Year, ymin = lwr, ymax = upr),
    fill = col_off_ribbon, alpha = 0.12
  ) +
  # fitted lines (train solid, proj dashed)
  geom_line(data = fp_adj$pred_train, aes(Year, fit), color = col_adj_line, linewidth = 1.5) +
  geom_line(data = fp_adj$pred_proj,  aes(Year, fit), color = col_adj_line, linewidth = 1.5, linetype = "22") +
  geom_line(data = fp_off$pred_train, aes(Year, fit), color = col_off_line, linewidth = 1.5) +
  geom_line(data = fp_off$pred_proj,  aes(Year, fit), color = col_off_line, linewidth = 1.5, linetype = "22") +
  # observed series (mapped for a clean legend)
  geom_line(
    data = rates_both,
    aes(Year, Rate, color = series),
    linewidth = 1.5
  ) +
  geom_point(
    data = rates_both,
    aes(Year, Rate, color = series),
    size = 3.2
  ) +
  # vertical split between train/proj
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(NA, y_max + pad),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Official (unadjusted)" = col_off_line,
      "Adjusted (smoothed)"   = col_adj_line
    )
  ) +
  labs(
    title    = "Cancer (C00–D48), ages 10–19 — official vs adjusted populations",
    subtitle = "Observed rates (2010–2023) with 2010–2019 linear trend and 95% projection band\nRed = official (unadjusted); Blue = adjusted (smoothed)",
    x = NULL, y = "Deaths per 100,000 (ages 10–19)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 12)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y   = element_text(size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(out_img, p, width = 12, height = 7, dpi = 300)
p


# --- change output name so you keep both versions ---
out_img_ci <- "visual/c00d48_10-19_official_vs_adjusted_overlay_CI.png"

# --- same function, but CI instead of PI ---
fit_and_predict_ci <- function(rates_df, train_years = 2010:2019) {
  model <- lm(Rate ~ Year, data = dplyr::filter(rates_df, Year %in% train_years))
  years_all <- data.frame(Year = 2010:2023)
  # NOTE: interval = "confidence" (not "prediction")
  pred <- as.data.frame(
    predict(model, newdata = years_all, interval = "confidence", level = 0.95)
  )
  pred$Year <- years_all$Year
  list(
    model = model,
    pred_train = dplyr::filter(pred, Year %in% train_years),
    pred_proj  = dplyr::filter(pred, Year >= 2020)
  )
}

# --- compute CI predictions for both series ---
fp_off_ci <- fit_and_predict_ci(rates_off)
fp_adj_ci <- fit_and_predict_ci(rates_adj)

# --- y-axis padding using CI upper bounds ---
ymax_obs  <- max(rates_both$Rate, na.rm = TRUE)
ymax_proj <- max(c(fp_off_ci$pred_proj$upr, fp_adj_ci$pred_proj$upr), na.rm = TRUE)
y_max     <- max(ymax_obs, ymax_proj, na.rm = TRUE)
pad       <- max(0.05 * y_max, 0.3)

# --- plot with 95% CIs (narrower ribbons) ---
p_ci <- ggplot() +
  # CI ribbons first
  geom_ribbon(
    data = fp_adj_ci$pred_proj,
    aes(x = Year, ymin = lwr, ymax = upr),
    fill = col_adj_ribbon, alpha = 0.12
  ) +
  geom_ribbon(
    data = fp_off_ci$pred_proj,
    aes(x = Year, ymin = lwr, ymax = upr),
    fill = col_off_ribbon, alpha = 0.12
  ) +
  # fitted lines (train solid, proj dashed)
  geom_line(data = fp_adj_ci$pred_train, aes(Year, fit), color = col_adj_line, linewidth = 1.5) +
  geom_line(data = fp_adj_ci$pred_proj,  aes(Year, fit), color = col_adj_line, linewidth = 1.5, linetype = "22") +
  geom_line(data = fp_off_ci$pred_train, aes(Year, fit), color = col_off_line, linewidth = 1.5) +
  geom_line(data = fp_off_ci$pred_proj,  aes(Year, fit), color = col_off_line, linewidth = 1.5, linetype = "22") +
  # observed series
  geom_line(data = rates_both, aes(Year, Rate, color = series), linewidth = 1.5) +
  geom_point(data = rates_both, aes(Year, Rate, color = series), size = 3.2) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    limits = c(NA, y_max + pad),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Official (unadjusted)" = col_off_line, "Adjusted (smoothed)" = col_adj_line)
  ) +
  labs(
    title    = "Cancer (C00–D48), ages 10–19 — official vs adjusted populations",
    subtitle = "Observed rates (2010–2023) with 2010–2019 linear trend and 95% confidence band (mean trend)\nRed = official (unadjusted); Blue = adjusted (smoothed)",
    x = NULL, y = "Deaths per 100,000 (ages 10–19)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title      = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 14, margin = margin(b = 12)),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y     = element_text(size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(out_img_ci, p_ci, width = 12, height = 7, dpi = 300)
p_ci
