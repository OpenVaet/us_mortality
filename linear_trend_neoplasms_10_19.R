# 10-19 death rates: recompute per 100,000, fit 2010–2019 trend, plot vs. 2020–2023 outcomes

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

in_path  <- "data/C00-D48 - 10-19 - 2010-to-2023.csv"
out_img  <- "visual/c00d48_10-19_rates_trend.png"

# ---------- Load & recompute rate for total 10-19 ----------
raw <- read_csv(in_path, show_col_types = FALSE)

# Sum deaths/population across 10-19
rates <- raw %>%
  mutate(
    Year       = as.integer(Year),
    Deaths     = as.numeric(Deaths),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(
    Deaths = sum(Deaths, na.rm = TRUE),
    Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Rate = 100000 * Deaths / Population) %>%
  arrange(Year)

# ---------- Fit linear trend on 2010–2019 ----------
train_years <- 2010:2019
model <- lm(Rate ~ Year, data = filter(rates, Year %in% train_years))

# Predictions for 2010–2023, with 95% prediction intervals
years_all <- data.frame(Year = 2010:2023)
pred <- as.data.frame(predict(model, newdata = years_all, interval = "prediction", level = 0.95))
pred$Year <- years_all$Year

pred_train <- pred %>% filter(Year %in% train_years)
pred_proj  <- pred %>% filter(Year >= 2020)

# ---------- Plot  ----------
dir.create("visual", showWarnings = FALSE, recursive = TRUE)

col_actual <- "#222222"
col_trend  <- "#1f77b4"
col_ribbon <- "#1f77b4"

# Headroom so value labels don't clip (considers observed & projected upper PI)
ymax_obs  <- max(rates$Rate, na.rm = TRUE)
ymax_proj <- suppressWarnings(max(pred_proj$upr, na.rm = TRUE))
y_max     <- max(ymax_obs, ymax_proj, na.rm = TRUE)
pad       <- max(0.05 * y_max, 0.3)

p <- ggplot(rates, aes(Year, Rate)) +
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
  # value labels on points (one decimal)
  geom_text(aes(label = scales::number(Rate, accuracy = 0.1)),
            vjust = -1.5, size = 4.6, color = col_actual) +
  # separator between training and projection
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    limits = c(NA, y_max + pad),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Cancer (C00–D48), ages 10-19 — death rate per 100,000",
    subtitle = "Observed rates (2010–2023) vs. 2010–2019 linear trend and projection (95% prediction band)",
    x = NULL, y = "Deaths per 100,000 (ages 10-19)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 12)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y   = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("visual/c00d48_10-19_rates_trend.png", p, width = 11.5, height = 6.8, dpi = 300)
p
