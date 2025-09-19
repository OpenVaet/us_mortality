library(tidyverse)
library(scales)
library(patchwork)

# --- 1) Read monthly data ---
births_m <- read_csv("data/monthly_births_2007_2025.csv", show_col_types = FALSE) # Wonder data, final & provisional, grouped by Year/Month, merged by merge_monthly_natality_data.R
deaths_m <- read_csv("data/monthly_deaths_2007_2025.csv", show_col_types = FALSE) # Wonder data, final & provisional, grouped by Year/Month, merged by merge_monthly_mortality_data.R

# --- 2) Aggregate to yearly totals ---
births_y <- births_m %>% group_by(Year) %>% summarise(Births = sum(Births, na.rm = TRUE), .groups = "drop")
deaths_y <- deaths_m %>% group_by(Year) %>% summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop")

# --- 3) Merge + compute natural growth ---
yearly <- births_y %>%
  full_join(deaths_y, by = "Year") %>%
  arrange(Year) %>%
  mutate(
    Births = replace_na(Births, 0L),
    Deaths = replace_na(Deaths, 0L),
    Natural_Growth = Births - Deaths
  )

# --- 4) Prepare data for plotting ---
bars_df <- yearly %>% pivot_longer(c(Births, Deaths), names_to = "Metric", values_to = "Count")

# YoY % change for natural growth (skip first year and divide-by-zero cases)
growth_df <- yearly %>%
  arrange(Year) %>%
  mutate(
    pct_change = (Natural_Growth - lag(Natural_Growth)) / abs(lag(Natural_Growth)),
    pct_label = ifelse(
      is.na(pct_change) | is.infinite(pct_change),
      "",
      paste0(ifelse(pct_change > 0, "+", ""), percent(pct_change, accuracy = 0.1))
    )
  ) %>%
  select(Year, Natural_Growth, pct_label)

# --- 5) Plot: births & deaths (bars with vertical labels inside) ---
p1 <- ggplot(bars_df, aes(x = factor(Year), y = Count, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = comma(Count)),
    position = position_dodge(width = 0.75),
    angle = 90, hjust = 1.05, color = "white", size = 4.2
  ) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c("Births" = "#006400", "Deaths" = "#C44536")) +
  labs(
    title = "US Yearly Births, Deaths, and Natural Growth",
    y = "Count (people)", x = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "top",
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

# --- Fit linear model on 2007–2019 and build predictions to 2025 ---
fit_df <- yearly %>% filter(Year >= 2007, Year <= 2019)
lm_ng <- lm(Natural_Growth ~ Year, data = fit_df)

pred_df <- tibble(Year = 2007:2025) %>%
  mutate(NG_trend = predict(lm_ng, newdata = cur_data_all()))
pred_hist <- pred_df %>% filter(Year <= 2019)   # solid part
pred_proj <- pred_df %>% filter(Year >= 2020)   # dashed projection

# Update the ymax to include the projection too
max_growth_all <- max(growth_df$Natural_Growth, pred_df$NG_trend, na.rm = TRUE)

# --- 6) Plot: natural growth with value + YoY% label + trend & projection ---
p2 <- ggplot(growth_df, aes(x = Year, y = Natural_Growth)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed") +
  # Actual series
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 3, color = "black") +
  # Trend (fit on 2007–2019) and projection (2020–2025)
  geom_line(data = pred_hist, aes(x = Year, y = NG_trend),
            linewidth = 1.1, color = "#1E88E5") +
  geom_line(data = pred_proj, aes(x = Year, y = NG_trend),
            linewidth = 1.1, color = "#1E88E5", linetype = "longdash") +
  # Value label (black) just above the point
  geom_text(aes(label = comma(Natural_Growth)), vjust = -0.8, size = 4.2) +
  # YoY % label (dark red) slightly to the right of the point
  geom_text(
    aes(label = pct_label),
    color = "#8B0000",
    position = position_nudge(x = 0.25),
    vjust = 1.5,
    size = 4.0
  ) +
  scale_x_continuous(breaks = growth_df$Year) +
  scale_y_continuous(
    labels = label_comma(),
    limits = c(min(growth_df$Natural_Growth, na.rm = TRUE) - 50000, max_growth_all + 500000),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Natural growth (Births - Deaths)",
    caption = "Source: wonder.cdc.gov | Blue: linear fit (2007–2019) with dashed projection (2020–2025)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 12, hjust = 0)
  )


# --- 7) Combine vertically ---
final_plot <- p1 / p2 + plot_layout(heights = c(2.2, 1))
final_plot

# Optional save
# ggsave("data/yearly_births_deaths_growth_labeled.png", fi
