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

# Years with provisional values
provisional_years <- 2024:2025


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
growth_df <- growth_df %>%
  mutate(
    status = ifelse(Year %in% provisional_years, "Provisional", "Final"),
    ng_label = paste0(comma(Natural_Growth), ifelse(Year %in% provisional_years, " *", ""))
  )


# --- 5) Plot: births & deaths (bars with vertical labels inside) ---
p1 <- ggplot(bars_df, aes(x = factor(Year), y = Count, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(comma(Count), ifelse(Year %in% provisional_years, " *", ""))),
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
  geom_line(linewidth = 1, color = "black") +
  geom_point(aes(shape = status), size = 3, color = "black") +  # <- creates the legend
  # trend + projection (unchanged) ...
  geom_text(aes(label = ng_label), vjust = -0.8, size = 4.2) +  # <- value label with *
  geom_text(
    aes(label = pct_label),
    color = "#8B0000",
    position = position_nudge(x = 0.25),
    vjust = 1.5,
    size = 4.0
  ) +
  scale_x_continuous(breaks = growth_df$Year) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(min(growth_df$Natural_Growth, na.rm = TRUE) - 50000, max_growth_all + 500000),
                     expand = c(0, 0)) +
  # ⭐ Legend text explicitly says "* provisional (2020–2025)"
  scale_shape_manual(
    name = NULL,
    values = c("Final" = 16, "Provisional" = 8),      # 16 = solid dot, 8 = star
    breaks = "Provisional",
    labels = c("Provisional" = "* provisional (2024–2025)")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    plot.caption = element_text(size = 12, hjust = 0)
  )




# --- 7) Combine vertically ---
final_plot <- (p1 / p2) + plot_layout(heights = c(2.2, 1), guides = "collect")
final_plot & theme(legend.position = "top")

# Optional save
# ggsave("data/yearly_births_deaths_growth_labeled.png", fi
