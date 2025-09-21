# Combined fertility rate: births per 1,000 women aged 15–44
# Uses:
#   - data/yearly_births_by_mother_age.csv  (Year, MotherAgeGroup, Births)
#   - data/merged_yearly_female_population_by_age_2020_smoothed.csv (Year, Age, Population)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(ggplot2); library(scales); library(tidyr)
})

births_file     <- "data/yearly_births_by_mother_age.csv"
female_pop_file <- "data/merged_yearly_female_population_by_age_2020_smoothed.csv"
out_img         <- "visual/gfr_15-44_trend.png"
out_csv         <- "data/gfr_15-44_series.csv"

dir.create("visual", showWarnings = FALSE, recursive = TRUE)

# ----- 1) Load inputs -----
births <- read_csv(births_file, show_col_types = FALSE) %>%
  mutate(
    Year           = as.integer(Year),
    MotherAgeGroup = MotherAgeGroup %>%
      str_replace("\\s*years$", "") %>% str_replace_all("[–—]", "-") %>% str_squish(),
    Births         = as.numeric(Births)
  )

female <- read_csv(female_pop_file, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

# ----- 2) Keep/merge the six study groups and sum births by year -----
groups_target <- c("15-19","20-24","25-29","30-34","35-39","40-44")

births_total <- births %>%
  filter(MotherAgeGroup %in% groups_target) %>%
  group_by(Year) %>%
  summarise(Births_15_44 = sum(Births, na.rm = TRUE), .groups = "drop")

# Female population 15–44 (sum single ages)
female_total <- female %>%
  filter(Age >= 15, Age <= 44) %>%
  group_by(Year) %>%
  summarise(FemalePop_15_44 = sum(Population, na.rm = TRUE), .groups = "drop")

# Combined rate per 1,000 women 15–44
rates <- births_total %>%
  inner_join(female_total, by = "Year") %>%
  mutate(Rate = 1000 * Births_15_44 / FemalePop_15_44) %>%
  arrange(Year)

# Save the series (optional)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
write_csv(rates %>% select(Year, Births_15_44, FemalePop_15_44, Rate), out_csv)

# ----- 3) Fit 2010–2019 trend & project to latest year -----
train_years <- 2010:2019
max_year    <- max(rates$Year, na.rm = TRUE)

model <- lm(Rate ~ Year, data = filter(rates, Year %in% train_years))

years_all <- tibble(Year = min(2010, min(rates$Year, na.rm = TRUE)):max_year)
pred_all  <- as.data.frame(predict(model, newdata = years_all,
                                   interval = "prediction", level = 0.95))
pred_all$Year <- years_all$Year

pred_train <- pred_all %>% filter(Year %in% train_years)
pred_proj  <- pred_all %>% filter(Year >= 2020)

# ----- 4) Plot (bigger fonts, thicker lines, value labels) -----
col_actual <- "#222222"
col_trend  <- "#1f77b4"
col_ribbon <- "#1f77b4"

ymax_obs  <- max(rates$Rate, na.rm = TRUE)
ymax_proj <- suppressWarnings(max(pred_proj$upr, na.rm = TRUE))
y_max     <- max(ymax_obs, ymax_proj, na.rm = TRUE)
pad       <- max(0.06 * y_max, 0.3)

p <- ggplot(rates, aes(Year, Rate)) +
  # prediction ribbon for projection years
  geom_ribbon(
    data = pred_proj, aes(ymin = lwr, ymax = upr, x = Year),
    inherit.aes = FALSE, fill = col_ribbon, alpha = 0.12
  ) +
  # 2010–2019 fitted trend (solid)
  geom_line(data = pred_train, aes(y = fit), color = col_trend, linewidth = 1.6) +
  # 2020+ projected trend (dashed)
  geom_line(data = pred_proj,  aes(y = fit), color = col_trend,
            linewidth = 1.6, linetype = "dashed", lineend = "round") +
  # observed series
  geom_line(color = col_actual, linewidth = 1.8) +
  geom_point(color = col_actual, size = 3.4) +
  geom_text(aes(label = number(Rate, accuracy = 0.1)),
            vjust = -0.7, size = 4.6, color = col_actual) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = seq(2010, max_year, by = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     limits = c(NA, y_max + pad),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Births per 1,000 women aged 15–44 (combined)",
    subtitle = "Observed (black) vs. 2010–2019 linear trend (blue) and 2020+ projection (blue, dashed)\nShaded band: 95% prediction interval",
    x = NULL, y = "Births per 1,000 women (15–44)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y   = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(out_img, p, width = 12.5, height = 7.2, dpi = 300)
p

message("Wrote series: ", out_csv, " ; Saved plot: ", out_img)
