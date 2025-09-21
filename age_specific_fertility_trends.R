# One plot: six lines (observed), with per-group 2010–2019 trend (dashed) and 2020+ projection (dotted)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

births_file     <- "data/yearly_births_by_mother_age.csv"
female_pop_file <- "data/merged_yearly_female_population_by_age_2020_smoothed.csv"
out_img         <- "visual/asfr_trends_oneplot.png"

dir.create("visual", showWarnings = FALSE, recursive = TRUE)

# ---------- 1) Load data ----------
births <- read_csv(births_file, show_col_types = FALSE) %>%
  transmute(
    Year           = as.integer(Year),
    MotherAgeGroup = MotherAgeGroup %>%
      str_replace("\\s*years$", "") %>% str_replace_all("[–—]", "-") %>% str_squish(),
    Births         = as.numeric(Births)
  )

female <- read_csv(female_pop_file, show_col_types = FALSE) %>%
  transmute(
    Year       = as.integer(Year),
    Age        = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

# ---------- 2) Aggregate female population into 5-year mother-age groups ----------
groups_target <- c("15-19","20-24","25-29","30-34","35-39","40-44")

female_grp <- female %>%
  filter(Age >= 15, Age <= 44) %>%
  mutate(
    MotherAgeGroup = case_when(
      Age <= 19 ~ "15-19",
      Age <= 24 ~ "20-24",
      Age <= 29 ~ "25-29",
      Age <= 34 ~ "30-34",
      Age <= 39 ~ "35-39",
      TRUE      ~ "40-44"
    )
  ) %>%
  group_by(Year, MotherAgeGroup) %>%
  summarise(FemalePop = sum(Population, na.rm = TRUE), .groups = "drop")

# ---------- 3) Compute ASFR (per 1,000) ----------
rates <- births %>%
  filter(MotherAgeGroup %in% groups_target) %>%
  left_join(female_grp, by = c("Year","MotherAgeGroup")) %>%
  filter(!is.na(FemalePop)) %>%
  mutate(ASFR = 1000 * Births / FemalePop) %>%
  arrange(MotherAgeGroup, Year)

# ---------- 4) Fit 2010–2019 linear trend per group & build projections ----------
train_years <- 2010:2019
max_year    <- max(rates$Year, na.rm = TRUE)

pred_all <- rates %>%
  group_by(MotherAgeGroup) %>%
  do({
    df <- .
    train <- df %>% filter(Year %in% train_years)
    if (nrow(train) < 2) return(tibble(Year=integer(), fit=numeric()))
    m <- lm(ASFR ~ Year, data = train)
    yrs <- tibble(Year = 2010:max_year)
    tibble(Year = yrs$Year, fit = as.numeric(predict(m, newdata = yrs)))
  }) %>%
  ungroup()

pred_train <- pred_all %>% filter(Year %in% train_years)
pred_proj  <- pred_all %>% filter(Year >= 2020)

# ---------- 5) Plot: one panel, 6 observed lines; dashed = fit (2010–2019); dotted = projection (2020+) ----------
col_palette <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b") # 6 distinct colors
names(col_palette) <- groups_target

# y headroom so lines/markers don’t clip
y_max <- max(rates$ASFR, pred_all$fit, na.rm = TRUE)
pad   <- max(0.06 * y_max, 0.2)

p <- ggplot() +
  # observed
  geom_line(data = rates,
            aes(x = Year, y = ASFR, color = MotherAgeGroup),
            linewidth = 1.6) +
  geom_point(data = rates,
             aes(x = Year, y = ASFR, color = MotherAgeGroup),
             size = 2.8) +

  geom_line(data = pred_train, aes(x = Year, y = fit, color = MotherAgeGroup),
            linewidth = 1.1, linetype = "dashed", lineend = "round") +
  geom_line(data = pred_proj,  aes(x = Year, y = fit, color = MotherAgeGroup),
            linewidth = 1.1, linetype = "dotted", lineend = "round") +

  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = col_palette, name = "Mother's age group") +
  scale_x_continuous(breaks = seq(2010, max_year, by = 1)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(NA, y_max + pad),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Age-specific fertility rates (ASFR) per 1,000 women, by mother's age",
    subtitle = "Observed (thick), 2010–2019 linear trends (dashed) and 2020+ projections (dotted)",
    x = NULL, y = "Births per 1,000 women"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 22, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y   = element_text(size = 12),
    legend.position = "top",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(out_img, p, width = 13, height = 8, dpi = 300)
p

message("Saved plot to: ", out_img)
