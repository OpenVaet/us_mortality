# install.packages(c("tidyverse","broom","scales")) # if needed
library(tidyverse)
library(broom)
library(scales)

# -------- paths --------
in_csv_realtes  <- "data/tes_fakes/real_vs_tes_fake_weekly_excess.csv"
in_csv_patterns <- "data/tes_fakes/tes_manipulation_patterns_full.csv"
out_plot        <- "data/tes_fakes/tes_model_fit_anchored.png"
out_resid       <- "data/tes_fakes/tes_model_residuals_anchored.csv"

dir.create(dirname(out_plot), showWarnings = FALSE, recursive = TRUE)

# -------- load --------
dt <- read_csv(in_csv_realtes, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date) |>
  filter(Date <= as.Date("2024-12-31"))

pf <- read_csv(in_csv_patterns, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

# -------- helpers --------
posneg <- function(x) list(pos = pmax(x, 0), neg = pmax(-x, 0))
norm01 <- function(x, lo, hi) if (hi > lo) pmin(1, pmax(0, (x - lo)/(hi - lo))) else 0

# -------- Phase A: 2020 weeks 1–13 (anchored) --------
A <- pf |>
  filter(Year == 2020, Week <= 13) |>
  transmute(Year, Week, Date, RealExcess, TESFakeExcess,
            TES_A = RealExcess + Difference)

# -------- Phase B: 2020 weeks > 13 (fit linear with time & pos/neg) --------
B <- pf |>
  filter(Year == 2020, Week > 13,
         !is.na(TESFakeExcess), !is.na(RealExcess)) |>
  mutate(
    s = norm01(as.numeric(Date), as.numeric(min(Date)), as.numeric(max(Date))),
    pos = posneg(RealExcess)$pos,
    neg = posneg(RealExcess)$neg
  )

mB <- if (nrow(B) >= 10) {
  lm(TESFakeExcess ~ 1 + s + pos + neg + s:pos + s:neg, data = B)
} else NULL

# -------- Phase C: 2021–2024 (fit linear with time & pos/neg) --------
C <- pf |>
  filter(Date >= as.Date("2021-01-01"), Date <= as.Date("2024-12-31"),
         !is.na(TESFakeExcess), !is.na(RealExcess)) |>
  mutate(
    u = norm01(as.numeric(Date),
               as.numeric(as.Date("2021-01-01")),
               as.numeric(as.Date("2024-12-31"))),
    pos = posneg(RealExcess)$pos,
    neg = posneg(RealExcess)$neg
  )

mC <- if (nrow(C) >= 20) {
  lm(TESFakeExcess ~ 1 + u + pos + neg + u:pos + u:neg, data = C)
} else NULL

# -------- Generate predictions for the whole dt --------
pred_dt <- dt |>
  mutate(
    pos = posneg(RealExcess)$pos,
    neg = posneg(RealExcess)$neg,
    s   = if_else(Year == 2020 & Week > 13,
                  norm01(as.numeric(Date),
                         as.numeric(min(pf$Date[pf$Year==2020 & pf$Week>13], na.rm=TRUE)),
                         as.numeric(max(pf$Date[pf$Year==2020 & pf$Week>13], na.rm=TRUE))),
                  NA_real_),
    u   = if_else(Date >= as.Date("2021-01-01") & Date <= as.Date("2024-12-31"),
                  norm01(as.numeric(Date),
                         as.numeric(as.Date("2021-01-01")),
                         as.numeric(as.Date("2024-12-31"))),
                  NA_real_)
  ) |>
  # attach anchored A values where available
  left_join(A |> select(Year, Week, TES_A), by = c("Year","Week")) |>
  mutate(
    TES_Model =
      dplyr::case_when(
        Year == 2020 & Week <= 13 & is.finite(TES_A) ~ TES_A,
        Year == 2020 & Week > 13  & !is.null(mB)     ~ as.numeric(predict(mB, newdata = cur_data())),
        Date >= as.Date("2021-01-01") & Date <= as.Date("2024-12-31") & !is.null(mC)
                                                   ~ as.numeric(predict(mC, newdata = cur_data())),
        TRUE ~ NA_real_
      )
  )

# -------- Diagnostics --------
valid <- with(pred_dt, !is.na(TESFakeExcess) & !is.na(TES_Model) & Date >= as.Date("2020-01-01"))
corr  <- cor(pred_dt$TESFakeExcess[valid], pred_dt$TES_Model[valid])
rmse  <- sqrt(mean((pred_dt$TESFakeExcess - pred_dt$TES_Model)[valid]^2))
cat(sprintf("Anchored model — 2020–2024:  r = %.3f, RMSE = %.1f\n", corr, rmse))

diag_year <- pred_dt |>
  filter(valid) |>
  group_by(Year) |>
  summarise(
    n = n(),
    rmse = sqrt(mean((TESFakeExcess - TES_Model)^2)),
    bias = mean(TESFakeExcess - TES_Model),
    med_bias = median(TESFakeExcess - TES_Model),
    .groups = "drop"
  )
print(diag_year)

# Save residuals for deeper dive
pred_dt |>
  transmute(Year, Week, Date, RealExcess, TESFakeExcess, TES_Model,
            Residual = TESFakeExcess - TES_Model,
            Phase = case_when(
              Year == 2020 & Week <= 13 ~ "A: early-2020 (anchored)",
              Year == 2020 & Week >  13 ~ "B: rest-2020 (OLS)",
              Year >= 2021              ~ "C: 2021–2024 (OLS)",
              TRUE                      ~ "Other"
            )) |>
  write_csv(out_resid)

cat("Residuals written to:", out_resid, "\n")

# -------- Plot --------
cols <- c("TES (fake)"="#00B050","Model"="#ff7f0e","Real"="#6a7cff")
g <- ggplot(pred_dt, aes(Date)) +
  geom_hline(yintercept = 0, color = "gray75", linewidth = 0.6) +  # slightly darker & thicker
  geom_line(aes(y = RealExcess, color = "Real"), linewidth = 0.9, alpha = 0.7) +
  geom_line(aes(y = TESFakeExcess, color = "TES (fake)"), linewidth = 1.3) +
  geom_line(aes(y = TES_Model, color = "Model"), linewidth = 1.3, linetype = 2) +
  annotate("segment", x = as.Date("2020-01-01"), xend = as.Date("2020-01-01"),
           y = -Inf, yend = Inf, linetype = "dotted", alpha = 0.4) +
  labs(
    title = sprintf("Anchored TES model (r=%.3f, RMSE=%.1f)", corr, rmse),
    subtitle = "A (2020≤W13): exact anchors • B (2020>W13): OLS with s,pos,neg • C (2021–24): OLS with u,pos,neg",
    x = NULL, y = "Weekly Excess Deaths", color = NULL
  ) +
  scale_color_manual(values = cols) +
  theme_minimal(base_size = 16) +  # larger base font size
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.spacing.x = unit(0.6, "cm"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 16)
  )

g

ggsave(out_plot, g, width = 14, height = 7, dpi = 250)  # slightly bigger output
cat("Saved plot:", out_plot, "\n")


ggsave(out_plot, g, width = 12, height = 6, dpi = 200)
cat("Saved plot:", out_plot, "\n")
