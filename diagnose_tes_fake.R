# install.packages(c("tidyverse","zoo","broom")) # if needed
library(tidyverse)
library(zoo)
library(broom)

# ---------- Paths ----------
in_csv     <- "data/tes_fakes/real_vs_tes_fake_weekly_excess.csv"
out_full   <- "data/tes_fakes/tes_manipulation_patterns_full.csv"
out_diag   <- "data/tes_fakes/tes_manipulation_diagnostics.csv"

dir.create(dirname(out_full), showWarnings = FALSE, recursive = TRUE)

# ---------- Tunable thresholds (diagnostic classification) ----------
thr_amp_pos <- 1.20   # Real > 0 and TES/Real > 1.20 => "amplify_positive"
thr_damp_neg <- 0.50  # Real < 0 and |TES|/|Real| < 0.50 => "dampen_negative"
thr_transition <- 0.20 # "transition-like" if Real<0, TES<0, |TES|/|Real| < 0.20
thr_near <- 0.20      # +/-20% band => "near_real" if |TES-Real| <= 0.2*|Real|

# ---------- Load ----------
dt <- read_csv(in_csv, show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  filter(Date <= as.Date("2024-12-31"))

# Safety: keep only rows where Real or TES exists (we still record missing TES)
stopifnot(all(c("Year","Week","Date","RealExcess","TESFakeExcess") %in% names(dt)))

# ---------- Helper: safe division ----------
safe_div <- function(num, den) ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)

# ---------- Per-week diagnostics over full period ----------
patterns_full <- dt %>%
  mutate(
    Difference         = TESFakeExcess - RealExcess,
    Ratio              = safe_div(TESFakeExcess, RealExcess),         # sign-preserving ratio
    EffectiveMultiplier= safe_div(TESFakeExcess, abs(RealExcess)),    # magnitude-based
    SignReal           = sign(RealExcess),
    SignTES            = sign(TESFakeExcess),
    SignFlip           = ifelse(is.na(RealExcess) | is.na(TESFakeExcess), NA,
                                SignReal < 0 & SignTES > 0),
    # Early-2020 labels that mirror the handcrafted interpretation:
    Early2020Amplify   = Year == 2020 & Week %in% 1:4  & RealExcess > 0 & TESFakeExcess > RealExcess,
    Early2020Flip      = Year == 2020 & Week %in% 5:11 & RealExcess < 0 & TESFakeExcess > 0,
    Early2020Trans     = Year == 2020 & Week %in% 12:13 & RealExcess < 0 & TESFakeExcess < 0 &
                         safe_div(abs(TESFakeExcess), abs(RealExcess)) < thr_transition
  ) %>%
  # General classification for all weeks (covers beyond early 2020)
  mutate(
    abs_ratio = safe_div(abs(TESFakeExcess), abs(RealExcess)),
    near_band = ifelse(abs(RealExcess) > 0,
                       abs(TESFakeExcess - RealExcess) <= thr_near * abs(RealExcess),
                       FALSE),
    ManipulationType = case_when(
      is.na(TESFakeExcess) ~ "missing_TES",
      Early2020Amplify     ~ "early_2020_amplify",
      Early2020Flip        ~ "sign_reversal",
      Early2020Trans       ~ "transition",
      # General rules:
      RealExcess < 0 & TESFakeExcess > 0                     ~ "sign_reversal",
      RealExcess > 0 & !is.na(Ratio) & Ratio > thr_amp_pos   ~ "amplify_positive",
      RealExcess < 0 & !is.na(abs_ratio) & abs_ratio < thr_damp_neg ~ "dampen_negative",
      near_band                                               ~ "near_real",
      TRUE                                                    ~ "moderate_adjustment"
    )
  ) %>%
  # Rolling summaries (13-week) to visualize inflation trend
  group_by() %>% # explicitly ungroup if grouped from above
  arrange(Date) %>%
  mutate(
    Diff_13wk_MA   = rollapply(Difference, width = 13, align = "center", fill = NA, FUN = mean, na.rm = TRUE),
    Ratio_13wk_MA  = rollapply(Ratio,      width = 13, align = "center", fill = NA, FUN = mean, na.rm = TRUE),
    Mult_13wk_MA   = rollapply(EffectiveMultiplier, width = 13, align = "center", fill = NA, FUN = mean, na.rm = TRUE)
  )

# Save the per-week full patterns
write_csv(patterns_full, out_full)
cat("Wrote full patterns to:", out_full, "\n")

# ---------- Summaries / diagnostics ----------
# Yearly stats (core view you referenced)
by_year <- patterns_full %>%
  filter(!is.na(TESFakeExcess), !is.na(RealExcess)) %>%
  group_by(Year) %>%
  summarise(
    n_weeks              = n(),
    mean_Real            = mean(RealExcess, na.rm = TRUE),
    mean_TES             = mean(TESFakeExcess, na.rm = TRUE),
    mean_gap_TES_minus_Real = mean(Difference, na.rm = TRUE),
    median_gap           = median(Difference, na.rm = TRUE),
    sign_flips           = sum(SignFlip %in% TRUE, na.rm = TRUE),
    corr_TES_Real        = cor(TESFakeExcess, RealExcess, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(Year)

# Global trend of inflation: slope of Difference over time for 2021–2024
infl_trend <- patterns_full %>%
  filter(Date >= as.Date("2021-01-01"), !is.na(Difference)) %>%
  mutate(t_numeric = as.numeric(Date)) %>%
  do(tidy(lm(Difference ~ t_numeric, data = .))) %>%
  mutate(metric = "trend_2021_2024_Diff_vs_time") %>%
  select(metric, term, estimate, std.error, statistic, p.value)

# Phase counts (how many weeks classified in each bucket)
phase_counts <- patterns_full %>%
  filter(!is.na(ManipulationType)) %>%
  count(ManipulationType, name = "n_weeks")

# Put summaries together (one file, multiple sections stacked)
diag_out <- bind_rows(
  tibble(section = "by_year") %>% bind_cols(by_year),
  tibble(section = "phase_counts") %>% bind_cols(phase_counts),
  tibble(section = "infl_trend") %>% bind_cols(infl_trend)
)

write_csv(diag_out, out_diag)
cat("Wrote diagnostics to:", out_diag, "\n")

# ---------- Console highlights ----------
cat("\n— Yearly summary (key numbers):\n")
print(by_year)

cat("\n— Phase counts across full period:\n")
print(phase_counts)

cat("\n— Inflation trend (2021–2024): slope of (TES−Real) vs time:\n")
print(infl_trend)
