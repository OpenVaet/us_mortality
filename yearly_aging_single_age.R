#########################################################
## Cohort aging table by single age (HTML with highlights)
## Δ at (Year N, Age A) = Pop[N, A] − Pop[N−1, A−1]
## Source: data/merged_yearly_population_by_age.csv
#########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

# --- Parameters ---
TARGET_DELTA <- 50000L   # expected cohort change
TOL          <- 0L       # tolerance around TARGET_DELTA (e.g., 2500L)

# --- Load ---
pop <- read_csv("data/merged_yearly_population_by_age.csv", show_col_types = FALSE) %>%
  transmute(Year = as.integer(Year),
            Age  = as.integer(Age),
            Population = as.numeric(Population)) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

yrs <- sort(unique(pop$Year))

# --- Wide matrix of populations (Age rows, Year cols); drop Age 0 in the output
pop_wide_years <- pop %>%
  pivot_wider(id_cols = Age, names_from = Year, values_from = Population) %>%
  arrange(Age) %>%
  filter(Age >= 1)  # “starting in 2010, we never care about Age 0”

# --- Cohort deltas: (N,A) minus (N-1,A-1) ---
delta_long <- pop %>%
  mutate(Year_prev = Year - 1, Age_prev = Age - 1) %>%
  left_join(pop %>% select(Year, Age, Population) %>%
              rename(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
            by = c("Year_prev", "Age_prev")) %>%
  filter(!is.na(Pop_prev), Age >= 1) %>%
  transmute(Age, Year, delta = Population - Pop_prev)

delta_wide <- delta_long %>%
  mutate(delta_col = paste0(Year, "-", Year - 1)) %>%
  select(Age, delta_col, delta) %>%
  pivot_wider(names_from = delta_col, values_from = delta)

# --- Combine absolute years + cohort deltas, with two-line cells (value + Δ) ---
# Colors
CLR_POS_TEXT <- "#006400"  # dark green
CLR_NEG_TEXT <- "#8B0000"  # dark red
CLR_POS_BG   <- "#d8f3dc"  # light green
CLR_NEG_BG   <- "#ffcccc"  # light red

# Threshold for highlight (set this near your Parameters section if you prefer)
HIGHLIGHT_THRESHOLD <- 50000L

# ---- PRINT-FRIENDLY HEAT MAPS (single ages, same scope) ----
suppressPackageStartupMessages({ library(tidyverse) })

OUT_DIR <- "visual/pop_aging"
dir.create(file.path(OUT_DIR, "heatmaps"), recursive = TRUE, showWarnings = FALSE)

YEAR_MIN <- min(yrs, na.rm = TRUE)
YEAR_MAX <- max(yrs, na.rm = TRUE)
year_span <- paste0(YEAR_MIN, "-", YEAR_MAX)

# Compact formatters (SI suffixes)
fmt_si     <- scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))
fmt_si_int <- scales::label_number(accuracy = 1,   scale_cut = scales::cut_si(""))

# Build per-cell values + labels (same data scope: Age >= 1; all years in `yrs`)
df <- pop %>%
  filter(Age >= 1) %>%
  left_join(delta_long, by = c("Year","Age")) %>%
  mutate(
    pop_lbl   = fmt_si(Population),
    delta_lbl = ifelse(is.na(delta), "", paste0(ifelse(delta > 0, "+", ""), fmt_si_int(delta))),
    label     = ifelse(delta_lbl == "", pop_lbl, paste0(pop_lbl, "\nΔ ", delta_lbl))
  )

# Clamp only for color mapping (labels use true values)
q98   <- stats::quantile(abs(df$delta), 0.98, na.rm = TRUE)
limit <- max(HIGHLIGHT_THRESHOLD, q98, na.rm = TRUE)
df <- df %>%
  mutate(
    delta_clamped = pmax(pmin(delta, limit), -limit),
    extreme       = !is.na(delta) & abs(delta) > HIGHLIGHT_THRESHOLD
  )

# Render in the SAME 20-age blocks (no scope change)
AGES_PER_PLOT <- 20L
ages_all      <- sort(unique(df$Age))
starts_hm     <- seq(1, length(ages_all), by = AGES_PER_PLOT)

for (i in seq_along(starts_hm)) {
  a_start <- ages_all[starts_hm[i]]
  a_end   <- ages_all[min(starts_hm[i] + AGES_PER_PLOT - 1, length(ages_all))]

  df_block <- df %>% filter(Age >= a_start, Age <= a_end)

  # Per-block color center & readable text color
  lim <- max(abs(df_block$delta_clamped), na.rm = TRUE)
  df_block <- df_block %>%
    mutate(text_col = dplyr::case_when(
      is.na(delta_clamped) ~ "black",
      abs(delta_clamped) > 0.55 * lim ~ "white",
      TRUE ~ "black"
    ))

  p <- ggplot(df_block, aes(x = Year, y = Age, fill = delta_clamped)) +
    geom_tile(color = "grey80", linewidth = 0.25) +
    geom_tile(
      data = subset(df_block, extreme),
      aes(x = Year, y = Age),
      fill = NA, color = "black", linewidth = 0.45
    ) +
    geom_text(
      aes(label = label, color = text_col),
      size = 3.0, lineheight = 0.92
      # , family = "Arial Narrow"  # uncomment if available to fit more text
    ) +
    scale_color_identity() +
    scale_y_continuous(breaks = a_start:a_end) +   # same orientation as your tables (no reverse)
    scale_x_continuous(breaks = seq(YEAR_MIN, YEAR_MAX, by = 2)) +
    scale_fill_gradient2(
      name = "Δ (Pop[N,A] – Pop[N−1,A−1])",
      low = "#b30000", mid = "#f2f2f2", high = "#007a1f",
      midpoint = 0, limits = c(-lim, lim),
      labels = scales::label_number(scale_cut = scales::cut_si("")),
      na.value = "grey95"
    ) +
    labs(
      title = paste0("Cohort Aging — Single Ages ", a_start, "–", a_end),
      subtitle = paste0("Years ", year_span, ". Cells outlined up to |Δ| > ",
                        scales::comma(HIGHLIGHT_THRESHOLD),
                        ". Red = negative Δ, Green = positive Δ."),
      x = NULL, y = "Age"
    ) +
    # ⬇⬇ NEW: horizontal legend at bottom
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = grid::unit(6, "in"),
        barheight = grid::unit(0.25, "in")
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "bottom",              # ⬅ bottom legend
      legend.direction = "horizontal",
      legend.title = element_text(size = 15, face = "bold"),
      legend.text  = element_text(size = 14),
      legend.key.height = grid::unit(0.9, "cm"),
      legend.key.width  = grid::unit(0.7, "cm"),
      legend.background = element_rect(fill = "white", color = NA), # ⬅ white legend bg
      legend.box.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),   # ⬅ white plot bg
      panel.background = element_rect(fill = "white", color = NA),   # ⬅ white panel bg
      plot.margin = margin(12, 16, 12, 16)
    )


  # Export (wide) — matches your 5-year figures
  fn_base <- sprintf("single_age_heatmap_%02d-%02d_%s_wide16x8", a_start, a_end, year_span)
  ggsave(file.path(OUT_DIR, "heatmaps", paste0(fn_base, ".pdf")),
         p, width = 16, height = 8, units = "in")
  ggsave(file.path(OUT_DIR, "heatmaps", paste0(fn_base, ".png")),
         p, width = 16, height = 8, units = "in", dpi = 300)
}
