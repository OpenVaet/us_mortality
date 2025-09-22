###############################################################
## Cohort aging by 5-year age groups (keeps Age 0)
## Δ at year N for a group = sum_A_in_group [ Pop(N,A) - Pop(N-1,A-1) ]
## Source: data/merged_yearly_population_by_age_2010_correction.csv
## Requires : merge_yearly_pop_est_2010_correc.R to run first
###############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

# ---- Params ----
HIGHLIGHT_THRESHOLD <- 250000L  # highlight when |Δ| > 50,000

# ---- Load ----
pop <- read_csv("data/merged_yearly_population_by_age_2010_correction.csv", show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

yrs     <- sort(unique(pop$Year))
max_age <- max(pop$Age, na.rm = TRUE)

# ---- Collapse ages BEFORE deltas: top-code at 85 (≥85 -> 85) ----
AGE_TOP <- 85L  # *** was 89L ***

pop_cap <- pop %>%
  mutate(Age = pmin(Age, AGE_TOP)) %>%
  group_by(Year, Age) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# ---- Cohort deltas on the COLLAPSED series ----
delta_long <- pop_cap %>%
  mutate(Year_prev = Year - 1L, Age_prev = pmax(Age - 1L, 0L)) %>%
  left_join(
    pop_cap %>% transmute(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
    by = c("Year_prev", "Age_prev")
  ) %>%
  transmute(Year, Age, delta = Population - Pop_prev)

# ---- 5-year groups with top bin fixed to 85+ (label it clearly) ----
mk_group5_capped <- function(A) {
  gs <- (A %/% 5) * 5
  gs <- ifelse(gs >= 85, 85, gs)   # force top bin start to 85
  ge <- ifelse(gs == 85, 89, gs + 4)
  sprintf("%02d–%02d", gs, ge)     # this is effectively "85+" when gs==85
}

pop_group_year <- pop_cap %>%
  mutate(grp_start = ifelse(Age >= 85, 85L, (Age %/% 5) * 5L),
         AgeGroup5 = mk_group5_capped(Age)) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

delta_group_year <- delta_long %>%
  mutate(grp_start = ifelse(Age >= 85, 85L, (Age %/% 5) * 5L),
         AgeGroup5 = mk_group5_capped(Age)) %>%
  group_by(AgeGroup5, grp_start, Year) %>%
  summarise(Delta = sum(delta, na.rm = TRUE), .groups = "drop")

suppressPackageStartupMessages({ library(tidyverse) })

# ---- Display params (no math changes) ----
OUT_DIR  <- "visual/pop_aging"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

YEAR_MIN <- min(pop_cap$Year, na.rm = TRUE)
YEAR_MAX <- max(pop_cap$Year, na.rm = TRUE)
year_span <- paste0(YEAR_MIN, "-", YEAR_MAX)

# Merge totals + Δ (keep first year even if Δ is NA)
df5 <- pop_group_year %>%
  left_join(delta_group_year, by = c("AgeGroup5","grp_start","Year")) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX)

# Order groups youngest → oldest; optionally relabel 85–89 as 85+
group_levels <- df5 %>% distinct(AgeGroup5, grp_start) %>% arrange(grp_start) %>% pull(AgeGroup5)
df5 <- df5 %>% mutate(AgeGroup5 = factor(AgeGroup5, levels = group_levels))
levels(df5$AgeGroup5)[levels(df5$AgeGroup5) == "85–89"] <- "85+"

# Compact label formatters (SI suffixes)
fmt_si     <- scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))
fmt_si_int <- scales::label_number(accuracy = 1,   scale_cut = scales::cut_si(""))

# Two-line labels: Population (line 1) + Δ (line 2, signed if present)
df5 <- df5 %>%
  mutate(
    pop_lbl   = fmt_si(Population),
    delta_lbl = ifelse(is.na(Delta), "", paste0(ifelse(Delta > 0, "+", ""), fmt_si_int(Delta))),
    label     = ifelse(delta_lbl == "", pop_lbl, paste0(pop_lbl, "\nΔ ", delta_lbl))
  )

# Clamp for color mapping only (labels use true values)
q98   <- stats::quantile(abs(df5$Delta), 0.98, na.rm = TRUE)
limit <- max(HIGHLIGHT_THRESHOLD, q98, na.rm = TRUE)
df5 <- df5 %>%
  mutate(
    delta_clamped = pmax(pmin(Delta, limit), -limit),
    extreme       = !is.na(Delta) & abs(Delta) > HIGHLIGHT_THRESHOLD
  )

# Legend limits & readable text color per cell
lim <- max(abs(df5$delta_clamped), na.rm = TRUE)
df5 <- df5 %>%
  mutate(text_col = dplyr::case_when(
    is.na(delta_clamped) ~ "black",
    abs(delta_clamped) > 0.55 * lim ~ "white",
    TRUE ~ "black"
  ))

# ---- Plot (legend bottom, white background) ----
p5 <- ggplot(df5, aes(x = Year, y = AgeGroup5, fill = delta_clamped)) +
  geom_tile(color = "grey80", linewidth = 0.25, na.rm = FALSE) +
  geom_tile(
    data = subset(df5, extreme),
    aes(x = Year, y = AgeGroup5),
    fill = NA, color = "black", linewidth = 0.45
  ) +
  geom_text(
    aes(label = label, color = text_col),
    size = 3.2, lineheight = 0.92
    # , family = "Arial Narrow"  # uncomment if installed
  ) +
  scale_color_identity() +
  scale_x_continuous(breaks = seq(YEAR_MIN, YEAR_MAX, by = 2)) +
  scale_fill_gradient2(
    name = "Δ (sum over ages in group of Pop[N,A] – Pop[N−1,A−1])",
    low = "#b30000", mid = "#f2f2f2", high = "#007a1f",
    midpoint = 0, limits = c(-lim, lim),
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    na.value = "grey95"
  ) +
  labs(
    title = "Cohort Aging — 5-Year Age Groups (Population & Δ in cells)",
    subtitle = paste0(
      "Top-coded at 85+ (capped before deltas). Cells outlined when |Δ| > ",
      scales::comma(HIGHLIGHT_THRESHOLD),
      ". Red = negative Δ, Green = positive Δ."
    ),
    x = NULL, y = "Age Group (5y)"
  ) +
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
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 15, face = "bold"),
    legend.text  = element_text(size = 14),
    legend.key.height = grid::unit(0.9, "cm"),
    legend.key.width  = grid::unit(0.7, "cm"),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.background      = element_rect(fill = "white", color = NA),
    plot.background       = element_rect(fill = "white", color = NA),
    plot.margin = margin(12, 16, 12, 16)
  )

# ---- Save (wide, print-friendly; white background) ----
ggsave(file.path(OUT_DIR, paste0("cohort_5y_groups_2010correc_pop_plus_delta_", year_span, "_wide16x8.pdf")),
       p5, width = 16, height = 8, units = "in", bg = "white")
ggsave(file.path(OUT_DIR, paste0("cohort_5y_groups_2010correc_pop_plus_delta_", year_span, "_wide16x8.png")),
       p5, width = 16, height = 8, units = "in", dpi = 300, bg = "white")
