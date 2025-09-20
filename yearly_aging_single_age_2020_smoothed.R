#########################################################
## Cohort aging table by single age with 85+ capping
## Δ at (Year N, Age A) = Pop_cap[N, A] − Pop_cap[N−1, A−1]
## Source: data/merged_yearly_population_by_age_2020_smoothed.csv
## Requires : smooth_2020_correction.R to run first
#########################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
})

# --- Parameters ---
TARGET_DELTA <- 50000L   # expected cohort change (unused here, kept for context)
TOL          <- 0L
HIGHLIGHT_THRESHOLD <- 50000L   # highlight when |Δ| > 50k
AGE_CAP_SINGLE <- 85L           # collapse all ages >= 85 into 85+

# --- Load ---
pop_raw <- read_csv("data/merged_yearly_population_by_age_2020_smoothed.csv",
                    show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

# --- Collapse ages >= 85 into a single 85+ bin BEFORE computing deltas ---
pop_cap <- pop_raw %>%
  mutate(Age = pmin(Age, AGE_CAP_SINGLE)) %>%
  group_by(Year, Age) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

yrs <- sort(unique(pop_cap$Year))

# --- Cohort deltas on the COLLAPSED series: (N,A) − (N−1, A−1) ---
delta_long <- pop_cap %>%
  mutate(Year_prev = Year - 1L,
         Age_prev  = pmax(Age - 1L, 0L)) %>%        # Age 0 has no predecessor
  left_join(
    pop_cap %>% transmute(Year_prev = Year,
                          Age_prev  = Age,
                          Pop_prev  = Population),
    by = c("Year_prev", "Age_prev")
  ) %>%
  transmute(Age, Year, delta = Population - Pop_prev)

# --- Build two-line cells (value + Δ) ---
# Colors
CLR_POS_TEXT <- "#006400"  # dark green
CLR_NEG_TEXT <- "#8B0000"  # dark red
CLR_POS_BG   <- "#d8f3dc"  # light green
CLR_NEG_BG   <- "#ffcccc"  # light red

combined_cells <- pop_cap %>%
  filter(Age >= 1) %>%  # keep Age 1+ in output (as in your original)
  left_join(delta_long, by = c("Year", "Age")) %>%
  mutate(
    pop_txt       = ifelse(is.na(Population), "", scales::comma(round(Population))),
    delta_txt_raw = ifelse(is.na(delta), "", scales::comma(round(delta))),
    delta_color = case_when(
      is.na(delta) ~ "black",
      delta > 0    ~ CLR_POS_TEXT,
      TRUE         ~ CLR_NEG_TEXT
    ),
    delta_bg = case_when(
      is.na(delta)                 ~ "transparent",
      delta >  HIGHLIGHT_THRESHOLD ~ CLR_POS_BG,
      delta < -HIGHLIGHT_THRESHOLD ~ CLR_NEG_BG,
      TRUE                         ~ "transparent"
    ),
    delta_html = ifelse(
      delta_txt_raw == "",
      "",
      kableExtra::cell_spec(
        delta_txt_raw,
        color = delta_color,
        background = delta_bg,
        escape = FALSE,
        extra_css = "font-size:0.9em;"
      )
    ),
    cell_html = ifelse(
      pop_txt == "",
      "",
      paste0(pop_txt, ifelse(delta_html == "", "", "<br/>"), delta_html)
    )
  ) %>%
  select(Age, Year, cell_html)

# --- Pivot to wide (Age rows, Year columns) ---
out <- combined_cells %>%
  tidyr::pivot_wider(id_cols = Age, names_from = Year, values_from = cell_html) %>%
  arrange(Age) %>%
  select(Age, all_of(as.character(yrs)))

# For display, show "85+" for the capped top age
out_disp <- out %>%
  mutate(Age = ifelse(Age == AGE_CAP_SINGLE, "85+", as.character(Age)))

# ---- Render to files in visual/pop_aging_2010_correc (20 ages per table; all years as columns) ----
AGES_PER_TABLE <- 20
out_dir <- "visual/pop_aging_2020_smoothed"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# We'll use numeric ages for ranges/file names, but display "85+" in the table.
ages_num_vec <- out$Age
n_ages       <- length(ages_num_vec)
starts       <- seq(1, n_ages, by = AGES_PER_TABLE)

# helper to pretty-print age endpoints in captions
age_label <- function(a) ifelse(a == AGE_CAP_SINGLE, "85+", as.character(a))

html_fragments <- character(length(starts))

for (i in seq_along(starts)) {
  idx_start <- starts[i]
  idx_end   <- min(idx_start + AGES_PER_TABLE - 1, n_ages)

  out_block_disp <- out_disp %>% dplyr::slice(idx_start:idx_end)

  cap <- paste0(
    "Cohort aging by single age (85+ capped): totals with Δ (below). ",
    "Δ at (N,A) = Pop[N,A] − Pop[N−1,A−1]. ",
    "Δ colored dark green/red; highlighted when |Δ| > ",
    scales::comma(HIGHLIGHT_THRESHOLD), ". ",
    "Ages ", age_label(ages_num_vec[idx_start]), "–", age_label(ages_num_vec[idx_end]),
    " (Table ", i, " of ", length(starts), ")"
  )

  kobj <- kableExtra::kbl(
    out_block_disp,
    format  = "html",
    escape  = FALSE,
    caption = cap,
    align   = c("r", rep("r", ncol(out_block_disp) - 1))
  ) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center") |>
    kableExtra::row_spec(0, bold = TRUE)

  # file name uses numeric ages (85 stays "85")
  file_html <- file.path(
    out_dir,
    sprintf("cohort_aging_ages_%02d-%02d.html",
            ages_num_vec[idx_start], ages_num_vec[idx_end])
  )
  kableExtra::save_kable(kobj, file = file_html)

  html_fragments[i] <- paste0(
    "<section>", as.character(kobj), "</section>",
    if (i < length(starts)) "<div class='pagebreak'></div>" else ""
  )
}

# combined index.html with page breaks
index_html <- paste0(
  "<!DOCTYPE html><html><head><meta charset='utf-8'>",
  "<title>Cohort aging by single age — 85+ capped</title>",
  "<style>",
  "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:24px}",
  ".pagebreak{page-break-after: always;}",
  "table{margin-left:auto;margin-right:auto}",
  "</style></head><body>",
  paste0(html_fragments, collapse = "\n"),
  "</body></html>"
)
writeLines(index_html, file.path(out_dir, "index.html"))

message("Wrote ", length(starts), " tables to ", out_dir, " and a combined index.html")
