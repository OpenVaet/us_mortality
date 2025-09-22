# Total population evolution + horizontal bar "pyramids" by 10-year age groups

library(tidyverse)
library(scales)

# ---------- 1) Load data ----------
# Census data, merged by merge_yearly_pop_est.R
pop <- read_csv("data/merged_yearly_population_by_age_2020_smoothed.csv", show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    Age  = as.integer(Age),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Year), !is.na(Age), !is.na(Population))

# ---------- 2) Total population by year + line chart ----------
totals <- pop %>%
  group_by(Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year)

# Headroom so labels don't collide
y_max <- max(totals$Population, na.rm = TRUE)
pad   <- max(0.02 * y_max, 5e5)

# --- Soft blue accents -------------------------------------------------------  # <<<
soft_blue_dark <- "#2F6FAE"   # line
soft_blue_mid  <- "#7FB3E6"   # points
soft_ink       <- "#294059"   # text (optional; use "black" if you prefer)

p_total <- ggplot(totals, aes(x = Year, y = Population)) +
  geom_line(linewidth = 1, color = soft_blue_dark) +                                      # <<<
  geom_point(size = 3, color = soft_blue_mid) +                                           # <<<
  geom_text(aes(label = comma(Population)), vjust = -0.8, size = 4.5, color = soft_ink) + # <<<
  scale_x_continuous(breaks = totals$Year) +
  scale_y_continuous(labels = label_comma(), limits = c(NA, y_max + pad), expand = c(0, 0)) +
  labs(
    title = "Total population (all ages, both sexes)",
    x = NULL, y = "Population"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title   = element_text(face = "bold", size = 20, margin = margin(b = 8)),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y  = element_text(size = 13),
    panel.grid.minor = element_blank()
  )

# Ensure output folders exist
dir.create("visual", showWarnings = FALSE, recursive = TRUE)
dir.create("visual/pop_pyramide_2020_smoothed", showWarnings = FALSE, recursive = TRUE)

# Save total evolution chart
ggsave("visual/pop_total_evolution_2020_smoothed.png", p_total, width = 12, height = 7, dpi = 300)

# ---------- 3) Build 10-year age groups ----------
max_age <- max(pop$Age, na.rm = TRUE)

pop_grp <- pop %>%
  mutate(
    grp_start = (Age %/% 10) * 10,
    grp_end   = pmin(grp_start + 9, max_age),
    AgeGroup  = paste0(sprintf("%02d", grp_start), "–", sprintf("%02d", grp_end))
  ) %>%
  group_by(Year, grp_start, AgeGroup) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, grp_start)

# ---------- 3) Build 10-year age groups (top bin forced to 80–89) ----------
TOP_GROUP_START <- 80L  # last 10-year bin starts at 80, label as 80–89

mk_agegroup10 <- function(A) {
  gs <- pmin((A %/% 10) * 10, TOP_GROUP_START)        # cap start at 80
  ge <- ifelse(gs >= TOP_GROUP_START, TOP_GROUP_START + 9L, gs + 9L)  # label end 89
  sprintf("%02d–%02d", gs, ge)
}

# Aggregate by Year × 10-year group (using forced top bin)
pop_grp <- pop %>%
  mutate(
    grp_start = pmin((Age %/% 10) * 10, TOP_GROUP_START),
    AgeGroup  = mk_agegroup10(Age)
  ) %>%
  group_by(Year, grp_start, AgeGroup) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, grp_start)

# ---------- 3b) Build 10-year age groups (complete across all years) ----------
age_lookup <- pop %>%
  mutate(
    grp_start = pmin((Age %/% 10) * 10, TOP_GROUP_START),
    AgeGroup  = mk_agegroup10(Age)
  ) %>%
  distinct(AgeGroup, grp_start) %>%
  arrange(grp_start)

age_levels <- age_lookup$AgeGroup  # e.g., 00–09, 10–19, …, 80–89

# Display-only relabel for the last bin
top_group_label <- sprintf("%02d–%02d", TOP_GROUP_START, TOP_GROUP_START + 9L)  # "80–89"
label_fun <- function(x) ifelse(x == top_group_label, "80–85+", x)

pop_grp <- pop %>%
  mutate(
    grp_start = pmin((Age %/% 10) * 10, TOP_GROUP_START),
    AgeGroup  = mk_agegroup10(Age)
  ) %>%
  group_by(Year, AgeGroup) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(
    Year,
    AgeGroup = factor(age_levels, levels = age_levels),
    fill = list(Population = 0)
  ) %>%
  left_join(age_lookup, by = "AgeGroup") %>%
  arrange(Year, grp_start)

# Fixed X max and breaks every 10M
x_max <- 55000000
x_breaks <- seq(0, x_max, by = 10000000)

# ---------- Soft blue gradient across age groups ------------------------------  # <<<
soft_blues <- colorRampPalette(c(
  "#ECF4FE", "#DCEBFB", "#CBE2F8", "#BAD8F4",
  "#A8CFF0", "#96C5EC", "#84BBE8", "#72B1E4",
  "#5FA7E0", "#4D9EDC", "#3A94D8", "#2F8BD4"
))(length(age_levels))
names(soft_blues) <- age_levels

# ---------- 4) Horizontal bar "pyramids" by 10-year age groups ----------
years <- sort(unique(pop_grp$Year))
n_groups <- length(age_levels)

for (yr in years) {
  df_y <- pop_grp %>%
    filter(Year == yr) %>%
    arrange(grp_start) %>%
    mutate(AgeGroup = factor(AgeGroup, levels = age_levels))  # lock global order

  # Fixed height based on total number of groups so frames are consistent
  h <- max(6, n_groups * 0.35)

  # --- label logic ---
  thr_prop     <- 0.06          # bars smaller than 6% of x_max get outside labels
  label_offset <- 0.01 * x_max  # gap to the right of the bar for outside labels

  df_inside  <- df_y %>%
    filter(Population > 0, Population >= thr_prop * x_max) %>%
    mutate(label = scales::comma(Population),
           x_lab = Population / 2)

  df_outside <- df_y %>%
    filter(Population > 0, Population <  thr_prop * x_max) %>%
    mutate(label = scales::comma(Population),
           x_lab = pmin(Population + label_offset, x_max))  # keep inside axis range

  p_pyr <- ggplot(df_y, aes(y = AgeGroup, x = Population, fill = AgeGroup)) +
    geom_col(width = 0.8) +
    # Labels with enough room: centered, black
    geom_text(
      data = df_inside,
      aes(x = x_lab, label = label),
      color = "black", size = 3.8, vjust = 0.5
    ) +
    # Labels with not enough room: to the right of the bar, black
    geom_text(
      data = df_outside,
      aes(x = x_lab, label = label),
      color = "black", size = 3.8, hjust = 0, vjust = 0.5
    ) +
    scale_fill_manual(values = soft_blues, limits = age_levels, drop = FALSE) +           # <<<
    scale_x_continuous(
      labels = scales::comma,
      limits = c(0, x_max),
      breaks = x_breaks,
      expand = c(0, 0)
    ) +
    scale_y_discrete(labels = label_fun) +   # <- display "80–85+" instead of "80–89"
    labs(
      title = paste0("2020 Smoothed Population by 10-year age groups - ", yr),
      x = "Population", y = "Age group"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title  = element_text(face = "bold", size = 18, margin = margin(b = 8)),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  ggsave(file.path("visual/pop_pyramide_2020_smoothed", sprintf("pyramide_%d.png", yr)),
         p_pyr, width = 9, height = h, dpi = 300)
}

cat("Saved:\n - visual/pop_total_evolution_2020_smoothed.png\n -", length(years), "horizontal bar pyramids in visual/pop_pyramide_2020_smoothed/\n")

library(magick)
library(stringr)

in_dir  <- "visual/pop_pyramide_2020_smoothed"
out_gif <- file.path(in_dir, "pyramide_years.gif")

files <- list.files(in_dir, pattern = "^pyramide_\\d{4}\\.png$", full.names = TRUE)
stopifnot("No pyramid images found." = length(files) > 0)

years <- as.integer(str_extract(basename(files), "\\d{4}"))
files <- files[order(years)]

imgs <- image_read(files)

# Align frames (pad to the largest W/H)
info <- image_info(imgs)
target_w <- max(info$width, na.rm = TRUE)
target_h <- max(info$height, na.rm = TRUE)
imgs_aligned <- image_extent(
  imgs,
  geometry = sprintf("%dx%d", target_w, target_h),
  gravity  = "center",
  color    = "white"
)

# --- NO COMPRESSION per request ----------------------------------------------  # <<<
# (Do not downscale or quantize; keep full resolution and palette)
# imgs_aligned <- image_scale("1200x")
# imgs_aligned <- image_quantize(max = 128)

# Animate: 1 second per frame
anim <- image_animate(imgs_aligned, fps = 1, loop = 0)

# Write GIF
image_write(anim, out_gif)
message("GIF written to: ", out_gif)
  