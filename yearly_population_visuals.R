# Total population evolution + horizontal bar "pyramids" by 10-year age groups
# PRETTY BLUE EDITION (no compression, soft blues only)

library(tidyverse)
library(scales)

# ---------- 1) Load data ----------
pop <- read_csv("data/merged_yearly_population_by_age.csv", show_col_types = FALSE) %>%
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

y_max <- max(totals$Population, na.rm = TRUE)
pad   <- max(0.02 * y_max, 5e5)

# --- Soft blue palette + theme helpers  -------------------------------------- # <<<
soft_blue_dark  <- "#2F6FAE"   # accents (line/points/text)
soft_blue_mid   <- "#7FB3E6"   # mid accents
soft_blue_light <- "#E7F1FB"   # backgrounds
soft_ink        <- "#294059"   # axis/title text
soft_grid       <- "#C9D9EE"

theme_soft_blue <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = soft_ink),
      plot.title = element_text(face = "bold", size = base_size + 4, margin = margin(b = 8)),
      axis.text = element_text(size = base_size - 3),
      axis.title = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = soft_grid),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = soft_blue_light, color = NA) # subtle tint
    )
}

p_total <- ggplot(totals, aes(x = Year, y = Population)) +
  geom_line(linewidth = 1.3, color = soft_blue_dark) +                                  # <<<
  geom_point(size = 3.4, color = soft_blue_mid, stroke = 0.8) +                         # <<<
  geom_text(aes(label = comma(Population)),
            vjust = -0.8, size = 4.2, color = soft_ink) +                               # <<<
  scale_x_continuous(breaks = totals$Year, expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_comma(), limits = c(NA, y_max + pad), expand = c(0, 0)) +
  labs(
    title = "Total population (all ages, both sexes)",
    x = NULL, y = "Population"
  ) +
  theme_soft_blue() +                                                                    # <<<
  theme(axis.text.x  = element_text(angle = 45, hjust = 1))

# Ensure output folders exist
dir.create("visual", showWarnings = FALSE, recursive = TRUE)
dir.create("visual/pop_pyramide", showWarnings = FALSE, recursive = TRUE)

# Save total evolution chart (crisp AA text)
# Prefer ragg if available for best text rendering; otherwise Cairo
if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png("visual/pop_total_evolution.png", width = 12, height = 7, units = "in", res = 300)  # <<<
  print(p_total)
  dev.off()
} else {
  grDevices::cairo_pdf() # ensure Cairo is loaded
  ggsave("visual/pop_total_evolution.png", p_total, width = 12, height = 7, dpi = 300, device = "cairo_png")  # <<<
}

# ---------- 3) Build 10-year age groups ----------
max_age <- max(pop$Age, na.rm = TRUE)

# Lookup of all 10-year groups present in the dataset (global, consistent order)
age_lookup <- pop %>%
  mutate(
    grp_start = (Age %/% 10) * 10,
    grp_end   = pmin(grp_start + 9, max_age),
    AgeGroup  = paste0(sprintf("%02d", grp_start), "–", sprintf("%02d", grp_end))
  ) %>%
  distinct(AgeGroup, grp_start) %>%
  arrange(grp_start)

age_levels <- age_lookup$AgeGroup  # consistent levels (youngest -> oldest)

label_hundred_plus <- function(x) sub("^100[–-]100$", "100+", x)

pop_grp <- pop %>%
  mutate(
    grp_start = (Age %/% 10) * 10,
    grp_end   = pmin(grp_start + 9, max_age),
    AgeGroup  = paste0(sprintf("%02d", grp_start), "–", sprintf("%02d", grp_end))
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

# ---------- Palette: soft blue gradient across age groups --------------------- # <<<
# Very light -> mid-light blues to keep the look soft
soft_blues <- colorRampPalette(c("#ECF4FE", "#DCEBFB", "#CBE2F8", "#BAD8F4",
                                 "#A8CFF0", "#96C5EC", "#84BBE8", "#72B1E4",
                                 "#5FA7E0", "#4D9EDC", "#3A94D8", "#2F8BD4"))(length(age_levels))
names(soft_blues) <- age_levels

# ---------- Axes limits/breaks (unchanged) ----------
x_max <- 55000000
x_breaks <- seq(0, x_max, by = 10000000)

# ---------- 4) Horizontal bar "pyramids" by 10-year age groups ----------
years <- sort(unique(pop_grp$Year))
n_groups <- length(age_levels)

for (yr in years) {
  df_y <- pop_grp %>%
    filter(Year == yr) %>%
    arrange(grp_start) %>%
    mutate(AgeGroup = factor(AgeGroup, levels = age_levels))  # lock global order

  # Fixed height for consistent frame geometry
  h <- max(6, n_groups * 0.35)

  # Label placement
  thr_prop     <- 0.06
  label_offset <- 0.01 * x_max

  df_inside  <- df_y %>%
    filter(Population > 0, Population >= thr_prop * x_max) %>%
    mutate(label = scales::comma(Population),
           x_lab = Population / 2)

  df_outside <- df_y %>%
    filter(Population > 0, Population <  thr_prop * x_max) %>%
    mutate(label = scales::comma(Population),
           x_lab = pmin(Population + label_offset, x_max))

  p_pyr <- ggplot(df_y, aes(y = AgeGroup, x = Population, fill = AgeGroup)) +
    geom_col(width = 0.8) +
    # Labels with enough room: centered, black
    geom_text(
      data = df_inside,
      aes(x = x_lab, label = label),
      color = "black", size = 3.8, vjust = 0.5
    ) +
    # Small bars: outside labels in dark ink
    geom_text(
      data = df_outside,
      aes(x = x_lab, label = label),
      color = soft_ink, size = 3.8, hjust = 0, vjust = 0.5
    ) +
    scale_fill_manual(values = soft_blues, limits = age_levels, drop = FALSE) +           # <<<
    scale_x_continuous(
      labels = scales::comma,
      limits = c(0, x_max),
      breaks = x_breaks,
      expand = c(0, 0)
    ) +
    scale_y_discrete(labels = label_hundred_plus) +
    labs(
      title = paste0("Population by 10-year age groups — ", yr),
      x = "Population", y = "Age group"
    ) +
    theme_soft_blue(base_size = 16) +                                                     # <<<
    theme(
      legend.position = "none",
      plot.title = element_text(color = soft_ink, face = "bold", size = 18, margin = margin(b = 8))
    )

  # Save PNG frames with crisp AA
  frame_path <- file.path("visual/pop_pyramide", sprintf("pyramide_%d.png", yr))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(frame_path, width = 9, height = h, units = "in", res = 300)            # <<<
    print(p_pyr)
    dev.off()
  } else {
    ggsave(frame_path, p_pyr, width = 9, height = h, dpi = 300, device = "cairo_png")     # <<<
  }
}

cat("Saved:\n - visual/pop_total_evolution.png\n -", length(years), "horizontal bar pyramids in visual/pop_pyramide/\n")

# ---------- 5) GIF assembly (no explicit compression) -------------------------
library(magick)
library(stringr)

in_dir  <- "visual/pop_pyramide"
out_gif <- file.path(in_dir, "pyramide_years.gif")

files <- list.files(in_dir, pattern = "^pyramide_\\d{4}\\.png$", full.names = TRUE)
stopifnot("No pyramid images found." = length(files) > 0)

years <- as.integer(str_extract(basename(files), "\\d{4}"))
files <- files[order(years)]

imgs <- image_read(files)

# Align frames to largest W/H, center, white background
info <- image_info(imgs)
target_w <- max(info$width, na.rm = TRUE)
target_h <- max(info$height, na.rm = TRUE)

imgs_aligned <- image_extent(
  imgs,
  geometry = sprintf("%dx%d", target_w, target_h),
  gravity  = "center",
  color    = "white"
)

# IMPORTANT: do NOT downscale or quantize (avoid extra compression).            # <<<
# imgs_aligned <- image_scale("1200x")        # removed
# imgs_aligned <- image_quantize(max = 128)   # removed

# Smooth animation, 1 fps. Consider a slight delay on last frame for emphasis:
anim <- image_animate(imgs_aligned, fps = 1, loop = 0)                          

# Write GIF. (No quality param for GIF; magick will choose a palette internally.)
image_write(anim, out_gif)
message("GIF written to: ", out_gif)
