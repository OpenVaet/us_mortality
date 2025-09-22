# install.packages(c("data.table","ggplot2")) # if needed
library(data.table)
library(ggplot2)

# ------------------ paths ------------------
in_file   <- "data/tes_fakes/weekly_0_4_deaths_with_tes_fake_excess.csv"
plot_file <- "data/tes_fakes/weekly_0_4_excess_comparison_upto_2024.png"

# ------------------ helpers ------------------
# Centered moving average with b weeks before and f weeks after (window = b+f+1)
ma <- function(x, b = 2, f = b) {
  pad <- c(rep(NA_real_, b), x, rep(NA_real_, f))
  rowMeans(embed(pad, b + f + 1), na.rm = TRUE)
}

# ------------------ load -------------------
dt <- fread(in_file)
# Expect: Year, Week, MMWRWeekCode, Date, Deaths, Excess (TES)
dt[, `:=`(
  Year   = as.integer(Year),
  Week   = as.integer(Week),
  Date   = as.Date(Date),
  Deaths = as.numeric(Deaths),
  Excess = as.numeric(Excess)  # TES series; often NA
)]

# ------------------ restrict to end of 2024 ------------------
cutoff <- as.Date("2024-12-31")
dt <- dt[!is.na(Date) & Date <= cutoff][order(Date)]

# ------------------ 5-week MA on Deaths ------------------
dt[, Deaths_smooth := ma(Deaths, b = 2, f = 2)]

# ------------------ baseline (2018–2019 weekly, using smoothed deaths) ------------------
pre <- dt[Year %in% c(2018, 2019) & !is.na(Deaths_smooth)]
wk_baseline <- pre[, .(baseline_1819_week = mean(Deaths_smooth, na.rm = TRUE)), by = Week]

# attach baseline to all weeks
dt <- merge(dt, wk_baseline, by = "Week", all.x = TRUE, sort = FALSE)

# ------------------ recompute excess (smoothed deaths vs wk baseline) ------------------
dt[, Excess_calc := Deaths_smooth - baseline_1819_week]

# ------------------ plotting data ------------------
# Main line: always show recalculated series where available
rec_layer <- dt[is.finite(Excess_calc), .(Date, Excess_calc)]

# TES overlay: only where TES exists
tes_layer <- dt[is.finite(Excess), .(Date, Excess)]

# x-axis limits
x_start <- min(dt$Date, na.rm = TRUE)
x_end   <- max(dt$Date, na.rm = TRUE)

# ------------------ plot ------------------
col_recalc <- "#1f77b4"  # blue
col_tes    <- "#00B050"  # green

every_n <- 30  # label every 10th point

# labels for TES points (downsampled 1 in 10)
tes_labels <- tes_layer[order(Date)][
  , idx := .I
][ (idx - 1) %% every_n == 0 ][
  , .(Date, Excess, label = scales::number(Excess, accuracy = 0.1))
]

# labels for Recalculated (downsampled 1 in 10)
rec_labels <- rec_layer[order(Date)][
  , idx := .I
][ (idx - 1) %% every_n == 0 ][
  , .(Date, Excess_calc, label = scales::number(Excess_calc, accuracy = 0.1))
]


gg <- ggplot() +
  # zero line
  geom_hline(yintercept = 0, color = "gray80", linewidth = 0.5) +

  # Recalculated line (with subtle halo for readability)
  geom_line(data = rec_layer, aes(Date, Excess_calc),
            color = "white", linewidth = 2.2, lineend = "round") +
  geom_line(data = rec_layer,
            aes(Date, Excess_calc, color = "Recalc (2018–2019 wk avg, 5w MA)"),
            linewidth = 1.1, lineend = "round") +

  # TES overlay (only where present): halo + line + points
  geom_line(data = tes_layer, aes(Date, Excess),
            color = "white", linewidth = 1.8, lineend = "round") +
  geom_line(data = tes_layer,
            aes(Date, Excess, color = "TES (file Excess)"),
            linewidth = 1.05, alpha = 0.95, lineend = "round") +
  # TES points (no legend)
  geom_point(data = tes_layer, aes(Date, Excess),
             color = "white", size = 3.0, stroke = 0, show.legend = FALSE) +
  geom_point(data = tes_layer, aes(Date, Excess, color = "TES (file Excess)"),
             size = 2.0, show.legend = FALSE) +

  # TES labels (no legend)
  geom_text(data = tes_labels, aes(Date, Excess, label = label),
            color = "white", size = 3.7, vjust = -0.9, fontface = "bold",
            lineheight = 0.9, check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = tes_labels,
            aes(Date, Excess, label = label, color = "TES (file Excess)"),
            size = 3.3, vjust = -0.9, lineheight = 0.9,
            check_overlap = TRUE, show.legend = FALSE) +

  # Recalc labels (no legend)
  geom_text(data = rec_labels, aes(Date, Excess_calc, label = label),
            color = "white", size = 3.6, vjust = -0.9, fontface = "bold",
            lineheight = 0.9, check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = rec_labels,
            aes(Date, Excess_calc, label = label, color = "Recalc (2018–2019 wk avg, 5w MA)"),
            size = 3.2, vjust = -0.9, lineheight = 0.9,
            check_overlap = TRUE, show.legend = FALSE) +


  labs(
    x = NULL, y = "Weekly excess deaths",
    title    = "Ages 0–4: Recalculated excess (5w MA) vs. The Ethical Skeptic",
    subtitle = "Scope ≤ 2024-12-31 · Baseline = mean 2018–2019 deaths for the same MMWR week (using smoothed deaths)",
    caption  = "Source: https://wonder.cdc.gov/mcd-icd10-provisional.html · Code at openvaet.substack.com"
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y\n%b",
    limits = c(x_start, x_end),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Recalc (2018–2019 wk avg, 5w MA)" = col_recalc,
      "TES (file Excess)"                 = col_tes
    ),
    name = NULL
  ) +
  # crisper, larger typography for RStudio viewer
  theme(
    text = element_text(family = NULL, size = 13.5),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.6),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12.5, color = "gray25"),
    axis.ticks = element_line(color = "gray70", linewidth = 0.35),
    legend.position = "top",
    legend.text = element_text(size = 12.5),
    legend.spacing.x = unit(4, "pt"),
    legend.key = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 15, face = 2, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 6)),
    plot.caption = element_text(size = 10.5, color = "gray40", hjust = 0.5, margin = margin(t = 8)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 8, r = 10, b = 14, l = 10)
  )

# View in RStudio and save
print(gg)
ggsave(plot_file, gg, width = 10, height = 5, dpi = 200)
cat("Saved plot:", plot_file, "\n")

# ------------------ quick checks ------------------
cat("Rows (<= 2024-12-31):", nrow(dt), "\n")
cat("TES points present:", nrow(tes_layer), "\n")


# ------------------ export tidy comparison CSV ------------------
out_csv <- "data/tes_fakes/real_vs_tes_fake_weekly_excess.csv"

# Keep exact columns & order requested
out_dt <- dt[order(Year, Week),
             .(Year,
               Week,
               MMWRWeekCode,
               Date,
               Deaths,
               TESFakeExcess = Excess,        # TES values from the file
               RealExcess    = Excess_calc)    # our recalculated excess
]

# Make sure the folder exists and write
out_dir <- dirname(out_csv)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fwrite(out_dt, out_csv)

cat("Wrote:", out_csv, "| rows:", nrow(out_dt),
    "| TES present:", sum(!is.na(out_dt$TESFakeExcess)), "\n")

