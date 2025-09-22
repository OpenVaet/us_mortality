library(data.table)
library(ggplot2)
library(MMWRweek)

# ---- helpers ----

# Centered moving average with b weeks before and f weeks after the index
ma <- function(x, b = 1, f = b) {
  # window size = b + f + 1
  pad <- c(rep(NA_real_, b), x, rep(NA_real_, f))
  # embed builds a matrix of overlapping windows; rowMeans with na.rm handles edges
  out <- rowMeans(embed(pad, b + f + 1), na.rm = TRUE)
  x[] <- out
  x
}

# ---- load & smooth ----
t <- fread("https://sars2.net/f/wondervaccinial0to4.csv")

# Smooth observed deaths with a 6-week centered MA: 3 before, 2 after (b=3,f=2)
t[, dead := ma(dead, b = 3, f = 2)]
t <- na.omit(t)

# Convert MMWR year/week to a Date (mid-week = 4th day)
t[, date := MMWRweek2Date(year, week, 4)]

# ---- pre-2020 slices ----
pre <- t[year < 2020]

# Weekly baseline shape from 2018–2019: mean by week number
wk_means <- pre[, .(base = mean(dead), base2 = mean(dead)), by = week]

# Annual trend (all weeks) on 2018–2019
yr_trend <- pre[, .(mean_dead = mean(dead)), by = year]
slope_all <- predict(lm(mean_dead ~ year, data = yr_trend),
                     newdata = data.frame(year = 2018:2025))

# Annual trend (weeks 15–35 only) on 2018–2019
yr_trend_1535 <- pre[week %in% 15:35, .(mean_dead = mean(dead)), by = year]
slope_1535 <- predict(lm(mean_dead ~ year, data = yr_trend_1535),
                      newdata = data.frame(year = 2018:2025))

# Normalize slopes so that 2018–2019 average = 1 (so we scale weekly shapes cleanly)
scale_all  <- slope_all  / mean(slope_all[1:2])
scale_1535 <- slope_1535 / mean(slope_1535[1:2])

# Merge weekly shape onto full table, then scale by year trend
t <- merge(wk_means, t, by = "week")
t[, base  := base  * scale_all [ factor(year, levels = 2018:2025) ]]
t[, base2 := base2 * scale_1535[ factor(year, levels = 2018:2025) ]]

# ---- hybrid baseline: linear time trend + seasonal weekly residuals (2018–2019) ----
fit_pre   <- lm(dead ~ date, data = pre)
t[, base3 := predict(fit_pre, newdata = t)]

# Add weekly seasonal adjustment: mean residual by week in 2018–2019
pre[, resid := dead - predict(fit_pre, newdata = pre)]
wk_adj <- pre[, .(adj = mean(resid)), by = week]
t <- merge(t, wk_adj, by = "week", all.x = TRUE)
t[, base3 := base3 + adj]
t[, adj := NULL]

# ---- flat average line (2018–2019 overall mean) ----
t[, ave := pre[, mean(dead)]]

# ---- construct plotting data ----
labels <- c(
  "Actual deaths",
  "2018–2019 linear regression of deaths on weeks 15–35",
  "Ethical Skeptic's baseline reverse engineered via WebPlotDigitizer",
  "2018–2019 average deaths"
)

# IMPORTANT: Ethical Skeptic line is dead - excess (excess comes from CSV)
p_deaths <- t[, .(
  x = date,
  y = c(dead, base2, dead - excess, ave),
  z = factor(rep(labels, each = .N), levels = labels)
)]

p_deaths[, facet := "Deaths"]

# Excess % vs. each baseline (except "Actual deaths")
actual_by_date <- p_deaths[z == "Actual deaths", .(x, actual = y)]
p_rel <- merge(p_deaths[z != "Actual deaths"], actual_by_date, by = "x")
p_rel[, `:=`(y = (actual / y - 1) * 100,
             facet = "Excess percentage of deaths")]
p_rel[, actual := NULL]

p <- rbindlist(list(p_deaths, p_rel), use.names = TRUE)
p[, facet := factor(facet, levels = c("Deaths", "Excess percentage of deaths"))]

# ---- axes, limits, and panel frames ----
xstart <- as.Date("2018-01-01")
xend   <- as.Date("2025-05-01")

xbreaks <- seq(xstart, xend, by = "6 months")
xlabels <- ifelse(lubridate::month(xbreaks) == 7, lubridate::year(xbreaks), "")

ylim <- p[, {
  rng <- scales::expand_range(range(y, na.rm = TRUE), mul = 0.03)
  .(ymin = rng[1], ymax = rng[2])
}, by = facet]

# ---- plot ----
gg <- ggplot(p) +
  facet_wrap(~ facet, dir = "v", scales = "free") +
  geom_vline(xintercept = seq(xstart, xend, by = "1 year"),
             color = "gray90", linewidth = 0.4) +
  geom_segment(data = p[.N], aes(x = xstart, xend = xend, y = 0, yend = 0),
               linewidth = 0.4, color = "gray75", inherit.aes = FALSE) +
  geom_rect(data = ylim,
            aes(ymin = ymin, ymax = ymax),
            xmin = xstart, xmax = xend,
            fill = NA, color = "gray72", linewidth = 0.4) +
  geom_line(aes(x, y, color = z), linewidth = 0.5) +
  geom_label(data = ylim,
             aes(x = xstart + 50, y = (ymin + ymax) / 2, label = facet),
             hjust = 0, label.r = unit(0, "pt"),
             label.padding = unit(5, "pt"),
             label.size = 0.4, color = "gray75", size = 3.8) +
  geom_label(data = ylim,
             aes(x = xstart + 50, y = (ymin + ymax) / 2, label = facet),
             hjust = 0, label.r = unit(0, "pt"),
             label.padding = unit(5, "pt"),
             label.size = 0, fill = NA, size = 3.8) +
  labs(
    x = NULL, y = NULL,
    title = "CDC WONDER, ages 0–4: Deaths with underlying cause A–R or 999–999\n(natural causes and R, excluding COVID and external causes)"
  ) +
  scale_x_continuous(limits = c(xstart, xend),
                     breaks = xbreaks, labels = xlabels) +
  scale_y_continuous(
    breaks = function(x) pretty(x, 7),
    labels = function(x) if (max(x, na.rm = TRUE) < 100) paste0(x, "%") else x
  ) +
  scale_color_manual(values = c("black", "#8888ff", "#aaaa00", hsv(3/36, .7, .7))) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme(
    axis.text = element_text(size = 11, color = "gray40"),
    axis.ticks = element_line(linewidth = 0.4, color = "gray75"),
    axis.ticks.length.x = unit(0, "pt"),
    axis.ticks.length.y = unit(4, "pt"),
    legend.background = element_blank(),
    legend.box.spacing = unit(0, "pt"),
    legend.direction = "vertical",
    legend.key = element_blank(),
    legend.key.height = unit(11, "pt"),
    legend.key.width  = unit(23, "pt"),
    legend.margin = margin(b = 4),
    legend.position = "top",
    legend.spacing.x = unit(2, "pt"),
    legend.spacing.y = unit(0, "pt"),
    legend.text = element_text(size = 11, vjust = .5),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.4),
    panel.spacing = unit(2, "pt"),
    plot.title = element_text(size = 11, face = 2, hjust = .5, margin = margin(b = 2)),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
gg

ggsave("1.png", plot = gg, width = 5.6, height = 4.8, dpi = 1200)
