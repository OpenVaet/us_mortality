# Infant mortality (deaths per 1,000 live births), United States, 1995–2023
# Source: NCHS (values as provided)

years <- 1995:2023
infant_rate <- c(
  7.57, # 1995
  7.30, # 1996
  7.21, # 1997
  7.19, # 1998
  7.04, # 1999
  6.89, # 2000
  6.84, # 2001
  6.95, # 2002
  6.84, # 2003
  6.78, # 2004
  6.86, # 2005
  6.68, # 2006
  6.75, # 2007
  6.61, # 2008
  6.39, # 2009
  6.14, # 2010
  6.07, # 2011
  5.98, # 2012
  5.96, # 2013
  5.82, # 2014
  5.90, # 2015
  5.87, # 2016
  5.79, # 2017
  5.67, # 2018
  5.58, # 2019
  5.42, # 2020
  5.44, # 2021
  5.61, # 2022
  5.61  # 2023
)

stopifnot(length(years) == length(infant_rate))

# ----- Plot: rate vs year -----
op <- par(mar = c(4.2, 4.5, 3.2, 1) + 0.1)
plot(
  years, infant_rate,
  type = "b", pch = 19, lwd = 2,
  xlab = "Year",
  ylab = "Infant deaths per 1,000 live births",
  main = "U.S. Infant Mortality Rate (1995–2023)\nTrend fitted on 2000–2020 and extended to 2023",
ylim=c(4,8)
)
grid(col = "grey85")

# ----- Linear trend using 2000–2020 (clean, df-based) -----
df <- data.frame(year = years, rate = infant_rate)
fit <- lm(rate ~ year, data = subset(df, year >= 2000 & year <= 2020))

# Predict from 2000 through 2023 using that model
pred_years <- 2000:2023
pred <- predict(fit, newdata = data.frame(year = pred_years))

# Draw the fitted line: solid over the fit window (2000–2020), dashed for extrapolation (2021–2023)
lines(2000:2020, pred[1:21], lwd = 2)          # fitted range
lines(2020:2023, pred[21:24], lwd = 2, lty = 2) # continue to 2023

# Visual aid: mark the end of the fitting window at 2020
abline(v = 2020, lty = 3)

# Legend with slope info
slope_per_year <- coef(fit)[2]
legend(
  "topright",
  inset = 0.02,
  bty = "n",
  lty = c(1, 2, 3), lwd = c(2, 2, 1), pch = c(19, NA, NA),
  legend = c(
    "Observed (1995–2023)",
    sprintf("Trend (2000–2020 ? 2023), slope = %.3f/yr", slope_per_year),
    "Fit window ends (2020)"
  )
)




# --- Build tidy df, fit 2000–2020, predict to 2023 ---
df <- data.frame(year = years, rate = infant_rate)

fit <- lm(rate ~ year, data = subset(df, year >= 2000 & year <= 2020))

pred_years <- 2000:2023
pred <- predict(fit, newdata = data.frame(year = pred_years))
df2 <- merge(df, data.frame(year = pred_years, pred_rate = pred), by = "year", all.x = TRUE)

# --- Calculate excess (obs - pred) ---
df2$excess <- df2$rate - df2$pred_rate

# --- Compute control limits based on 2000–2020 excess ---
baseline <- subset(df2, year >= 2000 & year <= 2020)
mu <- mean(baseline$excess)
sd_val <- sd(baseline$excess)

cl_2up  <- mu + 2 * sd_val
cl_2dn  <- mu - 2 * sd_val
cl_3up  <- mu + 3 * sd_val
cl_3dn  <- mu - 3 * sd_val

# --- Plot excess (2000–2023) with control lines ---
op <- par(mar = c(4.2, 4.6, 3, 0.8) + 0.1)
plot(
  df2$year, df2$excess,
  type = "b", pch = 19, lwd = 2,
  xlab = "Year",
  ylab = "Excess infant deaths per 1,000 live births",
  main = "Excess Infant Mortality vs Linear Trend (2000–2023)"
)
abline(h = mu, lty = 1, col = "blue", lwd = 2)
abline(h = c(cl_2up, cl_2dn), lty = 2, col = "red", lwd = 2)
abline(h = c(cl_3up, cl_3dn), lty = 3, col = "darkred", lwd = 2)
abline(h = 0, lty = 3, col = "grey")

legend(
  "topleft", bty = "n",
  legend = c(
    sprintf("Mean = %.3f", mu),
    sprintf("±2 SD = %.3f", sd_val * 2),
    sprintf("±3 SD = %.3f", sd_val * 3)
  ),
  lty = c(1, 2, 3), col = c("blue", "red", "darkred"), lwd = 2
)
par(op)

