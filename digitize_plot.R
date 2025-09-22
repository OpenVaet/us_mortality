# Enhanced digitization script with improved accuracy and manual correction support
# Packages: install.packages(c("png","tidyverse","zoo"))

library(png)
library(tidyverse)
library(zoo)

# -------------------- 1) Settings --------------------
img_path  <- "data/tes_fakes/Vaccinal-Generation-Excess-Mortality-6-resized.png"

y_min <- -50
y_max <- 210

date_start <- as.Date("2019-01-01")
date_end   <- as.Date("2024-12-31")
weeks <- seq(date_start, date_end, by = "week")

# Enhanced color detection parameters
target_rgb <- col2rgb("#4D79C7") / 255
thr_rgb    <- 0.25  # Slightly tighter threshold for better precision
dx         <- 3     # Slightly wider search window
dy         <- 5     # Vertical search window to handle line thickness

# -------------------- 2) Load image & setup --------------------
img <- readPNG(img_path)
if (length(dim(img)) < 3 || dim(img)[3] < 3) stop("Image must be RGB/RGBA")

h <- dim(img)[1]
w <- dim(img)[2]

# ROI = whole image
x_left <- 1L; x_right <- w
y_top  <- 1L; y_bottom <- h
roi_w  <- x_right - x_left + 1L
roi_h  <- y_bottom - y_top + 1L

# Extract channels
R <- img[y_top:y_bottom, x_left:x_right, 1]
G <- img[y_top:y_bottom, x_left:x_right, 2]
B <- img[y_top:y_bottom, x_left:x_right, 3]

# Distance matrix to target color
dist_mat <- sqrt((R - target_rgb[1])^2 + (G - target_rgb[2])^2 + (B - target_rgb[3])^2)

# -------------------- 3) Enhanced line detection --------------------
# Create binary mask for blue line pixels
line_mask <- dist_mat <= thr_rgb

# Simple morphological cleaning using base R
# Function to apply a convolution-like operation
apply_kernel <- function(mat, kernel) {
  kr <- nrow(kernel)
  kc <- ncol(kernel)
  mr <- nrow(mat)
  mc <- ncol(mat)
  
  # Pad the matrix
  pad_r <- floor(kr/2)
  pad_c <- floor(kc/2)
  
  result <- matrix(0, mr, mc)
  
  for(i in 1:mr) {
    for(j in 1:mc) {
      sum_val <- 0
      sum_weight <- 0
      
      for(ki in 1:kr) {
        for(kj in 1:kc) {
          ri <- i + ki - pad_r - 1
          cj <- j + kj - pad_c - 1
          
          if(ri >= 1 && ri <= mr && cj >= 1 && cj <= mc) {
            sum_val <- sum_val + mat[ri, cj] * kernel[ki, kj]
            sum_weight <- sum_weight + kernel[ki, kj]
          }
        }
      }
      
      result[i, j] <- if(sum_weight > 0) sum_val / sum_weight else 0
    }
  }
  
  return(result)
}

# Apply simple smoothing to clean up the mask
clean_mask <- line_mask
kernel <- matrix(1, 3, 3) / 9

# Only do cleaning if the mask is not too large (for performance)
if(prod(dim(clean_mask)) < 1e6) {
  clean_mask <- apply_kernel(as.numeric(clean_mask), kernel) > 0.5
} else {
  message("Skipping morphological cleaning due to image size")
}

# -------------------- 4) Smart point extraction --------------------
x_grid <- seq(1, roi_w, length.out = length(weeks))
x_idx  <- round(x_grid)

clamp <- function(v, lo, hi) pmin(pmax(v, lo), hi)

values <- rep(NA_real_, length(weeks))
y_px   <- rep(NA_integer_, length(weeks))
confidence <- rep(0, length(weeks))  # Track detection confidence

for (i in seq_along(x_idx)) {
  cx <- x_idx[i]
  cols <- clamp(seq(cx - dx, cx + dx), 1L, roi_w)
  
  # Method 1: Find weighted centroid of blue pixels in column strip
  blue_pixels <- which(clean_mask[, cols, drop = FALSE] > 0, arr.ind = TRUE)
  
  if (nrow(blue_pixels) > 0) {
    # Weight by inverse distance for better accuracy
    pixel_coords <- cbind(blue_pixels[,1], cols[blue_pixels[,2]])
    weights <- 1 / (dist_mat[pixel_coords] + 0.01)
    weighted_y <- weighted.mean(blue_pixels[,1], weights)
    
    y_ri <- round(weighted_y)
    y_px[i] <- y_ri
    confidence[i] <- min(1, length(blue_pixels) / (length(cols) * 10))  # Normalized confidence
    
    # Convert to data value
    y_rel <- (roi_h - y_ri) / (roi_h - 1)
    values[i] <- y_min + y_rel * (y_max - y_min)
  } else {
    # Fallback: original minimum distance method
    d_sub <- dist_mat[, cols, drop = FALSE]
    which_min <- which(d_sub == min(d_sub, na.rm = TRUE), arr.ind = TRUE)[1, ]
    best_dist <- d_sub[which_min[1], which_min[2]]
    
    if (is.finite(best_dist) && best_dist <= thr_rgb * 1.5) {  # Slightly relaxed threshold
      y_ri <- which_min[1]
      y_px[i] <- y_ri
      confidence[i] <- max(0, 1 - best_dist/thr_rgb) * 0.5  # Lower confidence for fallback
      
      y_rel <- (roi_h - y_ri) / (roi_h - 1)
      values[i] <- y_min + y_rel * (y_max - y_min)
    }
  }
}

# -------------------- 5) Outlier detection and smoothing --------------------
# Detect outliers using local median
window_size <- 11

# Create local medians with proper handling
if(length(values) >= window_size) {
  local_medians <- rollapply(values, width = window_size, FUN = median, 
                            na.rm = TRUE, partial = TRUE, align = "center")
} else {
  local_medians <- rep(median(values, na.rm = TRUE), length(values))
}

deviations <- abs(values - local_medians)
mad_scale <- median(deviations, na.rm = TRUE) * 1.4826  # Robust scale estimate

# Define outliers
if(is.finite(mad_scale) && mad_scale > 0) {
  outliers <- !is.na(deviations) & (deviations > 3 * mad_scale)
} else {
  outliers <- rep(FALSE, length(values))
}

# Mark low-confidence points as potential outliers
outliers[confidence < 0.3] <- TRUE

# Fill gaps with interpolation
values_clean <- values
values_clean[outliers | is.na(values_clean)] <- NA

if (sum(!is.na(values_clean)) > 10) {
  # Use spline interpolation for smoother results
  valid_idx <- which(!is.na(values_clean))
  if (length(valid_idx) > 3) {
    spl <- smooth.spline(valid_idx, values_clean[valid_idx], spar = 0.6)
    values_interp <- predict(spl, seq_along(values))$y
    values[is.na(values) | outliers] <- values_interp[is.na(values) | outliers]
  }
} else {
  # Simple linear interpolation as fallback
  values <- na.approx(values, na.rm = FALSE)
  values <- na.locf(values, na.rm = FALSE)
  values <- na.locf(values, fromLast = TRUE, na.rm = FALSE)
}

# Apply adaptive smoothing (less smoothing where confidence is high)
if (length(values) >= 7) {
  smooth_values <- values
  for (i in 4:(length(values)-3)) {
    if (!is.na(confidence[i])) {
      if (confidence[i] < 0.7) {
        # More smoothing for low-confidence points
        window_vals <- values[(i-3):(i+3)]
        if(sum(!is.na(window_vals)) > 0) {
          smooth_values[i] <- median(window_vals, na.rm = TRUE)
        }
      } else if (confidence[i] < 0.9) {
        # Light smoothing for medium confidence
        window_vals <- values[(i-1):(i+1)]
        if(sum(!is.na(window_vals)) > 0) {
          smooth_values[i] <- median(window_vals, na.rm = TRUE)
        }
      }
    }
  }
  values <- smooth_values
}

# -------------------- 6) Load or create manual corrections --------------------
corrections_file <- "data/tes_fakes/points_for_manual_correction.csv"

# Check if corrections file exists
if(file.exists(corrections_file)) {
  cat(sprintf("Loading existing corrections from: %s\n", corrections_file))
  
  # Create initial dataframe
  out <- tibble(
    week = weeks,
    excess = round(values, 3),
    confidence = round(confidence, 2),
    is_outlier = outliers,
    needs_review = outliers | (confidence < 0.5)
  )
  
  # Load corrections
  corrections <- read_csv(corrections_file, show_col_types = FALSE)
  
  # Apply corrections
  corrections_applied <- 0
  for(i in 1:nrow(corrections)) {
    idx <- which(out$week == corrections$week[i])
    if(length(idx) == 1) {
      # Use corrected_value if available, otherwise use suggested_value
      if(!is.na(corrections$corrected_value[i])) {
        out$excess[idx] <- corrections$corrected_value[i]
        out$confidence[idx] <- 1.0  # Mark as manually verified
        out$is_outlier[idx] <- FALSE
        out$needs_review[idx] <- FALSE
        corrections_applied <- corrections_applied + 1
      } else if(!is.na(corrections$suggested_value[i])) {
        out$excess[idx] <- corrections$suggested_value[i]
      }
    }
  }
  
  cat(sprintf("Applied %d manual corrections\n", corrections_applied))
  
} else {
  cat(sprintf("No corrections file found. Creating new template at: %s\n", corrections_file))
  
  # Create initial dataframe
  out <- tibble(
    week = weeks,
    excess = round(values, 3),
    confidence = round(confidence, 2),
    is_outlier = outliers,
    needs_review = outliers | (confidence < 0.5)
  )
  
  # Create template with ALL points
  all_points <- data.frame(
    week = out$week,
    suggested_value = out$excess,
    corrected_value = NA_real_,
    confidence = out$confidence,
    needs_review = out$needs_review,
    notes = ifelse(out$needs_review,
                  ifelse(out$is_outlier, "Detected as outlier",
                        ifelse(out$confidence < 0.3, "Very low confidence",
                              ifelse(out$confidence < 0.5, "Low confidence",
                                     "Review recommended"))),
                  "OK")
  )
  
  # Ensure directory exists
  dir.create(dirname(corrections_file), showWarnings = FALSE, recursive = TRUE)
  
  # Save the template
  write_csv(all_points, corrections_file)
  cat(sprintf("Created correction template with %d points\n", nrow(all_points)))
  cat("Edit 'corrected_value' column for any points that need adjustment, then re-run the script.\n")
}

dir.create("visual", showWarnings = FALSE, recursive = TRUE)
write_csv(out, "visual/digitized_weekly_excess_mortality_enhanced.csv")
message("Saved: visual/digitized_weekly_excess_mortality_enhanced.csv")

# Summary statistics
cat("\n=== Digitization Quality Report ===\n")
cat(sprintf("Total points: %d\n", nrow(out)))
cat(sprintf("High confidence (>0.7): %d\n", sum(out$confidence > 0.7, na.rm = TRUE)))
cat(sprintf("Medium confidence (0.5-0.7): %d\n", sum(out$confidence >= 0.5 & out$confidence <= 0.7, na.rm = TRUE)))
cat(sprintf("Low confidence (<0.5): %d\n", sum(out$confidence < 0.5, na.rm = TRUE)))
cat(sprintf("Detected outliers: %d\n", sum(out$is_outlier, na.rm = TRUE)))
cat(sprintf("Points needing review: %d\n", sum(out$needs_review, na.rm = TRUE)))

# -------------------- 7) Single comprehensive diagnostic plot --------------------
# Convert values back to pixels for display
y_rel <- (out$excess - y_min) / (y_max - y_min)
y_disp <- y_bottom - y_rel * (roi_h - 1)
x_disp <- x_left + x_grid - 1

# Color code by confidence and review status
point_colors <- ifelse(out$needs_review, "#FF0000",  # Red for review needed
                      ifelse(out$confidence > 0.7, "#00FF00",  # Green for high confidence
                            ifelse(out$confidence > 0.5, "#FFA500",  # Orange for medium
                                   "#FF69B4")))  # Pink for low confidence

# Create the plot with better margins
op <- par(mar = c(2, 2, 3, 8))  # Increase right margin for legend

plot(NA, NA, xlim = c(0, w), ylim = c(h, 0), xaxs = "i", yaxs = "i",
     xlab = "", ylab = "", axes = FALSE, asp = 1,
     main = "Digitized Points Over Original Plot - Check Red Points for Errors")

# Draw the original image
rasterImage(img, 0, h, w, 0)

# Draw connecting lines between points (helps visualize continuity)
lines(x_disp, y_disp, col = rgb(0, 0, 0, 0.3), lwd = 1)

# Draw the points with varying sizes based on confidence
point_sizes <- 0.4 + (1 - out$confidence) * 0.8  # Larger points for lower confidence
points(x_disp, y_disp, pch = 16, cex = point_sizes, col = point_colors)

# Add circles around points needing review for extra visibility
if(any(out$needs_review)) {
  review_idx <- which(out$needs_review)
  points(x_disp[review_idx], y_disp[review_idx], 
         pch = 1, cex = 1.2, col = "red", lwd = 2)
}

# Add date labels for orange and red points (medium/low confidence or needs review)
label_idx <- which(out$confidence <= 0.7 | out$needs_review)
if(length(label_idx) > 0) {
  # Offset labels alternately above and below to avoid overlap
  for(i in seq_along(label_idx)) {
    idx <- label_idx[i]
    # Alternate placement above/below the point
    y_offset <- if(i %% 2 == 0) -15 else 15
    
    # Color the text to match the point
    text_color <- ifelse(out$needs_review[idx], "#FF0000",
                        ifelse(out$confidence[idx] <= 0.5, "#FF69B4", "#FFA500"))
    
    text(x_disp[idx], y_disp[idx] + y_offset, 
         format(out$week[idx], "%Y-%m-%d"),
         cex = 0.5, col = text_color, srt = 45, xpd = TRUE)
  }
}

# Add legend outside the plot area
legend(x = w * 1.02, y = h * 0.1, 
       legend = c("High conf (>0.7)", "Med conf (0.5-0.7)", 
                  "Low conf (<0.5)", "Needs review", "", 
                  "○ Circle = Review", "Size ∝ 1/confidence"),
       col = c("#00FF00", "#FFA500", "#FF69B4", "#FF0000", NA, "red", "black"),
       pch = c(16, 16, 16, 16, NA, 1, 16),
       pt.cex = c(1, 1, 1, 1, 1, 1.2, 0.8),
       cex = 0.8, 
       bg = rgb(1, 1, 1, 0.9),
       box.lty = 1,
       xpd = TRUE)  # Allow drawing outside plot region

# Add date labels for reference
n_labels <- 6
label_idx <- round(seq(1, length(weeks), length.out = n_labels))
for(i in label_idx) {
  text(x_disp[i], h + 10, format(weeks[i], "%Y-%m"), 
       cex = 0.7, xpd = TRUE, srt = 0)
}

par(op)

# Print problematic weeks for easy reference
if(sum(out$needs_review) > 0) {
  cat("\n=== Points Needing Review ===\n")
  review_data <- out[out$needs_review, c("week", "excess", "confidence")]
  print(as.data.frame(review_data), row.names = FALSE)
}

# -------------------- 8) Update correction file if needed --------------------
# Update the correction file with current values
if(file.exists(corrections_file)) {
  # Read existing file
  corrections <- read_csv(corrections_file, show_col_types = FALSE)
  
  # Update suggested values with current digitization
  for(i in 1:nrow(out)) {
    idx <- which(corrections$week == out$week[i])
    if(length(idx) == 1) {
      # Only update if no manual correction has been made
      if(is.na(corrections$corrected_value[idx])) {
        corrections$suggested_value[idx] <- out$excess[i]
        corrections$confidence[idx] <- out$confidence[i]
        corrections$needs_review[idx] <- out$needs_review[i]
        corrections$notes[idx] <- ifelse(out$needs_review[i],
                                        ifelse(out$is_outlier[i], "Detected as outlier",
                                              ifelse(out$confidence[i] < 0.3, "Very low confidence",
                                                    ifelse(out$confidence[i] < 0.5, "Low confidence",
                                                           "Review recommended"))),
                                        "OK")
      }
    }
  }
  
  # Save updated file
  write_csv(corrections, corrections_file)
  cat(sprintf("Updated correction file: %s\n", corrections_file))
  
  # Report on correction status
  n_corrected <- sum(!is.na(corrections$corrected_value))
  n_needs_review <- sum(corrections$needs_review & is.na(corrections$corrected_value))
  
  if(n_needs_review > 0) {
    cat(sprintf("Status: %d points manually corrected, %d still need review\n", 
                n_corrected, n_needs_review))
    
    # Show points still needing review
    review_pending <- corrections[corrections$needs_review & is.na(corrections$corrected_value), 
                                 c("week", "suggested_value", "confidence", "notes")]
    cat("\nPoints still needing manual review:\n")
    print(as.data.frame(review_pending), row.names = FALSE)
  } else if(n_corrected > 0) {
    cat(sprintf("All issues resolved! %d points were manually corrected.\n", n_corrected))
  } else {
    cat("No manual corrections needed - digitization looks good!\n")
  }
}

# -------------------- 9) Save final output --------------------
# Save the corrected data to the standard output location
write_csv(out, "data/tes_fakes/digitized_weekly_excess_mortality_enhanced.csv")
cat(sprintf("\nFinal data saved to: data/tes_fakes/digitized_weekly_excess_mortality_enhanced.csv\n"))

# Also create a simplified version with just week and rate
out_simple <- out[, c("week", "excess")]
write_csv(out_simple, "data/tes_fakes/digitized_weekly_excess_mortality.csv")
cat(sprintf("Simple version saved to: data/tes_fakes/digitized_weekly_excess_mortality.csv\n"))

# -------------------- 10) Simple comparison: original (image) vs current (green) ----------
# Load the current (simple) series saved in Step 9
cur <- readr::read_csv("data/tes_fakes/digitized_weekly_excess_mortality.csv", show_col_types = FALSE)

# Align to the weekly grid used for digitization
cmp <- tibble(week = weeks) %>% left_join(cur, by = "week")

# Convert values back to image pixel rows
y_rel_c  <- (cmp$excess - y_min) / (y_max - y_min)           # 0..1 bottom->top
y_disp_c <- y_bottom - y_rel_c * (roi_h - 1)                         # pixel rows (top origin)
x_disp_c <- x_left + seq(1, roi_w, length.out = nrow(cmp)) - 1       # pixel cols

# Keep only finite points
ok <- is.finite(y_disp_c) & is.finite(x_disp_c)

# Draw overlay and save
png("visual/compare_original_vs_current.png", width = w, height = h,
    type = "cairo-png", antialias = "subpixel")
op <- par(mar = c(2, 2, 3, 2))
plot(NA, NA, xlim = c(0, w), ylim = c(h, 0), xaxs = "i", yaxs = "i",
     xlab = "", ylab = "", axes = FALSE, asp = 1,
     main = "Original image vs current digitization (green points)")
rasterImage(img, 0, h, w, 0)

if (any(ok)) {
  # Colors
  col_green <- "#00B050"
  
  # 1) Line with halo: thick white underlay, then green on top
  lines(x_disp_c[ok], y_disp_c[ok],
        col = grDevices::adjustcolor("white", alpha.f = 0.95),
        lwd = 4, lend = "round", ljoin = "round")
  lines(x_disp_c[ok], y_disp_c[ok],
        col = grDevices::adjustcolor(col_green, alpha.f = 0.95),
        lwd = 2.2, lend = "round", ljoin = "round")
  
  # 2) Points with halo: big white, then smaller green on top
  points(x_disp_c[ok], y_disp_c[ok],
         pch = 16, cex = 1.3,
         col = grDevices::adjustcolor("white", alpha.f = 0.98))
  points(x_disp_c[ok], y_disp_c[ok],
         pch = 16, cex = 0.85,
         col = col_green)
}

par(op)
dev.off()
cat("Overlay saved to: visual/compare_original_vs_current.png\n")

