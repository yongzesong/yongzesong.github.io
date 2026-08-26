# =============================================================================
# 10-prepare-data.R — load the packaged Zn data, log-transform, screen outliers
#
# This is step 1 of the paper's five-step case study (Sect. 3.2): "The trace
# element data were transformed using a logarithm function to avoid impacts on
# data distributions since trace element data are skewed distributed".
#
# Outputs: data/zn-samples.csv, data/grid-covariates.csv  (verbatim copies)
#          data/derived/samples.rds                       (working sample set)
#          results/response-summary.csv                   (paper Table 1 shape)
#          figs/fig01-response-distribution.{png,pdf}     (paper Fig. 4 shape)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geosimilarity", "the GOS method and its bundled data")

log_head("Step 1/5  Prepare data")

# -- the two packaged tables --------------------------------------------------
# Both live inside geosimilarity, so this pipeline never touches the network.
zn   <- as.data.frame(geosimilarity::zn)
grid <- as.data.frame(geosimilarity::grid)
log_info("zn:   %d samples x %d columns", nrow(zn), ncol(zn))
log_info("grid: %d prediction cells x %d columns", nrow(grid), ncol(grid))

missing_cov <- setdiff(CANDIDATES, intersect(names(zn), names(grid)))
if (length(missing_cov))
  stop("covariate(s) absent from zn or grid: ", paste(missing_cov, collapse = ", "))

# Verbatim CSV copies, so a reader can inspect the inputs without R.
write_result(zn, F_ZN_CSV)
write_result(grid, F_GRID_CSV)

# -- log transform ------------------------------------------------------------
if (LOG_TRANSFORM) {
  if (min(zn[[RESPONSE]], na.rm = TRUE) <= 0)
    stop("cannot log-transform ", RESPONSE, ": non-positive values present")
  zn[[RESPONSE_LOG]] <- log(zn[[RESPONSE]])
} else {
  zn[[RESPONSE_LOG]] <- zn[[RESPONSE]]
}

# -- outlier screening --------------------------------------------------------
# removeoutlier() returns the row numbers beyond coef x IQR of the transformed
# response. The paper keeps genuinely high trace-element values because they
# may mark mineral deposits, which is why the screen is loose (coef = 2.5) and
# is applied after the log transform rather than before it.
out_idx <- geosimilarity::removeoutlier(zn[[RESPONSE_LOG]], coef = OUTLIER_COEF)
d <- if (length(out_idx)) zn[-out_idx, ] else zn
rownames(d) <- NULL
log_info("outlier screen (coef = %.1f): %d of %d rows dropped, %d retained",
         OUTLIER_COEF, length(out_idx), nrow(zn), nrow(d))

# `all_samples` keeps the unscreened 894 rows as well. The paper deliberately
# retains high trace-element values ("the high values may indicate the clusters
# of mineral deposits"), so step 50 re-runs its comparison on them to show what
# the screen does to the ranking; only the vignette applies the screen.
saveRDS(list(samples = d, all_samples = zn, grid = grid, dropped = out_idx),
        D_SAMPLES)
log_info("wrote data/derived/samples.rds")

# -- summary of the response, before and after ---------------------------------
# Same columns as the paper's Table 1 (No, Mean, Min, Median, Max, sigma, CV).
summarise_one <- function(label, x) data.frame(
  series = label, n = length(x),
  mean = mean(x), min = min(x), median = stats::median(x), max = max(x),
  sd = stats::sd(x), cv = stats::sd(x) / mean(x), stringsAsFactors = FALSE)

resp_summary <- do.call(rbind, list(
  summarise_one("Zn, ppm (all samples)",            zn[[RESPONSE]]),
  summarise_one("log Zn (all samples)",             zn[[RESPONSE_LOG]]),
  summarise_one("Zn, ppm (outliers screened)",      d[[RESPONSE]]),
  summarise_one("log Zn (outliers screened)",       d[[RESPONSE_LOG]])))
resp_summary[, -(1:2)] <- round(resp_summary[, -(1:2)], 4)
write_result(resp_summary, F_RESPONSE)

log_info("log Zn skewness before screening %.3f, after %.3f",
         mean((zn[[RESPONSE_LOG]] - mean(zn[[RESPONSE_LOG]]))^3) / stats::sd(zn[[RESPONSE_LOG]])^3,
         mean((d[[RESPONSE_LOG]] - mean(d[[RESPONSE_LOG]]))^3) / stats::sd(d[[RESPONSE_LOG]])^3)
log_info("Shapiro-Wilk W on log Zn (screened): %.4f",
         stats::shapiro.test(d[[RESPONSE_LOG]])$statistic)


# -- Fig. 1: what the transform does ------------------------------------------
# The paper's Fig. 4 pairs a histogram with a normal Q-Q plot; here the raw
# response is shown alongside so the reader can see why the transform is done.
draw_fig01 <- function() {
  par(mfrow = c(2, 2), mar = c(3.4, 3.6, 2.0, 0.8), mgp = c(2.1, 0.6, 0),
      cex.axis = 0.8, cex.lab = 0.9, tcl = -0.3)

  hist_density <- function(x, xlab, main, tag) {
    h <- hist(x, breaks = 30, plot = FALSE)
    dn <- stats::density(x)
    plot(h, freq = FALSE, col = ACCENT_SOFT, border = "grey55", main = "",
         xlab = xlab, ylab = "Density", ylim = c(0, max(h$density, dn$y) * 1.08),
         axes = FALSE)
    axis(1); axis(2, las = 1)
    lines(dn, col = ACCENT, lwd = 1.8)
    abline(v = mean(x), col = "grey25", lwd = 1.2, lty = 2)
    box()
    mtext(main, side = 3, line = 0.4, font = 2, cex = 0.85)
    panel_tag(tag)
  }

  qq_panel <- function(x, main, tag) {
    q <- stats::qqnorm(x, plot.it = FALSE)
    plot(q$x, q$y, pch = 16, cex = 0.35, col = "grey40", axes = FALSE,
         xlab = "Theoretical quantiles", ylab = "Sample quantiles")
    axis(1); axis(2, las = 1)
    stats::qqline(x, col = ACCENT, lwd = 1.6)
    box()
    mtext(main, side = 3, line = 0.4, font = 2, cex = 0.85)
    panel_tag(tag)
  }

  hist_density(zn[[RESPONSE]], "Zn (ppm)", "Raw Zn, all 894 samples", "(a)")
  qq_panel(zn[[RESPONSE]], "Raw Zn, normal Q-Q", "(b)")
  hist_density(d[[RESPONSE_LOG]], expression(log(Zn)),
               sprintf("log Zn, %d samples retained", nrow(d)), "(c)")
  qq_panel(d[[RESPONSE_LOG]], "log Zn, normal Q-Q", "(d)")
}
draw_figure("fig01-response-distribution", draw_fig01,
            width = FIG_W_DOUBLE, height = 16)
