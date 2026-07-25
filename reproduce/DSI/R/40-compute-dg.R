# =============================================================================
# 40-compute-dg.R — step 4: point-wise degree of geocomplexity
# =============================================================================
# Input:  data/derived/test-residuals.csv
# Output: results/dg-pointwise.csv, results/dg-summary.csv,
#         results/dg-optimal-model.csv, tables/table-dg.csv
#
# DSI answers how much spatial structure a model explains overall. DG answers
# where. Run this module when the manuscript needs a spatially explicit claim,
# such as a locally optimal model map; skip it with RUN_DG <- FALSE when the
# sample is too sparse for stable local neighbourhoods.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
source(file.path(R_DIR, "02-spatial-metrics.R"))
source(file.path(R_DIR, "04-dg.R"))

log_step("Step 4/6  Compute DG")

if (!isTRUE(RUN_DG)) {
  log_info("RUN_DG is FALSE; skipping the local module")
} else if (!has_pkg("geocomplexity")) {
  log_warn("package 'geocomplexity' not installed; skipping the local module")
  log_info("install.packages('geocomplexity') to enable it")
} else {

  resid_df   <- read_result(F_RESIDUALS)
  model_cols <- setdiff(names(resid_df), c("lon", "lat", "observed"))
  residuals  <- resid_df[, model_cols, drop = FALSE]
  response   <- resid_df$observed
  coords     <- projected_coords(resid_df)

  if (nrow(resid_df) < 5 * K_GEOC) {
    log_warn("only %d evaluation points for k = %d; local complexity will be noisy",
             nrow(resid_df), K_GEOC)
  }

  log_info("geocomplexity neighbourhood: %d nearest neighbours", K_GEOC)

  # -- Point-wise DG ----------------------------------------------------------

  dg_points <- dg_table(response, residuals, coords, k = K_GEOC)
  dg_points$lon <- resid_df$lon[dg_points$point_id]
  dg_points$lat <- resid_df$lat[dg_points$point_id]
  write_result(dg_points, F_DG_POINT, digits = 5)

  # -- Summary ----------------------------------------------------------------

  dg_sum <- dg_summary(dg_points)
  write_result(dg_sum, F_DG_SUMMARY, digits = 4)

  # The mean is what the DG paper reports; the median is reported next to it
  # because the ratio has G0 in the denominator and a handful of points where
  # local complexity is near zero can move the mean on its own.
  disagree <- dg_sum$model[sign(dg_sum$dg_mean) != sign(dg_sum$dg_median)]
  if (length(disagree)) {
    log_warn("mean and median DG disagree in sign for: %s",
             paste(disagree, collapse = ", "))
    log_info("quote the median for these models and say why in the Results section")
  }

  paper_table <- data.frame(
    Model            = dg_sum$model,
    `Mean DG`        = round(dg_sum$dg_mean, 3),
    `95% CI`         = sprintf("[%.3f, %.3f]", dg_sum$dg_ci_low, dg_sum$dg_ci_high),
    `Median DG`      = round(dg_sum$dg_median, 3),
    `Share DG > 0`   = sprintf("%.1f%%", 100 * dg_sum$dg_positive_share),
    check.names = FALSE
  )
  write_result(paper_table, file.path(TAB_DIR, "table-dg.csv"))

  # -- Locally optimal model --------------------------------------------------

  optimal <- dg_optimal_model(dg_points, coords)
  optimal$lon <- resid_df$lon[optimal$point_id]
  optimal$lat <- resid_df$lat[optimal$point_id]
  write_result(optimal, F_DG_OPTIMAL, digits = 4)

  share <- dg_optimal_share(optimal)
  write_result(share, file.path(RES_DIR, "dg-optimal-share.csv"), digits = 4)
  log_info("locally best model: %s at %.1f%% of points (%d of %d)",
           share$model[1], 100 * share$share[1], share$n_locations[1], nrow(optimal))
  log_info("%d model(s) are locally optimal somewhere", nrow(share))
}
