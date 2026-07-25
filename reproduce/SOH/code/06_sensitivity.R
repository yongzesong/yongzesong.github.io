# =============================================================================
# 06_sensitivity.R — scale and threshold sensitivity
# =============================================================================
# The headline methodological result of the reference study came from the scale
# sweep: PD of the SOP-only and combined models rose with neighbourhood radius
# and stabilised near 200 km, while the covariate-only model was flat. That
# plateau is the scale at which local anomaly structure stops adding context.
#
# Two sweeps run here.
#   scale     PD against the largest neighbourhood radius
#   threshold PD against the outlier threshold (1.5, 2, 2.5 SD)
#
# Output tables
#   T12_scale_sensitivity
#   T13_threshold_sensitivity
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("06 SENSITIVITY")

d <- readRDS(soh_cache("data"))
points <- d$points; grids <- d$grids
sopvars <- readRDS(soh_cache("sopvars"))

scale_df <- NULL
if (isTRUE(cfg$experiments$scale_sensitivity)) {
  soh_log("scale sweep over radii: ", paste(cfg$scale_sweep, collapse = ", "))
  scale_df <- soh_exp_scale(points, sopvars, cfg)
  soh_write_table(scale_df, "T12_scale_sensitivity")
  print(scale_df)

  # Locate the plateau on the SOP-only curve, which is the one that measures
  # the second dimension. The combined curve is anchored by the covariate-only
  # floor and would report a plateau it inherited rather than earned.
  #
  # The rule is the smallest radius reaching 95 percent of the sweep maximum.
  # A step-difference rule is unusable, because CART is greedy and the curve is
  # not monotone.
  a1 <- scale_df[scale_df$scenario == "A1 SOP only", ]
  a1 <- a1[order(a1$radius), ]
  if (nrow(a1) > 2) {
    plateau <- a1$radius[which(a1$qv >= 0.95 * max(a1$qv))[1]]
    soh_log("SOP-only PD reaches 95 percent of its sweep maximum at a radius of ",
            plateau, " ", cfg$data$dist_unit,
            " (maximum ", sprintf("%.3f", max(a1$qv)), " at ",
            a1$radius[which.max(a1$qv)], " ", cfg$data$dist_unit, ")")
    soh_log("that radius is the scale at which local anomaly structure stops ",
            "adding explanatory context, and is the scale finding to report")
    if (plateau == max(a1$radius)) {
      soh_log("PD is still rising at the largest radius in the sweep; extend ",
              "cfg$scale_sweep before claiming a scale threshold", level = "WARN ")
    }
    if (any(diff(a1$qv) < -0.02)) {
      soh_log("the scale curve is not monotone. CART chooses splits greedily, ",
              "so a larger predictor set can yield a lower PD. Report the curve ",
              "as it is and interpret the plateau, not each step.")
    }
  }
  saveRDS(scale_df, soh_cache("scale"))
}

thr_df <- NULL
if (isTRUE(cfg$experiments$threshold_sensitivity)) {
  soh_log("threshold sweep over: ", paste(cfg$threshold_sweep, collapse = ", "), " SD")
  thr_df <- soh_exp_threshold(points, grids, cfg)
  soh_write_table(thr_df, "T13_threshold_sensitivity")
  print(thr_df)
  a2 <- thr_df[thr_df$scenario == "A2 variables + SOP", ]
  soh_log(sprintf("combined PD ranges from %.3f to %.3f across thresholds, a spread of %.3f",
                  min(a2$qv), max(a2$qv), diff(range(a2$qv))))
  if (diff(range(a2$qv)) > 0.1) {
    soh_log("conclusions are sensitive to the outlier threshold; report all ",
            "three thresholds in the manuscript rather than one", level = "WARN ")
  }
  saveRDS(thr_df, soh_cache("threshold"))
}
