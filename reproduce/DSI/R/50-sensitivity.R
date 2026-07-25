# =============================================================================
# 50-sensitivity.R — step 5: is the result an artefact of the settings?
# =============================================================================
# Input:  data/derived/analysis-data.csv, data/derived/test-residuals.csv
# Output: results/sensitivity-k.csv, results/sensitivity-strata.csv,
#         results/sensitivity-summary.txt
#
# Two settings are analyst choices rather than properties of the data: the
# neighbourhood size k behind Moran's I, and the stratification behind the Q
# value. Reviewers of a DSI paper ask about both. Section 5.2 of the DSI paper
# reports differences under alternative validation strategies below 5%; this
# step produces the equivalent evidence for a new case.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
source(file.path(R_DIR, "02-spatial-metrics.R"))
source(file.path(R_DIR, "03-dsi.R"))

log_step("Step 5/6  Sensitivity of DSI to measurement settings")

if (!isTRUE(RUN_SENSITIVITY)) {
  log_info("RUN_SENSITIVITY is FALSE; skipping")
} else {

  resid_df   <- read_result(F_RESIDUALS)
  full_df    <- read_result(F_ANALYSIS_DATA)
  predictors <- resolve_predictors(full_df, response = RESPONSE)
  model_cols <- setdiff(names(resid_df), c("lon", "lat", "observed"))
  residuals  <- resid_df[, model_cols, drop = FALSE]
  response   <- resid_df$observed
  coords     <- projected_coords(resid_df)
  eval_df    <- merge(resid_df[, c("lon", "lat")], full_df, by = c("lon", "lat"),
                      sort = FALSE)

  # -- Neighbourhood size -----------------------------------------------------

  k_values <- K_SENSITIVITY[K_SENSITIVITY < nrow(resid_df)]
  strata_ref <- make_strata(eval_df, response = RESPONSE, predictors = predictors)

  rows <- lapply(k_values, function(k) {
    lw <- build_knn_weights(coords, k = k)
    out <- dsi_table(response, residuals, lw, strata_ref)
    cbind(k = k, out[, c("model", "eta_autocorrelation", "eta_heterogeneity",
                         "theta_min", "theta_probable", "theta_max")])
  })
  sens_k <- do.call(rbind, rows)
  write_result(sens_k, F_SENSITIVITY, digits = 4)

  # Percentage change against the configured k, which is what the paper reports.
  base <- sens_k[sens_k$k == K_MORAN, c("model", "theta_probable")]
  names(base)[2] <- "theta_base"
  cmp <- merge(sens_k, base, by = "model")
  cmp$pct_change <- 100 * (cmp$theta_probable - cmp$theta_base) / abs(cmp$theta_base)
  worst <- cmp[which.max(abs(cmp$pct_change)), ]
  log_info("k varied over {%s}; largest change in theta_probable is %.2f%% (%s at k = %d)",
           paste(k_values, collapse = ", "), worst$pct_change, worst$model, worst$k)

  stability <- if (max(abs(cmp$pct_change), na.rm = TRUE) < 5) {
    "DSI is stable across neighbourhood sizes (all changes below 5%)."
  } else {
    paste0("DSI moves by more than 5% across neighbourhood sizes. Report the ",
           "range in the manuscript and justify the chosen k from the sampling ",
           "design rather than from the result it produces.")
  }
  log_info("%s", stability)

  # -- Stratification ---------------------------------------------------------
  # The Q value depends on how space is partitioned. Comparing tree strata with
  # quantile strata shows whether the heterogeneity component of DSI is a
  # property of the model or of the partition.

  strata_alt <- make_strata(eval_df, response = RESPONSE, predictors = predictors,
                            method = if (STRATIFY_METHOD == "tree") "kmeans" else "tree")
  lw <- build_knn_weights(coords, k = K_MORAN)
  dsi_ref <- dsi_table(response, residuals, lw, strata_ref)
  dsi_alt <- dsi_table(response, residuals, lw, strata_alt)

  sens_s <- merge(
    dsi_ref[, c("model", "eta_heterogeneity")],
    dsi_alt[, c("model", "eta_heterogeneity")],
    by = "model", suffixes = c("_primary", "_alternative")
  )
  sens_s$difference <- sens_s$eta_heterogeneity_alternative - sens_s$eta_heterogeneity_primary
  sens_s$strata_primary     <- attr(strata_ref, "method")
  sens_s$strata_alternative <- attr(strata_alt, "method")
  write_result(sens_s, file.path(RES_DIR, "sensitivity-strata.csv"), digits = 4)

  rank_agreement <- suppressWarnings(stats::cor(
    rank(-sens_s$eta_heterogeneity_primary),
    rank(-sens_s$eta_heterogeneity_alternative), method = "spearman"))
  log_info("model ranking agreement across stratification methods: rho = %.3f",
           rank_agreement)

  # -- Written summary --------------------------------------------------------

  writeLines(c(
    sprintf("Sensitivity summary — %s — %s", PROJECT_ID, format(Sys.Date(), "%Y-%m-%d")),
    "",
    sprintf("Neighbourhood size: k in {%s}, primary k = %d.",
            paste(k_values, collapse = ", "), K_MORAN),
    sprintf("Largest change in theta_probable: %.2f%% (%s at k = %d).",
            worst$pct_change, worst$model, worst$k),
    stability,
    "",
    sprintf("Stratification: %s (primary) against %s (alternative).",
            attr(strata_ref, "method"), attr(strata_alt, "method")),
    sprintf("Spearman rank agreement of eta_heterogeneity: %.3f.", rank_agreement),
    if (rank_agreement > 0.8)
      "Model ranking survives the change of partition."
    else
      paste("Model ranking depends on the partition. State the stratification",
            "rule in the Methods section and report both rankings.")
  ), file.path(RES_DIR, "sensitivity-summary.txt"))
  log_info("wrote results/sensitivity-summary.txt")
}
