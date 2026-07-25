# =============================================================================
# 30-compute-dsi.R — step 3: the DSI table
# =============================================================================
# Input:  data/derived/analysis-data.csv, data/derived/test-residuals.csv
# Output: results/dsi-metrics.csv, results/dsi-diagnostics.txt,
#         tables/table-dsi.csv
#
# The measurement set-up is fixed once and reused for every model: the same
# neighbours, the same strata, the same evaluation points. Comparing a delta_r
# computed under one set-up with a delta_0 computed under another produces a
# number that is not an attenuation ratio.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
source(file.path(R_DIR, "02-spatial-metrics.R"))
source(file.path(R_DIR, "03-dsi.R"))

log_step("Step 3/6  Compute DSI")

resid_df <- read_result(F_RESIDUALS)
full_df  <- read_result(F_ANALYSIS_DATA)
predictors <- resolve_predictors(full_df, response = RESPONSE)

model_cols <- setdiff(names(resid_df), c("lon", "lat", "observed"))
residuals  <- resid_df[, model_cols, drop = FALSE]
response   <- resid_df$observed

# -- Fixed measurement set-up -------------------------------------------------

coords <- projected_coords(resid_df)
listw  <- build_knn_weights(coords, k = K_MORAN)
log_info("Moran's I weights: %d-nearest neighbours, row-standardised, EPSG:%s",
         K_MORAN, PROJECTED_CRS)

# Strata come from the evaluation points and their predictors, so that
# delta_0 and every delta_r are measured over the same partition of space.
eval_df <- merge(resid_df[, c("lon", "lat")], full_df, by = c("lon", "lat"),
                 sort = FALSE)
strata <- make_strata(eval_df, response = RESPONSE, predictors = predictors)
log_info("Q-value strata: %s method, %d strata",
         attr(strata, "method"), attr(strata, "n_strata"))

# -- DSI ----------------------------------------------------------------------

dsi <- dsi_table(response, residuals, listw, strata)
write_result(dsi, F_DSI, digits = 4)

paper_table <- data.frame(
  Model                 = dsi$model,
  `Moran I data`        = round(dsi$delta0_moran, 3),
  `Moran I residual`    = round(dsi$deltar_moran, 3),
  `eta autocorrelation` = round(dsi$eta_autocorrelation, 3),
  `Q data`              = round(dsi$delta0_q, 3),
  `Q residual`          = round(dsi$deltar_q, 3),
  `eta heterogeneity`   = round(dsi$eta_heterogeneity, 3),
  `theta min`           = round(dsi$theta_min, 3),
  `theta probable`      = round(dsi$theta_probable, 3),
  `theta max`           = round(dsi$theta_max, 3),
  check.names = FALSE
)
write_result(paper_table, file.path(TAB_DIR, "table-dsi.csv"))

# -- Diagnostics --------------------------------------------------------------

notes <- dsi_diagnostics(dsi)
writeLines(c(sprintf("DSI diagnostics — %s — %s", PROJECT_ID,
                     format(Sys.Date(), "%Y-%m-%d")), "", notes),
           file.path(RES_DIR, "dsi-diagnostics.txt"))
for (n in notes) log_info("%s", n)

# -- Accuracy against interpretability ----------------------------------------
# The claim both source papers make is that these two rankings disagree. The
# rank correlation below is the number to quote when stating how far they do.

acc <- read_result(F_ACCURACY)
cmp <- merge(acc[, c("model", "R2", "RMSE", "MAE")], dsi, by = "model")
cmp <- cmp[order(-cmp$theta_probable), ]
write_result(cmp, file.path(RES_DIR, "accuracy-vs-dsi.csv"), digits = 4)

if (nrow(cmp) >= 3) {
  rho <- suppressWarnings(stats::cor(cmp$R2, cmp$theta_probable, method = "spearman"))
  log_info("Spearman rho between R2 and theta_probable: %.3f", rho)
  best_r2    <- cmp$model[which.max(cmp$R2)]
  best_theta <- cmp$model[which.max(cmp$theta_probable)]
  if (!identical(best_r2, best_theta)) {
    log_info("accuracy and interpretability disagree: %s leads on R2, %s on theta_probable",
             best_r2, best_theta)
  } else {
    log_info("%s leads on both R2 and theta_probable in this case", best_r2)
  }
}
