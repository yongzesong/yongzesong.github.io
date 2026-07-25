# =============================================================================
# 40-explain-errors.R — the paper's claim: geocomplexity explains spatial errors
#
# Each model's absolute error is regressed on the geocomplexity of the selected
# variables, using GWR so the explanation is allowed to vary over space. Two
# numbers matter: how much of the error geocomplexity accounts for, and how
# that share changes as the underlying model gets more spatially aware.
#
# Outputs: results/error-explanation.csv               (R2, RSS, AIC per model)
#          results/error-explanation-coefficients.csv   (local coefficients)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 4/5  Geocomplexity explains the errors")

x   <- sf::read_sf(file.path(DERIVED, "analysis-layer.gpkg"))
gc  <- read_result(F_GC)
err <- read_result(F_ERRORS)

vars <- if (is.null(ERROR_EXPLAIN_VARS)) predictor_names(x) else ERROR_EXPLAIN_VARS
gc_cols <- paste0("GC_", vars)
miss <- setdiff(gc_cols, names(gc))
if (length(miss)) stop("geocomplexity columns not found: ", paste(miss, collapse = ", "))
log_info("explaining errors with the geocomplexity of: %s", paste(vars, collapse = ", "))

if (!has_pkg("GWmodel")) {
  log_warn("GWmodel not installed; the error explanation needs it. Skipping.")
} else {

rows <- list(); coefs <- list()
for (nm in names(err)) {
  model <- sub("^error_", "", nm)
  # The size of the error is what we explain: a model can be wrong in either
  # direction, and geocomplexity is a claim about magnitude, not sign.
  dat <- x[, 0]
  dat$abs_error <- abs(err[[nm]])
  for (g in gc_cols) dat[[g]] <- gc[[g]]
  sp <- methods::as(dat, "Spatial")

  f <- stats::as.formula(paste("abs_error ~", paste(gc_cols, collapse = " + ")))
  bw <- GWmodel::bw.gwr(f, data = sp, approach = GWR_APPROACH,
                        kernel = GWR_KERNEL, adaptive = GWR_ADAPTIVE)
  g  <- GWmodel::gwr.basic(f, data = sp, bw = bw,
                           kernel = GWR_KERNEL, adaptive = GWR_ADAPTIVE)

  rss <- sum(g$SDF$residual^2)
  rows[[model]] <- data.frame(
    model = model, bandwidth = bw,
    R2 = round(g$GW.diagnostic$gw.R2, 4),
    RSS = round(rss, 4),
    AIC = round(g$GW.diagnostic$AICc, 1))

  cf <- as.data.frame(g$SDF)[, gc_cols, drop = FALSE]
  names(cf) <- paste0("coef_", names(cf))
  cf$model <- model
  coefs[[model]] <- cf

  log_info("%-4s errors: R2 = %.4f  RSS = %.4f  AIC = %.0f  (bandwidth %d)",
           model, g$GW.diagnostic$gw.R2, rss, g$GW.diagnostic$AICc, bw)
}

exp_df <- do.call(rbind, rows)
write_result(exp_df, F_EXPLAIN)
write_result(do.call(rbind, coefs), F_EXPCOEF)

# The ordering is the finding: the more spatially aware the model, the less of
# its error is left for geocomplexity to explain.
if (nrow(exp_df) > 1) {
  o <- exp_df[order(-exp_df$R2), ]
  log_info("error explained, high to low: %s",
           paste(sprintf("%s %.3f", o$model, o$R2), collapse = " > "))
}
}
