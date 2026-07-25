# =============================================================================
# 30-baseline-models.R — the models whose errors are the subject
#
# Three conventional models of increasing spatial awareness: a global linear
# model, a non-linear but aspatial one, and a locally varying one. The point is
# not which wins, but that each leaves errors, and that those errors are not
# spatially random.
#
# Outputs: results/baseline-models.csv  (fit statistics)
#          results/model-errors.csv      (per-unit errors of each model)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 3/5  Baseline models and their errors")

x <- sf::read_sf(file.path(DERIVED, "analysis-layer.gpkg"))
d <- sf::st_drop_geometry(x)
preds <- predictor_names(x)
f <- stats::as.formula(paste(RESPONSE, "~", paste(preds, collapse = " + ")))
y <- d[[RESPONSE]]

fits <- list(); errs <- list()

if (isTRUE(RUN_MLR)) {
  m <- stats::lm(f, data = d)
  p <- stats::fitted(m)
  fits[["MLR"]] <- data.frame(model = "MLR", R2 = round(r2_score(y, p), 4),
                              RMSE = round(rmse_score(y, p), 5),
                              AIC = round(stats::AIC(m), 1))
  errs[["MLR"]] <- y - p
  log_info("MLR  R2 = %.4f  RMSE = %.5f", r2_score(y, p), rmse_score(y, p))
}

if (isTRUE(RUN_SVR)) {
  if (!has_pkg("e1071")) {
    log_warn("e1071 not installed; skipping SVR")
  } else {
    set.seed(SEED)
    m <- e1071::svm(f, data = d, kernel = "radial",
                    cost = SVR_COST, gamma = SVR_GAMMA)
    p <- stats::fitted(m)
    fits[["SVR"]] <- data.frame(model = "SVR", R2 = round(r2_score(y, p), 4),
                                RMSE = round(rmse_score(y, p), 5), AIC = NA_real_)
    errs[["SVR"]] <- y - p
    log_info("SVR  R2 = %.4f  RMSE = %.5f  (cost = %.1f, gamma = %.1f)",
             r2_score(y, p), rmse_score(y, p), SVR_COST, SVR_GAMMA)
  }
}

if (isTRUE(RUN_GWR)) {
  if (!has_pkg("GWmodel")) {
    log_warn("GWmodel not installed; skipping GWR")
  } else {
    sp <- methods::as(x, "Spatial")
    bw <- GWmodel::bw.gwr(f, data = sp, approach = GWR_APPROACH,
                          kernel = GWR_KERNEL, adaptive = GWR_ADAPTIVE)
    m <- GWmodel::gwr.basic(f, data = sp, bw = bw,
                            kernel = GWR_KERNEL, adaptive = GWR_ADAPTIVE)
    p <- y - m$SDF$residual
    fits[["GWR"]] <- data.frame(model = "GWR", R2 = round(m$GW.diagnostic$gw.R2, 4),
                                RMSE = round(rmse_score(y, p), 5),
                                AIC = round(m$GW.diagnostic$AICc, 1))
    errs[["GWR"]] <- as.numeric(m$SDF$residual)
    log_info("GWR  R2 = %.4f  RMSE = %.5f  (adaptive bandwidth %d by %s)",
             m$GW.diagnostic$gw.R2, rmse_score(y, p), bw, GWR_APPROACH)
  }
}

if (!length(fits)) stop("no baseline model ran")
write_result(do.call(rbind, fits), F_BASE)

err_df <- as.data.frame(errs)
names(err_df) <- paste0("error_", names(errs))
write_result(err_df, F_ERRORS)

# Are the errors spatially structured? If they were random noise there would be
# nothing for geocomplexity to explain, and the rest of the pipeline would be
# pointless. This is the precondition, so it is tested rather than assumed.
if (has_pkg("spdep")) {
  wt <- readRDS(file.path(DERIVED, "weights.rds"))
  lw <- spdep::mat2listw(wt, style = "W", zero.policy = TRUE)
  for (m in names(errs)) {
    mt <- spdep::moran.test(errs[[m]], lw, zero.policy = TRUE)
    log_info("%s errors: Moran's I = %+.3f (p = %.3g) %s", m,
             mt$estimate[1], mt$p.value,
             ifelse(mt$p.value < 0.05, "-> spatially structured", "-> no structure"))
  }
}
