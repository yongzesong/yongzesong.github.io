# =============================================================================
# 40-block-cv.R — spatial block cross-validation of six models (paper Table 2)
#
# Five folds of whole 15 km blocks are held out in turn, and OK, IDW, LM, RF,
# RFK and SRK are each fitted on the remaining blocks and scored on the held-out
# one. Blocking matters: a random split would leave every test point ringed by
# its own training neighbours, which flatters every kriging-based model.
#
# The step also fits SRK once on all samples to record variable importance, the
# residual variogram and the RF-vs-SRK residual contrast of Fig. 6(c, g).
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("sp", "gstat", "automap", "ranger")
log_head("40  Spatial block cross-validation")
t0 <- Sys.time()

fold_rows <- list(); pred_rows <- list(); imp_rows <- list()
vgm_rows  <- list(); res_rows  <- list()

for (elem in names(ELEMENTS)) {
  cfg  <- ELEMENTS[[elem]]
  yvar <- cfg$yvar
  d    <- read_result(F_SAMPLES(elem))
  sv   <- read_result(F_SV(elem))

  fold <- srk_block_folds(d, CV_FOLDS, CV_BLOCK_SIZE, CV_SEED)
  log_info("%s: %d samples in %d blocks -> %d folds (%s per fold)",
           elem, nrow(d), attr(fold, "n_blocks"), CV_FOLDS,
           paste(as.integer(table(fold)), collapse = "/"))

  for (k in seq_len(CV_FOLDS)) {
    tr <- d[fold != k, ]; te <- d[fold == k, ]
    for (mname in names(SRK_MODELS)) {
      out <- SRK_MODELS[[mname]](tr, te, yvar, cfg$xvars, sv = sv)
      met <- srk_metrics(te[[yvar]], out$pred)
      fold_rows[[length(fold_rows) + 1L]] <-
        data.frame(element = elem, model = mname, fold = k, n_test = nrow(te), met)
      pred_rows[[length(pred_rows) + 1L]] <-
        data.frame(element = elem, model = mname, fold = k,
                   x = te$x, y = te$y, obs = te[[yvar]], pred = out$pred)
    }
    log_info("  fold %d (n = %3d) done", k, nrow(te))
  }

  # -- One fit on everything: importance, variogram, residual shape -----------
  full <- srk_run(d, d, yvar, cfg$xvars, sv = sv)
  imp  <- sort(full$importance, decreasing = TRUE)
  imp_rows[[elem]] <- data.frame(
    element = elem, feature = names(imp), importance = as.numeric(imp),
    kind = ifelse(names(imp) %in% c("x", "y"), "coordinate",
           ifelse(grepl("^sv_", names(imp)), "singularity", "covariate")),
    share = as.numeric(imp) / sum(as.numeric(imp)))
  if (!is.null(full$vgm))
    vgm_rows[[elem]] <- data.frame(element = elem, full$vgm[, c("model", "psill", "range")])

  rf_full <- bench_rf(d, d, yvar, cfg$xvars)
  res_rows[[elem]] <- data.frame(
    element = elem,
    RF  = d[[yvar]] - rf_full$pred,
    SRK = full$resid)
  log_info("%s: sd(residual) RF = %.2f, SRK = %.2f; kept %s", elem,
           stats::sd(d[[yvar]] - rf_full$pred), stats::sd(full$resid),
           paste(full$sv_kept, collapse = ", "))
}

folds <- do.call(rbind, fold_rows)
write_result(folds, F_CV_FOLDS)
write_result(do.call(rbind, pred_rows), F_CV_PRED)
write_result(do.call(rbind, imp_rows),  F_IMPORTANCE)
write_result(do.call(rbind, vgm_rows),  F_VARIOGRAM)
write_result(do.call(rbind, res_rows),  file.path(RES_DIR, "residuals-rf-vs-srk.csv"))

# -- Fold means are what Table 2 reports -------------------------------------
summary <- do.call(rbind, lapply(split(folds, list(folds$element, folds$model)),
  function(g) data.frame(element = g$element[1], model = g$model[1],
                         R2 = mean(g$R2), RMSE = mean(g$RMSE), MAE = mean(g$MAE),
                         R2_sd = stats::sd(g$R2), RMSE_sd = stats::sd(g$RMSE))))
summary <- summary[order(summary$element, -summary$R2), ]
rownames(summary) <- NULL

# Differences are always quoted against SRK, as in the paper's "vs SRK" rows.
summary <- do.call(rbind, lapply(split(summary, summary$element), function(g) {
  s <- g[g$model == "SRK", ]
  g$dR2_vs_SRK   <- s$R2 - g$R2
  g$dRMSE_vs_SRK <- s$RMSE - g$RMSE
  g$dMAE_vs_SRK  <- s$MAE - g$MAE
  g
}))
write_result(summary, F_CV_SUMMARY)

for (elem in names(ELEMENTS)) {
  g <- summary[summary$element == elem, ]
  log_info("%s ranking by R2: %s", elem,
           paste(sprintf("%s %.3f", g$model, g$R2), collapse = "  "))
}

record_runtime("40-block-cv", as.numeric(difftime(Sys.time(), t0, units = "secs")))
