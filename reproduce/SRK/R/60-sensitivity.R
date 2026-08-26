# =============================================================================
# 60-sensitivity.R — robustness to the two SRK parameters (paper Fig. 7)
#
# SRK has exactly two tuning choices: how far up the scale ladder the
# singularity regression runs, and how flat a singularity feature is allowed to
# be before it is dropped. This step re-runs the block cross-validation over a
# grid of both and measures the change from the published baseline
# (20 km, SD 0.5).
#
# Truncating the ladder needs no new neighbourhood search: step 30 cached the
# per-scale intensity matrices, so a shorter ladder is a column subset and a
# re-fitted log-log slope.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("sp", "gstat", "automap", "ranger")
log_head("60  Parameter sensitivity")
t0 <- Sys.time()

BASE_SCALE <- max(SV_SCALES)
BASE_THR   <- SV_SD_THRESHOLD
rows <- list()

for (elem in names(ELEMENTS)) {
  cfg  <- ELEMENTS[[elem]]
  yvar <- cfg$yvar
  d    <- read_result(F_SAMPLES(elem))
  Cs   <- readRDS(file.path(DERIVED, sprintf("intensity-%s.rds", elem)))
  fold <- srk_block_folds(d, CV_FOLDS, CV_BLOCK_SIZE, CV_SEED)

  for (mx in SENS_MAX_SCALES) {
    keep_scale <- SV_SCALES <= mx
    sv <- d[, c("x", "y")]
    for (xv in cfg$xvars) {
      a <- srk_alpha(Cs[[xv]][, keep_scale, drop = FALSE], SV_SCALES[keep_scale],
                     min_valid_scales = MIN_VALID_SCALES)
      a[!is.finite(a)] <- SV_NEUTRAL
      sv[[paste0("sv_", xv)]] <- a
    }
    for (thr in SENS_THRESHOLDS) {
      fm <- do.call(rbind, lapply(seq_len(CV_FOLDS), function(k) {
        out <- srk_run(d[fold != k, ], d[fold == k, ], yvar, cfg$xvars,
                       sv = sv, sd_threshold = thr)
        cbind(srk_metrics(d[[yvar]][fold == k], out$pred),
              n_features = length(out$sv_kept))
      }))
      rows[[length(rows) + 1L]] <- data.frame(
        element = elem, max_scale_km = mx / 1000, sd_threshold = thr,
        R2 = mean(fm$R2), RMSE = mean(fm$RMSE), MAE = mean(fm$MAE),
        sv_features = fm$n_features[1],
        baseline = mx == BASE_SCALE && thr == BASE_THR)
    }
    log_info("%s: max scale %2.0f km done", elem, mx / 1000)
  }
}

sens <- do.call(rbind, rows)

# Relative change against the baseline cell, as in Fig. 7(e).
sens <- do.call(rbind, lapply(split(sens, sens$element), function(g) {
  b <- g[g$baseline, ]
  g$dR2_pct   <- 100 * (g$R2   - b$R2)   / abs(b$R2)
  g$dRMSE_pct <- 100 * (g$RMSE - b$RMSE) / b$RMSE
  g
}))
write_result(sens, F_SENS)

for (elem in names(ELEMENTS)) {
  g <- sens[sens$element == elem, ]
  best <- g[which.max(g$R2), ]
  log_info("%s: best R2 %.3f at %.0f km / SD %.1f; baseline %.3f",
           elem, best$R2, best$max_scale_km, best$sd_threshold,
           g$R2[g$baseline])
  log_info("%s: %d of %d cells stay within +/-5%% of the baseline on both metrics",
           elem, sum(abs(g$dR2_pct) <= 5 & abs(g$dRMSE_pct) <= 5), nrow(g))
}

record_runtime("60-sensitivity", as.numeric(difftime(Sys.time(), t0, units = "secs")))
