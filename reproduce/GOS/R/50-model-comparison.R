# =============================================================================
# 50-model-comparison.R — the paper's Fig. 8 / Table 3 comparison
#
# Three predictors of log Zn, all scored on the same repeated 50/50 splits
# (paper Sect. 3.2, step 4):
#   MLR multivariate linear regression   — the covariates entered linearly
#   BCS basic configuration similarity   — gos(kappa = 1), Eq. 6
#   GOS geographically optimal similarity — gos(kappa = lambda), Eq. 11
# Errors are MAE (Eq. 13) and RMSE (Eq. 8) in log Zn units, averaged over the
# repeats. Splits come from cv_splits(), so every model sees exactly the same
# training and testing rows in every repeat and the differences between models
# are paired.
#
# The comparison is run twice, because the two sources disagree about
# preprocessing. The package vignette screens outliers with
# removeoutlier(coef = 2.5); the paper explicitly does not ("potential outliers
# ... will not be removed ... as the high values may indicate the clusters of
# mineral deposits", Sect. 3.2). The screened run is the primary one, because
# every earlier step of this pipeline uses the screened data; the unscreened
# run is reported alongside so the tutorial can say what the screen does rather
# than guess. lambda is re-derived on the unscreened data so GOS is not
# handicapped by a threshold tuned on a different sample set.
#
# Outputs: results/model-comparison.csv
#          results/model-comparison-no-outlier-screening.csv
#          results/model-comparison-repeats.csv   (both runs, `dataset` column)
#          data/derived/best-kappa-unscreened.rds
#          figs/fig06-model-comparison.{png,pdf}   (paper Fig. 8 shape)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geosimilarity", "the similarity models")

log_head("Step 5/5  Model comparison")

dat      <- readRDS(D_SAMPLES)
selected <- readRDS(D_SELECTED)$selected
lambda   <- readRDS(D_LAMBDA)$lambda
f <- stats::as.formula(paste(RESPONSE_LOG, "~", paste(selected, collapse = " + ")))
log_info("formula: %s", deparse(f))

#' Score all three models on one training/testing split.
one_repeat <- function(d, idx, lam) {
  tr <- d[idx, ]; te <- d[-idx, ]
  obs <- te[[RESPONSE_LOG]]

  ## MLR — the same covariates, entered linearly instead of as a configuration
  p_mlr <- stats::predict(stats::lm(f, data = tr), newdata = te)

  ## BCS — every observation contributes, weighted by similarity (Eq. 6)
  p_bcs <- geosimilarity::gos(f, data = tr, newdata = te, kappa = 1,
                              cores = CORES)$pred

  ## GOS — only the observations above the optimal threshold (Eq. 11)
  p_gos <- geosimilarity::gos(f, data = tr, newdata = te, kappa = lam,
                              cores = CORES)$pred

  preds <- list(MLR = p_mlr, BCS = p_bcs, GOS = p_gos)
  data.frame(model = names(preds),
             mae   = vapply(preds, function(p) mae_score(obs, p), numeric(1)),
             rmse  = vapply(preds, function(p) rmse_score(obs, p), numeric(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Run every repeat and return the per-repeat errors.
run_comparison <- function(d, lam, tag) {
  splits <- cv_splits(nrow(d), CV_REPEATS, CV_SPLIT, SEED)
  log_info("[%s] %d samples, %d repeats, %d training / %d testing rows each",
           tag, nrow(d), CV_REPEATS, length(splits[[1]]),
           nrow(d) - length(splits[[1]]))
  t0 <- Sys.time()
  out <- vector("list", CV_REPEATS)
  for (i in seq_len(CV_REPEATS)) {
    out[[i]] <- cbind(dataset = tag, n_samples = nrow(d), repeat_id = i,
                      one_repeat(d, splits[[i]], lam))
    if (i %% 25 == 0 || i == CV_REPEATS)
      log_info("[%s] repeat %d/%d (%.0f s elapsed)", tag, i, CV_REPEATS,
               as.numeric(difftime(Sys.time(), t0, units = "secs")))
  }
  res <- do.call(rbind, out)
  res$model <- factor(res$model, levels = CV_MODELS)
  res
}

#' Collapse the repeats into the paper's Table 3 shape.
summarise_comparison <- function(res, lam) {
  agg <- function(col, fun) vapply(CV_MODELS, function(m)
    fun(res[[col]][res$model == m]), numeric(1))
  cmp <- data.frame(
    dataset  = res$dataset[1],
    n_samples = res$n_samples[1],
    lambda_used_by_gos = lam,
    model    = CV_MODELS,
    mae      = agg("mae", mean),
    mae_sd   = agg("mae", stats::sd),
    rmse     = agg("rmse", mean),
    rmse_sd  = agg("rmse", stats::sd),
    row.names = NULL, stringsAsFactors = FALSE)
  g <- cmp$model == "GOS"
  cmp$mae_reduction_by_gos_pct  <- 100 * (cmp$mae  - cmp$mae[g])  / cmp$mae
  cmp$rmse_reduction_by_gos_pct <- 100 * (cmp$rmse - cmp$rmse[g]) / cmp$rmse
  cmp[g, c("mae_reduction_by_gos_pct", "rmse_reduction_by_gos_pct")] <- NA
  cmp[, sapply(cmp, is.numeric)] <- round(cmp[, sapply(cmp, is.numeric)], 4)
  cmp
}

report <- function(cmp) {
  for (i in seq_len(nrow(cmp)))
    log_info("%-4s MAE %.4f (sd %.4f)  RMSE %.4f (sd %.4f)%s", cmp$model[i],
             cmp$mae[i], cmp$mae_sd[i], cmp$rmse[i], cmp$rmse_sd[i],
             if (is.na(cmp$mae_reduction_by_gos_pct[i])) "" else
               sprintf("   GOS reduces MAE %+.1f%%, RMSE %+.1f%%",
                       cmp$mae_reduction_by_gos_pct[i],
                       cmp$rmse_reduction_by_gos_pct[i]))
}

#' Paired comparisons over the shared splits. The models saw identical data,
#' so the per-repeat differences are paired.
paired_notes <- function(res, tag) {
  for (m in setdiff(CV_MODELS, "GOS")) {
    dmae <- res$mae[res$model == m] - res$mae[res$model == "GOS"]
    drms <- res$rmse[res$model == m] - res$rmse[res$model == "GOS"]
    log_info("[%s] GOS beats %-3s in %2d/%d repeats on MAE (paired t p = %.3g) and %2d/%d on RMSE",
             tag, m, sum(dmae > 0), CV_REPEATS, stats::t.test(dmae)$p.value,
             sum(drms > 0), CV_REPEATS)
  }
}


# =============================================================================
# Run 1 — the screened data every earlier step of this pipeline uses
# =============================================================================
rep_screened <- run_comparison(dat$samples, lambda, "screened")
cmp_screened <- summarise_comparison(rep_screened, lambda)
write_result(cmp_screened, F_MODELS)
report(cmp_screened)
paired_notes(rep_screened, "screened")


# =============================================================================
# Run 2 — the paper's own preprocessing: no outlier screening at all
# =============================================================================
# lambda is re-derived here. Reusing the screened lambda would test a threshold
# tuned on a different sample set, which is not what the paper does either.
all_s <- dat$all_samples
log_head("Robustness check: no outlier screening (%d samples)", nrow(all_s))
set.seed(SEED)
bk_all <- timeit("gos_bestkappa on the unscreened data",
                 geosimilarity::gos_bestkappa(
                   f, data = all_s, kappa = KAPPA_GRID,
                   nrepeat = BESTKAPPA_NREPEAT, nsplit = BESTKAPPA_NSPLIT,
                   cores = CORES))
lambda_all <- bk_all$bestkappa
cv_all <- bk_all$cvmean
log_info("lambda on the unscreened data = %.2f (screened: %.2f)", lambda_all, lambda)
log_info("cross-validation RMSE %.4f at lambda vs %.4f at kappa = 1 (%.2f%% lower)",
         cv_all$rmse[cv_all$kappa == lambda_all], cv_all$rmse[cv_all$kappa == 1],
         100 * (cv_all$rmse[cv_all$kappa == 1] - cv_all$rmse[cv_all$kappa == lambda_all]) /
           cv_all$rmse[cv_all$kappa == 1])
saveRDS(list(lambda = lambda_all, curve = as.data.frame(cv_all)),
        file.path(DERIVED, "best-kappa-unscreened.rds"))

rep_all <- run_comparison(all_s, lambda_all, "unscreened")
cmp_all <- summarise_comparison(rep_all, lambda_all)
write_result(cmp_all, F_MODELS_NOSCREEN)
report(cmp_all)
paired_notes(rep_all, "unscreened")

# Both row-sets go into one repeats file, distinguished by `dataset`.
per_rep <- rbind(rep_screened, rep_all)
write_result(per_rep[order(per_rep$dataset, per_rep$repeat_id, per_rep$model),
                     c("dataset", "n_samples", "repeat_id", "model", "mae", "rmse")],
             F_MODREP)

# The one-line answer the tutorial needs: does the screen change the ranking?
rank_of <- function(cmp) cmp$model[order(cmp$rmse)]
log_info("RMSE ranking, screened  : %s", paste(rank_of(cmp_screened), collapse = " < "))
log_info("RMSE ranking, unscreened: %s", paste(rank_of(cmp_all), collapse = " < "))


# -- Fig. 6: the error distributions -------------------------------------------
# Boxplots over the repeats rather than bars, because the split-to-split
# spread is large next to the differences between models; the diamond marks
# the mean, which is the number the summary table reports. Only the screened
# run is drawn — the unscreened numbers are in their own CSV.
draw_fig06 <- function() {
  par(mfrow = c(1, 2), mar = c(3.4, 4.0, 2.2, 0.8), mgp = c(2.5, 0.6, 0),
      cex.axis = 0.85, cex.lab = 0.95, tcl = -0.3)
  panel <- function(col, ylab, main, tag) {
    vals <- split(rep_screened[[col]], rep_screened$model)
    boxplot(vals, col = adjustcolor(PAL_MODEL[CV_MODELS], 0.35),
            border = PAL_MODEL[CV_MODELS], outpch = 16, outcex = 0.4,
            outcol = "grey55", axes = FALSE, ylab = "", boxwex = 0.5, lwd = 1.1)
    axis(1, at = seq_along(CV_MODELS), labels = CV_MODELS)
    axis(2, las = 1)
    points(seq_along(CV_MODELS), vapply(vals, mean, numeric(1)),
           pch = 23, bg = "white", col = "grey20", cex = 1.0, lwd = 1.1)
    abline(h = mean(vals[["GOS"]]), col = PAL_MODEL[["GOS"]], lty = 2, lwd = 1.0)
    box()
    title(ylab = ylab)
    mtext(main, side = 3, line = 0.4, font = 2, cex = 0.9)
    mtext(sprintf("%d repeated 50/50 splits", CV_REPEATS), side = 1, line = 2.1,
          cex = 0.75)
    panel_tag(tag, adj = -0.16)
  }
  panel("mae", "MAE of log Zn", "Mean absolute error", "(a)")
  panel("rmse", "RMSE of log Zn", "Root-mean-square error", "(b)")
}
draw_figure("fig06-model-comparison", draw_fig06, width = FIG_W_DOUBLE, height = 9.5)
