# =============================================================================
# test-02-reproduce-pipeline.R — the committed results regenerate exactly
#
# Every number in results/ was produced by run-all.R under the seeds in
# config/project-config.R. This test recomputes the load-bearing ones from
# scratch and compares them with the committed CSVs, so a change in R, in
# geosimilarity, or in the pipeline itself shows up as a failing check rather
# than as a quietly different tutorial.
#
# It repeats the expensive parts of steps 30-50, so allow about a minute.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geosimilarity", "the GOS method")

cat("\n== test-02  The committed results regenerate ============================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-60s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}
near <- function(a, b, tol = 1e-8) length(a) == length(b) &&
  all(is.finite(a)) && all(is.finite(b)) && max(abs(a - b)) < tol

need <- c(F_RESPONSE, F_VARSEL, F_KAPPA, F_GRID, F_GRIDBCS, F_MODELS,
          F_MODELS_NOSCREEN, F_MODREP)
check("every results/*.csv is present", all(file.exists(need)))
if (!all(file.exists(need))) stop("run `Rscript run-all.R` before the tests")


## -- step 10: the data preparation --------------------------------------------
zn <- as.data.frame(geosimilarity::zn)
zn[[RESPONSE_LOG]] <- log(zn[[RESPONSE]])
idx <- geosimilarity::removeoutlier(zn[[RESPONSE_LOG]], coef = OUTLIER_COEF)
d <- zn[-idx, ]; rownames(d) <- NULL

resp <- read_result(F_RESPONSE)
check(sprintf("the packaged zn table still holds 894 samples (got %d)", nrow(zn)),
      nrow(zn) == 894)
check(sprintf("the outlier screen still retains 885 rows (got %d)", nrow(d)),
      nrow(d) == 885)
row_log <- resp$series == "log Zn (outliers screened)"
check(sprintf("committed mean of log Zn = %.4f", resp$mean[row_log]),
      near(resp$mean[row_log], mean(d[[RESPONSE_LOG]]), 1e-4))
check(sprintf("committed CV of log Zn = %.4f", resp$cv[row_log]),
      near(resp$cv[row_log],
           stats::sd(d[[RESPONSE_LOG]]) / mean(d[[RESPONSE_LOG]]), 1e-4))


## -- step 20: correlation and VIF ---------------------------------------------
vs <- read_result(F_VARSEL)
selected <- vs$variable[vs$selected]
r_now <- vapply(vs$variable, function(v)
  unname(stats::cor(d[[RESPONSE_LOG]], d[[v]])), numeric(1))
check("all nine candidate correlations reproduce", near(vs$r, r_now, 1e-4))
check(sprintf("the same %d covariates are selected: %s", length(selected),
              paste(selected, collapse = ", ")),
      setequal(selected, VIGNETTE_VARS))
vif_now <- vif_lm(d, selected)
check("VIFs of the selected set reproduce", near(vs$vif[vs$selected], vif_now[vs$variable[vs$selected]], 1e-4))
check(sprintf("every selected VIF is below %.0f (max %.3f)", VIF_THRESHOLD, max(vif_now)),
      max(vif_now) < VIF_THRESHOLD)


## -- step 30: the optimal threshold lambda ------------------------------------
f <- stats::as.formula(paste(RESPONSE_LOG, "~", paste(selected, collapse = " + ")))
kap <- read_result(F_KAPPA)
lambda_committed <- kap$kappa[as.logical(kap$selected)]

set.seed(SEED)
bk <- geosimilarity::gos_bestkappa(f, data = d, kappa = KAPPA_GRID,
                                   nrepeat = BESTKAPPA_NREPEAT,
                                   nsplit = BESTKAPPA_NSPLIT, cores = CORES)
check(sprintf("lambda reproduces as %.2f", lambda_committed),
      identical(bk$bestkappa, lambda_committed))
curve_now <- bk$cvmean[order(bk$cvmean$kappa), ]
check("the whole kappa-RMSE curve reproduces", near(kap$rmse, curve_now$rmse, 1e-6))


## -- step 40: the grid prediction ----------------------------------------------
# gos() standardises each covariate over the observation and prediction
# locations together, so the full 13,132-cell grid has to be predicted again;
# a subset would legitimately give different numbers.
gridd <- as.data.frame(geosimilarity::grid)
gp <- read_result(F_GRID)
g <- as.data.frame(geosimilarity::gos(f, data = d, newdata = gridd,
                                      kappa = lambda_committed, cores = CORES))
check(sprintf("the grid prediction has %d rows", nrow(gp)), nrow(gp) == nrow(gridd))
check("every grid prediction reproduces", near(gp$pred, g$pred, 1e-6))
check("the back-transformed column is exp(pred)",
      near(gp$pred_zn_ppm, exp(gp$pred), 1e-3))
check("committed uncertainty at zeta = 0.99 reproduces",
      near(gp[["uncertainty99"]], g$uncertainty99, 1e-7))


## -- step 50: the model comparison --------------------------------------------
# Three of the CV_REPEATS splits are enough to prove the split generator and
# every model agree with what was committed; running all of them would only
# repeat the same arithmetic.
splits <- cv_splits(nrow(d), CV_REPEATS, CV_SPLIT, SEED)
rep_all <- read_result(F_MODREP)
rep_committed <- rep_all[rep_all$dataset == "screened", ]
check("the repeats file covers both preprocessing variants",
      setequal(unique(rep_all$dataset), c("screened", "unscreened")) &&
        nrow(rep_all) == 2 * CV_REPEATS * length(CV_MODELS))

recompute <- do.call(rbind, lapply(1:3, function(i) {
  tr <- d[splits[[i]], ]; te <- d[-splits[[i]], ]; obs <- te[[RESPONSE_LOG]]
  preds <- list(
    MLR = stats::predict(stats::lm(f, data = tr), newdata = te),
    BCS = geosimilarity::gos(f, data = tr, newdata = te, kappa = 1,
                             cores = CORES)$pred,
    GOS = geosimilarity::gos(f, data = tr, newdata = te,
                             kappa = lambda_committed, cores = CORES)$pred)
  data.frame(repeat_id = i, model = names(preds),
             mae = vapply(preds, function(p) mae_score(obs, p), numeric(1)),
             rmse = vapply(preds, function(p) rmse_score(obs, p), numeric(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}))
key <- function(df) paste(df$repeat_id, df$model)
sub <- rep_committed[match(key(recompute), key(rep_committed)), ]
check("the first three CV repeats reproduce for all three models, MAE",
      near(sub$mae, recompute$mae, 1e-8))
check("the first three CV repeats reproduce for all three models, RMSE",
      near(sub$rmse, recompute$rmse, 1e-8))

cmp <- read_result(F_MODELS)
check("model-comparison.csv averages the per-repeat file",
      near(cmp$mae, vapply(cmp$model, function(m)
        mean(rep_committed$mae[rep_committed$model == m]), numeric(1)), 1e-4) &&
      near(cmp$rmse, vapply(cmp$model, function(m)
        mean(rep_committed$rmse[rep_committed$model == m]), numeric(1)), 1e-4))
check("GOS lowers both errors relative to BCS, as the paper reports",
      cmp$mae[cmp$model == "GOS"] < cmp$mae[cmp$model == "BCS"] &&
        cmp$rmse[cmp$model == "GOS"] < cmp$rmse[cmp$model == "BCS"])
check("GOS lowers both errors relative to MLR",
      cmp$mae[cmp$model == "GOS"] < cmp$mae[cmp$model == "MLR"] &&
        cmp$rmse[cmp$model == "GOS"] < cmp$rmse[cmp$model == "MLR"])

## -- the robustness run on the paper's own preprocessing ---------------------
# This is a check that the second configuration was actually run on all 894
# samples and averages its own repeats, not that it produces any particular
# ranking; whatever ranking it gives is what the tutorial reports.
cmp_ns <- read_result(F_MODELS_NOSCREEN)
rep_ns <- rep_all[rep_all$dataset == "unscreened", ]
check(sprintf("the unscreened run uses all %d samples", nrow(zn)),
      all(cmp_ns$n_samples == nrow(zn)) && all(rep_ns$n_samples == nrow(zn)))
check("the unscreened summary averages its own repeats",
      near(cmp_ns$rmse, vapply(cmp_ns$model, function(m)
        mean(rep_ns$rmse[rep_ns$model == m]), numeric(1)), 1e-4))
check("GOS still lowers both errors relative to BCS without screening",
      cmp_ns$mae[cmp_ns$model == "GOS"] < cmp_ns$mae[cmp_ns$model == "BCS"] &&
        cmp_ns$rmse[cmp_ns$model == "GOS"] < cmp_ns$rmse[cmp_ns$model == "BCS"])

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass == total)
  cat("   PASS - run-all.R regenerates the committed results\n") else
  stop("test-02 had failures")
