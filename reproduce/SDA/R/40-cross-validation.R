# =============================================================================
# 40-cross-validation.R — does the second dimension predict better than the first?
#
# The whole method stands or falls here. The same samples, the same split, the
# same models: only the explanatory variables differ.
#   FDA — the eight surfaces read AT the sample locations (8 variables)
#   SDA — second-dimension variables selected from around the samples
# Variable selection happens on the training set only, so the test set stays
# untouched by the selection.
#
# Outputs: results/cross-validation.csv  (R2 and RMSE per model x dimension)
#          results/cv-predictions.csv     (observed vs predicted, for the figure)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 4/5  Cross validation: SDA against FDA")

pts  <- read_result(F_POINTS)
xall <- load_reference_vars()
fda  <- utils::read.csv(file.path(PROJ_ROOT, FDA_FILE), check.names = FALSE)
if (nrow(fda) != nrow(pts)) stop("FDA_FILE must have one row per sample")

set.seed(SEED)
train <- sample(nrow(pts), floor(TRAIN_FRACTION * nrow(pts)), replace = FALSE)
log_info("split: %d training / %d testing (%.0f%%)", length(train),
         nrow(pts) - length(train), 100 * TRAIN_FRACTION)

# Outliers are removed from the TRAINING response only: the test set must stay
# as observed, otherwise the comparison flatters both models equally.
ytr_all <- pts$y[train]
krm <- sda_rmoutlier(ytr_all, OUTLIER_SD)
keep <- if (length(krm)) train[-krm] else train
if (length(krm)) log_info("removed %d training outlier(s)", length(krm))

ytr <- pts$y[keep]; yte <- pts$y[-train]

## -- SDA: select on training data, then name the test data to match ----------
xtr <- lapply(xall, function(m) m[keep, , drop = FALSE])
xte <- lapply(xall, function(m) m[-train, , drop = FALSE])
sel <- sda_select(ytr, xtr, ctr.vif = VIF_MAX)
newx <- sda_newdata(xte)[, names(sel), drop = FALSE]
log_info("SDA: %d variables selected on the training set", ncol(sel))

## -- FDA: the same surfaces at the sample points -----------------------------
ftr <- fda[keep, , drop = FALSE]; fte <- fda[-train, , drop = FALSE]
log_info("FDA: %d variables (one per surface)", ncol(ftr))

rows <- list(); preds <- list()

fit_eval <- function(dim_label, model_label, fit, newdata) {
  p <- as.numeric(stats::predict(fit, newdata = newdata))
  rows[[length(rows) + 1]] <<- data.frame(
    dimension = dim_label, model = model_label,
    n_variables = ncol(newdata),
    R2 = round(r2_score(yte, p), 4), RMSE = round(rmse_score(yte, p), 4))
  preds[[length(preds) + 1]] <<- data.frame(
    dimension = dim_label, model = model_label,
    observed = yte, predicted = p)
  log_info("%-4s %-14s R2 = %6.4f   RMSE = %6.4f", dim_label, model_label,
           r2_score(yte, p), rmse_score(yte, p))
}

if (isTRUE(RUN_LM)) {
  fit_eval("SDA", "linear model", stats::lm(y ~ ., cbind(y = ytr, sel)), newx)
  fit_eval("FDA", "linear model", stats::lm(y ~ ., cbind(y = ytr, ftr)), fte)
}

if (isTRUE(RUN_RF)) {
  if (!has_pkg("randomForest")) {
    log_warn("randomForest not installed; skipping the random-forest comparison")
  } else {
    set.seed(SEED)
    fit_eval("SDA", "random forest",
             randomForest::randomForest(x = sel, y = ytr, ntree = RF_TREES), newx)
    set.seed(SEED)
    fit_eval("FDA", "random forest",
             randomForest::randomForest(x = ftr, y = ytr, ntree = RF_TREES), fte)
  }
}

cv <- do.call(rbind, rows)
write_result(cv, F_CV)
write_result(do.call(rbind, preds), F_PRED)

for (m in unique(cv$model)) {
  s <- cv$R2[cv$dimension == "SDA" & cv$model == m]
  f <- cv$R2[cv$dimension == "FDA" & cv$model == m]
  if (length(s) && length(f))
    log_info("%s: SDA %.4f vs FDA %.4f  (%+.1f%% relative)", m, s, f, (s / f - 1) * 100)
}
