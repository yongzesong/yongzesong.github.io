# =============================================================================
# 10-prepare-data.R — step 1: raw input to analysis-ready table
# =============================================================================
# Input:  config/project-config.R INPUT_FILE (or the synthetic generator)
# Output: data/derived/analysis-data.csv
#
# Every downstream step reads only the derived file, so the raw data is never
# modified and the preprocessing decisions are recorded in one place.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
source(file.path(R_DIR, "09-make-synthetic-data.R"))

log_step("Step 1/6  Prepare data")

# -- Load ---------------------------------------------------------------------

if (is.null(INPUT_FILE)) {
  log_info("INPUT_FILE is NULL; generating the synthetic demonstration dataset")
  df <- make_synthetic_data()
} else {
  path <- if (file.exists(INPUT_FILE)) INPUT_FILE else file.path(PROJ_ROOT, INPUT_FILE)
  if (!file.exists(path)) {
    stop("INPUT_FILE not found: ", INPUT_FILE,
         "\nPut your table in data/raw/ and set INPUT_FILE in config/project-config.R.")
  }
  log_info("reading %s", basename(path))
  df <- utils::read.csv(path, check.names = FALSE)
}

log_info("%d rows, %d columns", nrow(df), ncol(df))

# -- Validate and clean -------------------------------------------------------

df <- validate_input(df, response = RESPONSE)
predictors <- resolve_predictors(df, response = RESPONSE)
log_info("response: %s", RESPONSE)
log_info("%d predictor(s): %s", length(predictors), paste(predictors, collapse = ", "))

df <- df[, c("lon", "lat", RESPONSE, predictors), drop = FALSE]
n_before <- nrow(df)
df <- df[stats::complete.cases(df), , drop = FALSE]
if (nrow(df) < n_before) {
  log_warn("dropped %d row(s) with missing predictor values", n_before - nrow(df))
}

# -- Response transformation --------------------------------------------------
# A right-skewed response makes RMSE and the Q value dominated by a few
# extreme points. The DG paper log-transforms biomass for exactly this reason.

if (LOG_TRANSFORM_RESPONSE) {
  if (any(df[[RESPONSE]] <= 0)) {
    shift <- abs(min(df[[RESPONSE]])) + 1e-6
    log_warn("response has non-positive values; using log(y + %.4g)", shift)
    df[[RESPONSE]] <- log(df[[RESPONSE]] + shift)
  } else {
    df[[RESPONSE]] <- log(df[[RESPONSE]])
  }
  log_info("response log-transformed")
}

# -- Collinearity screen ------------------------------------------------------
# Reported, not applied. Dropping predictors changes the tree that defines the
# Q-value strata, so the decision belongs to the analyst and belongs in the
# manuscript's preprocessing paragraph.

cormat <- stats::cor(df[, predictors, drop = FALSE], use = "pairwise.complete.obs")
cormat[upper.tri(cormat, diag = TRUE)] <- NA
high <- which(abs(cormat) > 0.8, arr.ind = TRUE)
if (nrow(high)) {
  pairs <- data.frame(
    var1 = rownames(cormat)[high[, "row"]],
    var2 = colnames(cormat)[high[, "col"]],
    r    = round(cormat[high], 3)
  )
  write_result(pairs, file.path(RES_DIR, "collinear-pairs.csv"))
  log_warn("%d predictor pair(s) with |r| > 0.8; see results/collinear-pairs.csv",
           nrow(pairs))
  log_info("decide which to drop via DROP_COLS in config/project-config.R, then re-run")
} else {
  log_info("no predictor pair exceeds |r| = 0.8")
}

# -- Descriptive summary for the manuscript -----------------------------------

summ <- data.frame(
  variable = c(RESPONSE, predictors),
  role     = c("response", rep("predictor", length(predictors))),
  n        = nrow(df),
  mean     = vapply(df[, c(RESPONSE, predictors)], mean, numeric(1)),
  sd       = vapply(df[, c(RESPONSE, predictors)], stats::sd, numeric(1)),
  min      = vapply(df[, c(RESPONSE, predictors)], min, numeric(1)),
  max      = vapply(df[, c(RESPONSE, predictors)], max, numeric(1)),
  row.names = NULL
)
write_result(summ, file.path(RES_DIR, "variable-summary.csv"), digits = 4)

# -- Write --------------------------------------------------------------------

write_result(df, F_ANALYSIS_DATA)
log_info("analysis set: %d points, %d predictors", nrow(df), length(predictors))
