# =============================================================================
# 20-fit-models.R — step 2: models, predictions, residuals, accuracy
# =============================================================================
# Input:  data/derived/analysis-data.csv
# Output: data/derived/test-predictions.csv
#         data/derived/test-residuals.csv
#         results/accuracy-metrics.csv
#         tables/table-models.csv
#
# Residuals are computed on the held-out test set, following both source
# papers: DSI measured on training residuals rewards memorisation, since an
# overfitted model drives training residuals towards zero and therefore towards
# a spuriously perfect spatial interpretability.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
source(file.path(R_DIR, "05-models.R"))

log_step("Step 2/6  Fit models and extract residuals")

df <- read_result(F_ANALYSIS_DATA)
predictors <- resolve_predictors(df, response = RESPONSE)

# -- Split --------------------------------------------------------------------

df$split <- make_split(df, response = RESPONSE)
n_train <- sum(df$split == "train"); n_test <- sum(df$split == "test")
log_info("stratified split: %d training / %d test (%.0f%% / %.0f%%)",
         n_train, n_test, 100 * n_train / nrow(df), 100 * n_test / nrow(df))
write_result(df[, c("lon", "lat", RESPONSE, "split")], F_SPLIT)

x_train <- df[df$split == "train", predictors, drop = FALSE]
y_train <- df[df$split == "train", RESPONSE]
x_test  <- df[df$split == "test",  predictors, drop = FALSE]
y_test  <- df[df$split == "test",  RESPONSE]

# -- Fit ----------------------------------------------------------------------

models <- available_models(MODELS)
log_info("fitting %d model(s): %s", length(models), paste(models, collapse = ", "))

predictions <- list()
for (m in models) {
  t0 <- Sys.time()
  pred <- fit_predict(m, x_train, y_train, x_test)
  if (is.null(pred) || length(pred) != length(y_test) || !all(is.finite(pred))) {
    log_warn("%s produced no usable prediction and is excluded", m)
    next
  }
  predictions[[m]] <- pred
  log_info("%-10s done in %.1fs", m, as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

if (!length(predictions)) stop("No model produced predictions; nothing to evaluate.")

# -- Residuals and accuracy ---------------------------------------------------

residuals <- lapply(predictions, function(p) y_test - p)

pred_df <- data.frame(lon = df$lon[df$split == "test"],
                      lat = df$lat[df$split == "test"],
                      observed = y_test, as.data.frame(predictions, check.names = FALSE))
resid_df <- data.frame(lon = df$lon[df$split == "test"],
                       lat = df$lat[df$split == "test"],
                       observed = y_test, as.data.frame(residuals, check.names = FALSE))
write_result(pred_df, F_PREDICTIONS)
write_result(resid_df, F_RESIDUALS)

acc <- do.call(rbind, lapply(names(predictions), function(m) {
  cbind(model = m, accuracy_metrics(y_test, predictions[[m]]))
}))
acc <- acc[order(-acc$R2), ]
write_result(acc, F_ACCURACY, digits = 4)

write_result(model_table(names(predictions)), file.path(TAB_DIR, "table-models.csv"))

log_info("best R2: %s (%.3f) | lowest RMSE: %s (%.3f)",
         acc$model[1], acc$R2[1],
         acc$model[which.min(acc$RMSE)], min(acc$RMSE))
