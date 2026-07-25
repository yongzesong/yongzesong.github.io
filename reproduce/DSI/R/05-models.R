# =============================================================================
# 05-models.R — model registry and train/test protocol
# =============================================================================
# Both source papers fit ten models through caret. This template keeps the same
# model families but calls the underlying packages directly, so a project runs
# with whatever is installed and skips the rest with a message instead of
# failing. Set USE_CARET <- TRUE in config/project-config.R to route every fit
# through caret when it is available and reproduce the papers' tuning exactly.
#
# Each registry entry declares the package it needs, a fit function and a
# predict function. Adding a model means adding one entry.
# =============================================================================

MODEL_REGISTRY <- list(

  lm = list(
    label = "Linear regression", type = "Linear model", pkg = NULL,
    fit = function(x, y, ...) stats::lm(y ~ ., data = data.frame(y = y, x)),
    predict = function(m, x) stats::predict(m, newdata = data.frame(x))
  ),

  rpart = list(
    label = "Regression tree", type = "Tree-based model", pkg = "rpart",
    fit = function(x, y, ...) rpart::rpart(y ~ ., data = data.frame(y = y, x)),
    predict = function(m, x) stats::predict(m, newdata = data.frame(x))
  ),

  rf = list(
    label = "Random forest", type = "Tree-based model", pkg = "ranger",
    fit = function(x, y, seed = SEED, ...)
      ranger::ranger(y ~ ., data = data.frame(y = y, x), num.trees = 500, seed = seed),
    predict = function(m, x) stats::predict(m, data = data.frame(x))$predictions
  ),

  xgbTree = list(
    label = "Extreme gradient boosting", type = "Tree-based model", pkg = "xgboost",
    # xgb.train on a DMatrix rather than the xgboost() wrapper, whose argument
    # names changed in xgboost 3.x.
    fit = function(x, y, seed = SEED, ...) {
      set.seed(seed)
      dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = y)
      xgboost::xgb.train(
        params = list(objective = "reg:squarederror", max_depth = 6,
                      eta = 0.1, subsample = 0.8, nthread = 2),
        data = dtrain, nrounds = 200, verbose = 0)
    },
    predict = function(m, x) stats::predict(m, xgboost::xgb.DMatrix(as.matrix(x)))
  ),

  gbm = list(
    label = "Gradient boosting machine", type = "Tree-based model", pkg = "gbm",
    fit = function(x, y, seed = SEED, ...) {
      set.seed(seed)
      gbm::gbm(y ~ ., data = data.frame(y = y, x), distribution = "gaussian",
               n.trees = 500, interaction.depth = 3, shrinkage = 0.05,
               verbose = FALSE)
    },
    predict = function(m, x) stats::predict(m, newdata = data.frame(x), n.trees = 500)
  ),

  cubist = list(
    label = "Cubist rule-based model", type = "Rule-based model", pkg = "Cubist",
    fit = function(x, y, ...) Cubist::cubist(x = as.data.frame(x), y = y, committees = 1),
    predict = function(m, x) stats::predict(m, newdata = as.data.frame(x))
  ),

  gam = list(
    label = "Generalised additive model", type = "Non-linear model", pkg = "mgcv",
    fit = function(x, y, ...) {
      # Smooth every predictor with enough distinct values; leave the rest linear.
      terms <- vapply(names(x), function(v) {
        if (length(unique(x[[v]])) >= 10) sprintf("s(%s, k = 5)", v) else v
      }, character(1))
      mgcv::gam(stats::as.formula(paste("y ~", paste(terms, collapse = " + "))),
                data = data.frame(y = y, x))
    },
    predict = function(m, x) as.numeric(stats::predict(m, newdata = data.frame(x)))
  ),

  earth = list(
    label = "Multivariate adaptive regression splines", type = "Non-linear model",
    pkg = "earth",
    fit = function(x, y, ...) earth::earth(x = as.data.frame(x), y = y),
    predict = function(m, x) as.numeric(stats::predict(m, newdata = as.data.frame(x)))
  ),

  knn = list(
    label = "k-nearest neighbours", type = "Neighbourhood-based model", pkg = "FNN",
    fit = function(x, y, ...) list(x = scale(as.matrix(x)), y = y),
    predict = function(m, x) {
      xt <- scale(as.matrix(x), center = attr(m$x, "scaled:center"),
                  scale = attr(m$x, "scaled:scale"))
      FNN::knn.reg(train = m$x, test = xt, y = m$y, k = 5)$pred
    }
  ),

  pls = list(
    label = "Partial least squares regression", type = "Dimension reduction model",
    pkg = "pls",
    fit = function(x, y, ...) pls::plsr(y ~ ., data = data.frame(y = y, x),
                                        ncomp = min(5, ncol(x)), validation = "none"),
    predict = function(m, x) as.numeric(stats::predict(m, newdata = data.frame(x),
                                                       ncomp = m$ncomp))
  ),

  svmRadial = list(
    label = "Support vector machine (radial)", type = "Kernel model", pkg = "kernlab",
    fit = function(x, y, seed = SEED, ...) {
      set.seed(seed)
      kernlab::ksvm(as.matrix(x), y, kernel = "rbfdot", type = "eps-svr")
    },
    predict = function(m, x) as.numeric(kernlab::predict(m, as.matrix(x)))
  )
)

#' Which requested models can actually run here.
available_models <- function(requested = MODELS) {
  unknown <- setdiff(requested, names(MODEL_REGISTRY))
  if (length(unknown)) {
    log_warn("not in the registry, ignored: %s", paste(unknown, collapse = ", "))
    requested <- intersect(requested, names(MODEL_REGISTRY))
  }
  ok <- vapply(requested, function(m) {
    pkg <- MODEL_REGISTRY[[m]]$pkg
    is.null(pkg) || has_pkg(pkg)
  }, logical(1))
  if (any(!ok)) {
    skipped <- requested[!ok]
    log_warn("skipped (package not installed): %s",
             paste(sprintf("%s [%s]", skipped,
                           vapply(skipped, function(m) MODEL_REGISTRY[[m]]$pkg, character(1))),
                   collapse = ", "))
    log_info("install them with the commands in env/requirements.md to match the published model set")
  }
  requested[ok]
}

#' Stratified train/test split.
#'
#' Stratifying on quantiles of the response keeps its distribution comparable
#' across the two sets, which matters because DSI is measured on the test set
#' and a test set missing the high tail would understate delta_0.
make_split <- function(df, response = RESPONSE, train_fraction = TRAIN_FRACTION,
                       seed = SEED) {
  set.seed(seed)
  y <- df[[response]]
  bins <- cut(y, breaks = unique(stats::quantile(y, seq(0, 1, 0.1), na.rm = TRUE)),
              include.lowest = TRUE, labels = FALSE)
  idx_train <- unlist(lapply(split(seq_along(y), bins), function(i) {
    sample(i, size = max(1, floor(length(i) * train_fraction)))
  }), use.names = FALSE)
  split <- rep("test", length(y))
  split[idx_train] <- "train"
  split
}

#' Fit one model and predict on the evaluation set.
fit_predict <- function(model_name, x_train, y_train, x_test, seed = SEED) {
  spec <- MODEL_REGISTRY[[model_name]]
  fitted <- tryCatch(spec$fit(x_train, y_train, seed = seed),
                     error = function(e) {
                       log_warn("%s failed to fit: %s", model_name, conditionMessage(e))
                       NULL
                     })
  if (is.null(fitted)) return(NULL)
  pred <- tryCatch(as.numeric(spec$predict(fitted, x_test)),
                   error = function(e) {
                     log_warn("%s failed to predict: %s", model_name, conditionMessage(e))
                     NULL
                   })
  pred
}

#' Model metadata for the manuscript's model table.
model_table <- function(models) {
  data.frame(
    model = models,
    label = vapply(models, function(m) MODEL_REGISTRY[[m]]$label, character(1)),
    type  = vapply(models, function(m) MODEL_REGISTRY[[m]]$type, character(1)),
    row.names = NULL
  )
}
