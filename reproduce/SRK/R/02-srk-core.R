# =============================================================================
# 02-srk-core.R — the SRK method in one file
#
# Ren K, Song Y, Chen M, Yu Q (2026). A singularity regression kriging for
# spatial prediction. GIScience & Remote Sensing 63(1):2690341.
# doi:10.1080/15481603.2026.2690341
#
# Three steps, in the paper's order and with the paper's equation numbers:
#
#   1. srk_intensity()  + srk_alpha()   local covariate intensity C_k(A(s,r))
#                                       over a ladder of scales (Eq. 1-2), then
#                                       the singularity index as the log-log
#                                       slope plus two (Eq. 3-4).
#   2. srk_trend()                      random forest on the augmented feature
#                                       set F(s) (Eq. 5-6) and its residuals
#                                       (Eq. 7).
#   3. srk_krige_residuals()            ordinary kriging of those residuals
#                                       (Eq. 8) added back to the trend (Eq. 9),
#                                       with the kriging standard error (Eq. 10).
#
# srk_run() chains the three and is what every pipeline step calls. The five
# benchmark runners (OK, IDW, LM, RF, RFK) share its interface so the
# cross-validation loop can treat all six models identically.
#
# Ported from the authors' released scripts (singularity.R, 01_precompute_
# singularity.R, 02_main_analysis.R at github.com/renkaigis/Singularity_
# Regression_Kriging). Departures from those scripts are marked DEPARTURE and
# are all optional — the defaults in config/project-config.R reproduce the
# released behaviour.
# =============================================================================


# -- Step 1a. Local covariate intensity, Eq. 2 --------------------------------
#' Mean absolute covariate value inside a square window, at every scale.
#'
#' Returns an n_eval x n_scale matrix of C_k(A(s, r)). Reference points are the
#' locations where the covariate is observed; evaluation points are where the
#' index is wanted — the two coincide when singularity is computed at the
#' sample locations themselves.
#'
#' The window is a square of half-width r, i.e. the Chebyshev ball of radius r,
#' matching `abs(x_ref - x_eval) <= r & abs(y_ref - y_eval) <= r` in the
#' authors' singularity.R. A scale carrying fewer than `min_pts_per_scale`
#' reference points is returned as NA and drops out of the regression.
srk_intensity <- function(x_ref, y_ref, z_ref, x_eval, y_eval, scales,
                          min_pts_per_scale = 3L, use_abs = TRUE,
                          chunk = 2000L) {
  stopifnot(length(x_ref) == length(y_ref), length(x_ref) == length(z_ref),
            length(x_eval) == length(y_eval), length(scales) >= 1L)
  if (use_abs) z_ref <- abs(z_ref)
  n_eval <- length(x_eval)
  J      <- length(scales)
  C      <- matrix(NA_real_, n_eval, J,
                   dimnames = list(NULL, paste0("r", seq_len(J))))
  # Chunked so a large evaluation set never allocates one huge distance matrix.
  for (ii in split(seq_len(n_eval), ceiling(seq_len(n_eval) / chunk))) {
    d <- pmax(abs(outer(x_eval[ii], x_ref, "-")),
              abs(outer(y_eval[ii], y_ref, "-")))
    for (j in seq_len(J)) {
      inw <- d <= scales[j]
      cnt <- rowSums(inw)
      val <- as.numeric(inw %*% z_ref) / cnt
      val[cnt < min_pts_per_scale] <- NA_real_
      C[ii, j] <- val
    }
  }
  C
}


# -- Step 1b. The singularity index, Eq. 3-4 ----------------------------------
#' alpha_k(s) = OLS slope of log C_k(A(s,r)) on log r, plus two.
#'
#' Locations with fewer than `min_valid_scales` usable scales get NA, which the
#' caller replaces with the neutral value 2 (a spatially uniform field).
srk_alpha <- function(C, scales, min_valid_scales = 2L, eps = 1e-9) {
  logr <- log(scales)
  vapply(seq_len(nrow(C)), function(i) {
    cr <- C[i, ]
    ok <- is.finite(cr)
    if (sum(ok) < min_valid_scales) return(NA_real_)
    # log(0) and log(<0) are impossible to fit; the authors substitute eps.
    yv <- log(ifelse(cr[ok] > 0, cr[ok], eps))
    xv <- logr[ok]
    xd <- xv - mean(xv)
    den <- sum(xd * xd)
    if (den <= 0) return(NA_real_)
    sum(xd * (yv - mean(yv))) / den + 2
  }, numeric(1))
}


#' Convenience wrapper: intensity then slope, for one covariate.
srk_singularity <- function(x_ref, y_ref, z_ref, x_eval, y_eval, scales,
                            min_pts_per_scale = 3L, min_valid_scales = 2L,
                            use_abs = TRUE, neutral = 2.0) {
  C <- srk_intensity(x_ref, y_ref, z_ref, x_eval, y_eval, scales,
                     min_pts_per_scale = min_pts_per_scale, use_abs = use_abs)
  a <- srk_alpha(C, scales, min_valid_scales = min_valid_scales)
  a[!is.finite(a)] <- neutral
  a
}


#' Drop singularity features that barely vary — Sec. 2.2.1's SD filter.
#' Returns the names that survive; `sd_threshold = 0` keeps everything.
srk_select_features <- function(data, sv_cols, sd_threshold = 0.5) {
  keep <- vapply(sv_cols, function(cc) stats::sd(data[[cc]], na.rm = TRUE),
                 numeric(1))
  sv_cols[is.finite(keep) & keep >= sd_threshold]
}


# -- Step 2. Random-forest trend, Eq. 5-7 -------------------------------------
#' Fit g() on the augmented feature set and return trend + residuals.
#'
#' DEPARTURE: `use_coords` reproduces the authors' case-study scripts, which
#' pass x and y to ranger alongside the covariates; the paper's Eq. 5 defines
#' F(s) without coordinates. Set RF_USE_COORDS = FALSE to follow the equation.
#' DEPARTURE: `seed` is fixed here. The released scripts call ranger() without
#' a seed, so their SRK, RF and RFK numbers move between runs — quantified in
#' R/50-seed-stability.R.
srk_trend <- function(train, test, yvar, features, ntree = 500L, seed = NULL,
                      use_coords = TRUE, coord_cols = c("x", "y")) {
  feats <- if (use_coords) c(coord_cols, features) else features
  args  <- list(formula = stats::reformulate(feats, yvar), data = train,
                num.trees = ntree, importance = "impurity")
  if (!is.null(seed)) args$seed <- seed
  fit <- do.call(ranger::ranger, args)
  list(fit        = fit,
       features   = feats,
       trend_tr   = stats::predict(fit, data = train)$predictions,
       trend_te   = stats::predict(fit, data = test)$predictions,
       importance = fit$variable.importance)
}


# -- Step 3. Ordinary kriging of the residuals, Eq. 8-10 ----------------------
#' Krige a residual column from `train` onto `test`.
#'
#' Returns the kriged residual, the kriging standard error (Eq. 10) and the
#' fitted variogram. `fitter = "automap"` is the published choice: autoKrige
#' picks among automap's candidate families under an isotropic assumption.
srk_krige_residuals <- function(train, test, resid_col = "resid",
                                coord_cols = c("x", "y"), fitter = "automap") {
  tr <- as.data.frame(train)
  te <- as.data.frame(test)
  tr <- tr[is.finite(tr[[resid_col]]), , drop = FALSE]
  if (nrow(tr) < 5L)
    return(list(pred = rep(NA_real_, nrow(te)), se = rep(NA_real_, nrow(te)),
                model = NULL))
  sp::coordinates(tr) <- stats::as.formula(
    paste("~", paste(coord_cols, collapse = "+")))
  sp::coordinates(te) <- stats::as.formula(
    paste("~", paste(coord_cols, collapse = "+")))
  f <- stats::as.formula(paste(resid_col, "~ 1"))
  if (identical(fitter, "automap")) {
    ak <- automap::autoKrige(f, input_data = tr, new_data = te)
    list(pred  = ak$krige_output$var1.pred,
         se    = sqrt(pmax(0, ak$krige_output$var1.var)),
         model = ak$var_model)
  } else {
    v  <- gstat::variogram(f, tr)
    vm <- gstat::fit.variogram(v, gstat::vgm("Sph"))
    kr <- gstat::krige(f, tr, te, model = vm)
    list(pred = kr$var1.pred, se = sqrt(pmax(0, kr$var1.var)), model = vm)
  }
}


# -- The model ----------------------------------------------------------------
#' Singularity regression kriging, end to end.
#'
#' `sv` is an optional pre-computed data frame of singularity features carrying
#' the coordinate columns plus one sv_<covariate> column each; supplying it
#' avoids recomputing indices inside every cross-validation fold. When it is
#' NULL the indices are computed from the union of train and test covariate
#' values, which is legitimate because covariates — unlike the response — are
#' observable at prediction locations (paper Sec. 2.2.1).
srk_run <- function(train, test, yvar, xvars,
                    sv = NULL,
                    scales = SV_SCALES,
                    min_pts_per_scale = MIN_PTS_PER_SCALE,
                    min_valid_scales  = MIN_VALID_SCALES,
                    sd_threshold      = SV_SD_THRESHOLD,
                    ntree = RF_NTREE, seed = RF_SEED,
                    use_coords = RF_USE_COORDS,
                    fitter = VARIOGRAM_FIT,
                    neutral = SV_NEUTRAL) {
  tr <- as.data.frame(train)
  te <- as.data.frame(test)
  sv_cols <- paste0("sv_", xvars)

  if (is.null(sv)) {
    all_x <- c(tr$x, te$x); all_y <- c(tr$y, te$y)
    n_tr  <- nrow(tr)
    for (j in seq_along(xvars)) {
      a <- srk_singularity(all_x, all_y, c(tr[[xvars[j]]], te[[xvars[j]]]),
                           all_x, all_y, scales,
                           min_pts_per_scale = min_pts_per_scale,
                           min_valid_scales  = min_valid_scales,
                           neutral = neutral)
      tr[[sv_cols[j]]] <- a[seq_len(n_tr)]
      te[[sv_cols[j]]] <- a[n_tr + seq_len(nrow(te))]
    }
  } else {
    key <- function(d) paste(d$x, d$y, sep = "_")
    m_tr <- match(key(tr), key(sv)); m_te <- match(key(te), key(sv))
    if (anyNA(m_tr) || anyNA(m_te))
      stop("pre-computed singularity does not cover every location")
    for (cc in sv_cols) {
      tr[[cc]] <- sv[[cc]][m_tr]
      te[[cc]] <- sv[[cc]][m_te]
    }
  }
  for (cc in sv_cols) {
    tr[[cc]][!is.finite(tr[[cc]])] <- neutral
    te[[cc]][!is.finite(te[[cc]])] <- neutral
  }

  sv_keep <- srk_select_features(tr, sv_cols, sd_threshold)
  tri <- srk_trend(tr, te, yvar, c(xvars, sv_keep), ntree = ntree,
                   seed = seed, use_coords = use_coords)
  tr$resid <- tr[[yvar]] - tri$trend_tr
  kr <- srk_krige_residuals(tr, te, "resid", fitter = fitter)

  list(model      = "SRK",
       pred       = tri$trend_te + kr$pred,
       trend      = tri$trend_te,
       kriged     = kr$pred,
       se         = kr$se,
       resid      = tr$resid,
       sv_kept    = sv_keep,
       sv_dropped = setdiff(sv_cols, sv_keep),
       sv_sd      = vapply(sv_cols, function(cc) stats::sd(tr[[cc]]), numeric(1)),
       importance = tri$importance,
       vgm        = kr$model)
}


# -- Benchmarks, paper Sec. 3.2.3 ---------------------------------------------
bench_ok <- function(train, test, yvar, xvars = NULL, fitter = VARIOGRAM_FIT, ...) {
  tr <- as.data.frame(train); te <- as.data.frame(test)
  sp::coordinates(tr) <- ~x + y; sp::coordinates(te) <- ~x + y
  f <- stats::as.formula(paste(yvar, "~ 1"))
  if (identical(fitter, "automap")) {
    ak <- automap::autoKrige(f, input_data = tr, new_data = te)
    list(model = "OK", pred = ak$krige_output$var1.pred,
         se = sqrt(pmax(0, ak$krige_output$var1.var)), vgm = ak$var_model)
  } else {
    v  <- gstat::variogram(f, tr)
    vm <- gstat::fit.variogram(v, gstat::vgm("Sph"))
    kr <- gstat::krige(f, tr, te, model = vm)
    list(model = "OK", pred = kr$var1.pred, se = sqrt(pmax(0, kr$var1.var)),
         vgm = vm)
  }
}

bench_idw <- function(train, test, yvar, xvars = NULL, idp = 2, ...) {
  tr <- as.data.frame(train); te <- as.data.frame(test)
  sp::coordinates(tr) <- ~x + y; sp::coordinates(te) <- ~x + y
  z <- gstat::idw(stats::as.formula(paste(yvar, "~ 1")),
                  locations = tr, newdata = te, idp = idp, debug.level = 0)
  list(model = "IDW", pred = z$var1.pred)
}

bench_lm <- function(train, test, yvar, xvars, ...) {
  fit <- stats::lm(stats::reformulate(xvars, yvar), data = as.data.frame(train))
  list(model = "LM", pred = stats::predict(fit, newdata = as.data.frame(test)),
       fit = fit)
}

bench_rf <- function(train, test, yvar, xvars, ntree = RF_NTREE, seed = RF_SEED,
                     use_coords = RF_USE_COORDS, ...) {
  tri <- srk_trend(as.data.frame(train), as.data.frame(test), yvar, xvars,
                   ntree = ntree, seed = seed, use_coords = use_coords)
  list(model = "RF", pred = tri$trend_te, importance = tri$importance)
}

bench_rfk <- function(train, test, yvar, xvars, ntree = RF_NTREE, seed = RF_SEED,
                      use_coords = RF_USE_COORDS, fitter = VARIOGRAM_FIT, ...) {
  tr <- as.data.frame(train); te <- as.data.frame(test)
  tri <- srk_trend(tr, te, yvar, xvars, ntree = ntree, seed = seed,
                   use_coords = use_coords)
  tr$resid <- tr[[yvar]] - tri$trend_tr
  kr <- srk_krige_residuals(tr, te, "resid", fitter = fitter)
  list(model = "RFK", pred = tri$trend_te + kr$pred, trend = tri$trend_te,
       se = kr$se, importance = tri$importance, vgm = kr$model)
}

# Name -> runner. The cross-validation loop iterates over this list, so adding
# a benchmark to the comparison is one entry here.
SRK_MODELS <- list(OK = bench_ok, IDW = bench_idw, LM = bench_lm,
                   RF = bench_rf, RFK = bench_rfk, SRK = srk_run)


# -- Accuracy metrics, Eq. 11-13 ----------------------------------------------
srk_metrics <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]; pred <- pred[ok]
  if (length(obs) < 3L)
    return(data.frame(R2 = NA_real_, RMSE = NA_real_, MAE = NA_real_))
  data.frame(R2   = 1 - sum((pred - obs)^2) / sum((obs - mean(obs))^2),
             RMSE = sqrt(mean((pred - obs)^2)),
             MAE  = mean(abs(pred - obs)))
}


# -- Spatial block cross-validation, paper Sec. 3.2.3 -------------------------
#' Square blocks of `block_size` map units are shuffled and dealt round-robin
#' to `k` folds, so a held-out fold is a set of whole blocks and no test point
#' sits next to its own training neighbours.
srk_block_folds <- function(data, k = 5L, block_size = 15000, seed = 123L) {
  df <- as.data.frame(data)
  bx <- floor((df$x - min(df$x, na.rm = TRUE)) / block_size)
  by <- floor((df$y - min(df$y, na.rm = TRUE)) / block_size)
  block <- paste(bx, by, sep = "_")
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv)
  set.seed(seed)
  ub <- sample(unique(block))
  if (!is.null(old)) assign(".Random.seed", old, .GlobalEnv)
  fold <- as.integer(stats::setNames(((seq_along(ub) - 1L) %% k) + 1L, ub)[block])
  attr(fold, "n_blocks") <- length(ub)
  fold
}
