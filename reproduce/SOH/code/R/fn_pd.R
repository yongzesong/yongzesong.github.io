# =============================================================================
# fn_pd.R — stratification and the power of determinant
# =============================================================================
# Implements Equations 5 and 6 of Ren et al. (2026): a CART regression tree
# partitions the analysis units into strata, and the geographical detector
# q statistic measures how much of the variance of the response is explained by
# that partition.
#
#   PD = q = 1 - sum_h ( N_h * var_h ) / ( N * var )
#
# The significance test follows Wang et al. (2010): a non-central F test with
# L-1 and N-L degrees of freedom and non-centrality parameter lambda.
#
# The reference implementation calls GD::gd(). This file reproduces the same
# statistic in base R so the template runs with no extra package, and
# soh_pd_backend() switches to GD::gd() automatically when that package is
# installed, which lets a user verify the two agree.
# =============================================================================

#' Power of determinant of a stratification
#'
#' @param y numeric response
#' @param strata vector of stratum labels, same length as y
#' @param min_stratum_n strata smaller than this are pooled into one residual
#'   stratum, which avoids zero-variance strata inflating q
#' @return list(q, p_value, n_strata, n)
soh_pd <- function(y, strata, min_stratum_n = 2) {
  ok <- is.finite(y) & !is.na(strata)
  y <- y[ok]; strata <- as.character(strata[ok])
  N <- length(y)
  if (N < 3) return(list(q = NA_real_, p_value = NA_real_, n_strata = NA_integer_, n = N))

  tab <- table(strata)
  small <- names(tab)[tab < min_stratum_n]
  if (length(small)) strata[strata %in% small] <- "_pooled_"

  sp <- split(y, strata)
  Nh <- vapply(sp, length, integer(1))
  vh <- vapply(sp, function(v) if (length(v) < 2) 0 else stats::var(v), numeric(1))
  mh <- vapply(sp, mean, numeric(1))
  L <- length(sp)

  SST <- N * stats::var(y)
  SSW <- sum(Nh * vh)
  if (!is.finite(SST) || SST <= 0) {
    return(list(q = NA_real_, p_value = NA_real_, n_strata = L, n = N))
  }
  q <- 1 - SSW / SST

  # non-central F test (Wang et al. 2010)
  p <- NA_real_
  if (L > 1 && N > L && q > 0 && q < 1) {
    Fv <- ((N - L) / (L - 1)) * (q / (1 - q))
    sigma2 <- stats::var(y)
    lambda <- (sum(mh^2) - (sum(sqrt(Nh) * mh))^2 / N) / sigma2
    lambda <- max(lambda, 0)
    p <- stats::pf(Fv, df1 = L - 1, df2 = N - L, ncp = lambda, lower.tail = FALSE)
  } else if (isTRUE(all.equal(q, 0))) {
    p <- 1
  }

  list(q = q, p_value = p, n_strata = L, n = N)
}

#' CART stratification of the response by one or more predictors
#'
#' Terminal nodes of the regression tree become the strata (Equation 5). The
#' number of strata is data adaptive rather than fixed in advance.
soh_stratify <- function(y, X, cfg) {
  X <- as.data.frame(X)
  X <- X[, vapply(X, function(v) is.finite(stats::sd(as.numeric(v))) &&
                    stats::sd(as.numeric(v)) > 0, logical(1)), drop = FALSE]
  if (!ncol(X)) return(rep("1", length(y)))
  dt <- data.frame(X, .y = y)
  tree <- rpart::rpart(
    .y ~ ., data = dt,
    control = rpart::rpart.control(
      cp = cfg$pd$cart_cp,
      minsplit = cfg$pd$cart_minsplit,
      minbucket = cfg$pd$cart_minbucket
    )
  )
  as.character(as.numeric(tree$where))
}

#' Stratify then detect: the full PD of a predictor set
#'
#' @param y response vector
#' @param X data.frame of predictors (one variable, an SOP block, or any union)
#' @return data.frame with one row: q, p_value, n_strata, n_predictors
soh_pd_of <- function(y, X, cfg, label = NA_character_) {
  X <- as.data.frame(X)
  strata <- soh_stratify(y, X, cfg)
  r <- soh_pd(y, strata, cfg$pd$min_stratum_n)
  data.frame(
    label = label,
    qv = r$q,
    sig = r$p_value,
    n_strata = r$n_strata,
    n_predictors = ncol(X),
    stringsAsFactors = FALSE
  )
}

#' Verify the native q statistic against the GD package, when available
#'
#' Returns a comparison data.frame, or NULL with a message if GD is absent.
soh_pd_verify_against_GD <- function(y, X, cfg) {
  if (!requireNamespace("GD", quietly = TRUE)) {
    message("Package GD is not installed; the native q statistic cannot be ",
            "cross-checked. install.packages('GD') to enable this check.")
    return(invisible(NULL))
  }
  strata <- soh_stratify(y, X, cfg)
  native <- soh_pd(y, strata, cfg$pd$min_stratum_n)
  dt <- data.frame(tree = strata, y = y)
  gdr <- GD::gd(y ~ tree, data = dt)
  data.frame(
    source = c("native", "GD::gd"),
    qv  = c(native$q, as.numeric(gdr$Factor$qv[1])),
    sig = c(native$p_value, as.numeric(gdr$Factor$sig[1]))
  )
}

# --- interaction classification ---------------------------------------------

#' Classify a two-factor interaction in geographical detector terms
#'
#' @param q1,q2 PD of each factor alone
#' @param q12 PD of the two factors jointly
soh_interaction_type <- function(q1, q2, q12) {
  lo <- min(q1, q2); hi <- max(q1, q2); sm <- q1 + q2
  if (is.na(q12)) return(NA_character_)
  if (q12 < lo) return("weaken, nonlinear")
  if (q12 >= lo && q12 < hi) return("weaken, univariate")
  if (q12 > sm) return("enhance, nonlinear")
  if (isTRUE(all.equal(q12, sm))) return("independent")
  "enhance, bivariate"
}

# --- Moran's I ---------------------------------------------------------------

#' Global Moran's I of the response over analysis unit centroids
#'
#' Uses k nearest neighbours rather than polygon contiguity, so it works on
#' point-supported analysis units. Supply a contiguity listw via `listw` to
#' reproduce a polygon-based Moran's I instead.
soh_morans_i <- function(points, k = 8, nsim = 999, listw = NULL, seed = 1) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    message("Package spdep is not installed; Moran's I skipped.")
    return(NULL)
  }
  set.seed(seed)
  y <- points$response
  if (is.null(listw)) {
    xy <- as.matrix(points[, c("x", "y")])
    nb <- spdep::knn2nb(spdep::knearneigh(xy, k = k))
    listw <- spdep::nb2listw(nb, style = "W")
  }
  mc <- spdep::moran.mc(y, listw, nsim = nsim)
  lag <- spdep::lag.listw(listw, as.numeric(scale(y)))
  list(
    statistic = as.numeric(mc$statistic),
    p_value = mc$p.value,
    n_sim = nsim,
    k = k,
    sim = as.numeric(mc$res),
    scatter = data.frame(z = as.numeric(scale(y)), lag = lag)
  )
}
