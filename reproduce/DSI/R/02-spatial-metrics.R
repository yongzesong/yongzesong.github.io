# =============================================================================
# 02-spatial-metrics.R — the spatial characteristics DSI and DG are built from
# =============================================================================
# Three measurements live here, each applied twice: once to the response field
# and once to a model's residual field.
#
#   spatial autocorrelation  -> Moran's I        (global, delta^a)
#   spatial heterogeneity    -> Q statistic      (global, delta^h)
#   local spatial complexity -> geocomplexity    (local,  G)
#
# Nothing in this file knows about DSI or DG. Keeping the measurements separate
# from the indicators is what lets a new project add a fourth characteristic
# without touching 03-dsi.R.
# =============================================================================

# -- Neighbourhood ------------------------------------------------------------

#' Build k-nearest-neighbour spatial weights in a metric CRS.
#'
#' @param coords matrix of projected coordinates from projected_coords()
#' @param k number of neighbours
#' @param style weighting style; "W" is row-standardised
build_knn_weights <- function(coords, k = K_MORAN, style = "W") {
  n <- nrow(coords)
  if (k >= n) {
    stop("k_neighbors (", k, ") must be smaller than the number of points (", n, ").")
  }
  if (k > 30) log_warn("k = %d is large; neighbourhoods may span the study area", k)
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k))
  spdep::nb2listw(nb, style = style, zero.policy = TRUE)
}

# -- Spatial autocorrelation --------------------------------------------------

#' Global Moran's I with its significance test.
#'
#' Returns the estimate and the p value together, because a delta^a that is not
#' significant makes the corresponding DSI value uninterpretable rather than
#' merely small.
moran_i <- function(x, listw, alternative = MORAN_ALTERNATIVE) {
  if (all(abs(x - mean(x, na.rm = TRUE)) < .Machine$double.eps^0.5)) {
    return(list(estimate = NA_real_, p_value = NA_real_))
  }
  mt <- spdep::moran.test(x, listw, alternative = alternative, zero.policy = TRUE)
  list(estimate = unname(mt$estimate[1]), p_value = unname(mt$p.value))
}

# -- Spatial heterogeneity ----------------------------------------------------

#' Strata used for the Q statistic.
#'
#' Both methods partition space using the explanatory variables, never the
#' response: strata cut from quantiles of the response would drive the Q value
#' of the response towards 1 by construction and make eta meaningless.
#'
#' "tree" reproduces the DSI paper: a single regression tree fitted to the
#' response defines strata from the predictors, and every model is then scored
#' against those same strata so that Q values stay comparable across models.
#' "kmeans" clusters the standardised predictors instead, and serves both as
#' the fallback when a tree degenerates to one leaf and as the alternative
#' partition used by the sensitivity step.
make_strata <- function(df, response = RESPONSE, predictors,
                        method = STRATIFY_METHOD, n_strata = STRATIFY_K,
                        seed = SEED) {
  method <- match.arg(method, c("tree", "kmeans"))
  d <- df[, c(response, predictors), drop = FALSE]
  names(d) <- make.names(names(d))
  resp_safe <- make.names(response)

  if (method == "tree") {
    fit <- rpart::rpart(stats::as.formula(paste(resp_safe, "~ .")), data = d)
    strata <- as.factor(as.numeric(fit$where))
    if (nlevels(strata) < 2) {
      log_warn("regression tree produced a single leaf; falling back to kmeans strata")
      method <- "kmeans"
    } else {
      return(structure(strata, method = "tree", n_strata = nlevels(strata)))
    }
  }

  set.seed(seed)
  x <- scale(as.matrix(df[, predictors, drop = FALSE]))
  x[!is.finite(x)] <- 0
  k <- min(n_strata, max(2, floor(nrow(df) / 10)))
  km <- stats::kmeans(x, centers = k, nstart = 25, iter.max = 50)
  strata <- as.factor(km$cluster)
  structure(strata, method = "kmeans", n_strata = nlevels(strata))
}

#' Q statistic of the geographical detector.
#'
#' q = 1 - SSW / SST, with SSW the within-stratum sum of squares and SST the
#' total sum of squares. This sum-of-squares form reproduces the delta^h values
#' published in Table 3 of Liu et al. (2026); see tests/test-01. The variance
#' form (sum of n_h * var_h with sample variances) gives systematically lower
#' values and does not reproduce the published numbers.
q_statistic <- function(x, strata) {
  strata <- as.factor(strata)
  keep <- is.finite(x) & !is.na(strata)
  x <- x[keep]; strata <- droplevels(strata[keep])
  sst <- sum((x - mean(x))^2)
  if (sst <= 0) return(NA_real_)
  ssw <- sum(vapply(split(x, strata), function(v) sum((v - mean(v))^2), numeric(1)))
  1 - ssw / sst
}

#' Significance of the Q statistic by permutation.
#'
#' The geographical detector's F test assumes a form the residual field often
#' violates, so the template permutes stratum labels instead.
q_significance <- function(x, strata, n_perm = 999, seed = SEED) {
  set.seed(seed)
  observed <- q_statistic(x, strata)
  if (is.na(observed)) return(NA_real_)
  null_q <- replicate(n_perm, q_statistic(x, sample(strata)))
  (sum(null_q >= observed, na.rm = TRUE) + 1) / (n_perm + 1)
}

# -- Local spatial complexity -------------------------------------------------

#' Local geocomplexity of a field.
#'
#' Wraps geocomplexity::geocd_vector(), the reference implementation of the
#' local complexity measure of Zhang et al. (2023) that the DG paper builds on.
#' DG uses the unnormalised values, which are signed and unbounded; the
#' normalised values in [0, 1] are what the DG paper maps.
#'
#' @param normalize FALSE for DG computation, TRUE for mapping
geocomplexity_field <- function(values, coords, k = K_GEOC, normalize = FALSE) {
  if (!has_pkg("geocomplexity")) {
    stop("Package 'geocomplexity' is required for the DG module. ",
         "Install it, or set RUN_DG <- FALSE in config/project-config.R.")
  }
  pts <- sf::st_as_sf(data.frame(value = values, X = coords[, 1], Y = coords[, 2]),
                      coords = c("X", "Y"), crs = PROJECTED_CRS)
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k))
  wt <- spdep::nb2mat(nb, style = "W", zero.policy = TRUE)
  out <- geocomplexity::geocd_vector(pts["value"], wt = wt, method = "moran",
                                     normalize = normalize, returnsf = FALSE)
  as.numeric(out[[1]])
}

# -- One-call measurement of a field ------------------------------------------

#' Measure every configured spatial characteristic of a single field.
#'
#' Add a characteristic to a project by returning one more named element here
#' and one more row in the DSI table; nothing else changes.
measure_field <- function(values, listw, strata) {
  m <- moran_i(values, listw)
  list(
    moran      = m$estimate,
    moran_p    = m$p_value,
    q          = q_statistic(values, strata)
  )
}
