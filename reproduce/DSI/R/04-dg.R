# =============================================================================
# 04-dg.R — degree of geocomplexity (local extension of DSI)
# =============================================================================
# Liu, H., Song, Y., Yi, W., & Zhang, P. (2026). Degree of geocomplexity for
# diagnosing spatial pattern loss beyond prediction accuracy.
# GIScience & Remote Sensing, 63(1), 2657087.
# https://doi.org/10.1080/15481603.2026.2657087
#
#   DG_i = (G0_i - Gr_i) / G0_i
#
# G0 is the local geocomplexity of the response field and Gr that of the
# residual field, computed at the same neighbourhood size. Because DG is
# point-wise, it answers where a model explains local structure, which the
# global DSI cannot. Values are unbounded: positive means local structure was
# attenuated, near zero means the model had no effect on it, negative means the
# model added structure that the data did not contain.
# =============================================================================

#' Point-wise DG for one model.
dg_for_model <- function(model_name, g_response, residual, coords, k = K_GEOC) {
  g_resid <- geocomplexity_field(residual, coords, k = k, normalize = FALSE)
  dg <- (g_response - g_resid) / g_response
  data.frame(model = model_name, point_id = seq_along(dg),
             g_data = g_response, g_residual = g_resid, dg = dg)
}

#' Point-wise DG for every model.
dg_table <- function(response, residuals, coords, k = K_GEOC) {
  g0 <- geocomplexity_field(response, coords, k = k, normalize = FALSE)
  out <- lapply(names(residuals), function(m) {
    dg_for_model(m, g0, residuals[[m]], coords, k = k)
  })
  do.call(rbind, out)
}

#' Summarise a point-wise DG distribution.
#'
#' The mean with a 95% confidence interval is what the DG paper reports. The
#' distribution is heavy-tailed, because G0_i sits in the denominator and
#' approaches zero at some points, so the median and trimmed mean are reported
#' alongside it. When mean and median disagree in sign, the mean is being
#' driven by a handful of near-zero denominators and the median is the number
#' to quote in the manuscript.
dg_summary <- function(dg_points) {
  models <- unique(dg_points$model)
  rows <- lapply(models, function(m) {
    v <- dg_points$dg[dg_points$model == m]
    v <- v[is.finite(v)]
    se <- stats::sd(v) / sqrt(length(v))
    data.frame(
      model        = m,
      n            = length(v),
      dg_mean      = mean(v),
      dg_ci_low    = mean(v) - 1.96 * se,
      dg_ci_high   = mean(v) + 1.96 * se,
      dg_median    = stats::median(v),
      dg_trim10    = mean(v, trim = 0.10),
      dg_positive_share = mean(v > 0)
    )
  })
  out <- do.call(rbind, rows)
  out[order(-out$dg_median), ]
}

#' Locally optimal model at every sample point.
#'
#' The map this produces is the argument that no single model wins everywhere,
#' which is the practical claim of the DG paper.
dg_optimal_model <- function(dg_points, coords) {
  wide <- stats::reshape(dg_points[, c("point_id", "model", "dg")],
                         idvar = "point_id", timevar = "model", direction = "wide")
  names(wide) <- sub("^dg\\.", "", names(wide))
  mat <- as.matrix(wide[, setdiff(names(wide), "point_id"), drop = FALSE])
  best_idx <- apply(mat, 1, function(r) if (all(is.na(r))) NA_integer_ else which.max(r))
  data.frame(
    point_id   = wide$point_id,
    X          = coords[wide$point_id, 1],
    Y          = coords[wide$point_id, 2],
    best_model = colnames(mat)[best_idx],
    best_dg    = mat[cbind(seq_len(nrow(mat)), best_idx)]
  )
}

#' How often each model is locally best.
dg_optimal_share <- function(optimal) {
  tab <- as.data.frame(table(optimal$best_model), stringsAsFactors = FALSE)
  names(tab) <- c("model", "n_locations")
  tab$share <- tab$n_locations / sum(tab$n_locations)
  tab[order(-tab$n_locations), ]
}
