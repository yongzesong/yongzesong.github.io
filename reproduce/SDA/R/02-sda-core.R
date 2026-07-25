# =============================================================================
# 02-sda-core.R — the second dimension of spatial association (SDA), standalone
# =============================================================================
# A self-contained implementation of the SDA method of Song (2022),
# doi:10.1016/j.jag.2022.102834. Nothing here depends on the SecDim package:
# source this one file and the whole method is available.
#
#   sda_variables()   generate second-dimension variables for one grid surface
#   sda_vif()         variance inflation factors of a variable set
#   sda_select_one()  rank by |correlation|, then drop multicollinearity
#   sda_select()      the same selection across several variables (two stages)
#   sda_newdata()     name the prediction/validation variables to match
#   sda_rmoutlier()   locate observations beyond k standard deviations
#
# Only base R and one distance function are required. geosphere::distHaversine
# is used when available; otherwise an identical built-in haversine is used, so
# the file runs with no third-party packages at all.
# =============================================================================


# -- Great-circle distance ----------------------------------------------------
# Metres between one point (lon, lat) and a matrix of points, on a sphere of
# radius r. Matches geosphere::distHaversine(..., r = 6378137).

sda_haversine <- function(p, pts, r = 6378137) {
  d2r <- pi / 180
  lon1 <- p[[1]] * d2r; lat1 <- p[[2]] * d2r
  lon2 <- pts[[1]] * d2r; lat2 <- pts[[2]] * d2r
  dlon <- lon2 - lon1; dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}


# -- 1. Second-dimension variables --------------------------------------------
#' Generate second-dimension variables for one grid surface.
#'
#' For every sample point, the value of a grid variable is summarised over the
#' locations *around* the sample — the second dimension. Each buffer radius b
#' (km) and quantile probability tau gives one variable, so a surface yields
#' length(distbuf) x length(quantileprob) columns named b<b>t<tau>.
#'
#' pointlocation  data.frame/matrix of sample coordinates (lon, lat)
#' gridlocation   data.frame/matrix of grid coordinates (lon, lat)
#' gridvar        numeric vector of the grid variable, one value per grid cell
#' distbuf        buffer radii in km
#' quantileprob   quantile probabilities in [0, 1]
#' Returns the point coordinates column-bound to the generated variables.

sda_variables <- function(pointlocation, gridlocation, gridvar,
                          distbuf = seq(1, 10, 1),
                          quantileprob = seq(0, 1, 0.1),
                          verbose = FALSE) {
  samples <- as.data.frame(pointlocation)
  grids   <- as.data.frame(gridlocation)
  if (nrow(grids) != length(gridvar))
    stop("gridvar must have one value per row of gridlocation")

  nbuf <- length(distbuf); nprob <- length(quantileprob)
  out <- data.frame(matrix(NA_real_, nrow(samples), nbuf * nprob))
  names(out) <- paste0(rep(paste0("b", distbuf), each = nprob),
                       rep(paste0("t", quantileprob), times = nbuf))

  use_geosphere <- requireNamespace("geosphere", quietly = TRUE)
  maxbuf <- max(distbuf)

  for (i in seq_len(nrow(samples))) {
    zi <- if (use_geosphere) {
      geosphere::distHaversine(samples[i, ], grids, r = 6378137) / 1000
    } else {
      sda_haversine(samples[i, ], grids) / 1000
    }
    # One pass per buffer: cells inside b, then the requested quantiles.
    vals <- numeric(0)
    for (b in distbuf) {
      inb <- which(zi < b)
      q <- if (length(inb) > 0) {
        as.numeric(stats::quantile(gridvar[inb], quantileprob, na.rm = TRUE))
      } else {
        rep(NA_real_, nprob)
      }
      vals <- c(vals, q)
    }
    out[i, ] <- vals
    if (verbose && i %% 100 == 0)
      cat(sprintf("   ...%d / %d points\n", i, nrow(samples)))
  }
  cbind(pointlocation, out)
}


# -- 2. Variance inflation factor ---------------------------------------------
#' VIF of every column of x, computed with base-R least squares.
#' VIF_j = 1 / (1 - R^2_j) where R^2_j regresses column j on the others.

sda_vif <- function(x) {
  x <- as.data.frame(x)
  nx <- ncol(x)
  if (nx < 2) return(stats::setNames(rep(1, nx), names(x)))
  vapply(seq_len(nx), function(j) {
    y <- x[[j]]
    X <- cbind(1, as.matrix(x[, -j, drop = FALSE]))
    fit <- tryCatch(stats::lm.fit(X, y), error = function(e) NULL)
    if (is.null(fit)) return(Inf)
    r2 <- 1 - sum(fit$residuals^2) / sum((y - mean(y))^2)
    if (!is.finite(r2) || r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
}


# -- 3. Variable selection ----------------------------------------------------
#' Select variables from one set: order by |correlation with y|, then walk down
#' that order dropping any variable that pushes the VIF above ctr.vif.
#'
#' NOTE. The returned columns keep the names of the columns they actually hold.
#' This matters: a selection that returns correctly-ordered values under the
#' wrong names still fits, because the fitted values are self-consistent, but
#' predict() on new data matches columns *by name* and would then read the wrong
#' variable — silently, and catastrophically.

sda_select_one <- function(y, x, ctr.vif = 10) {
  x <- as.data.frame(x)
  keep <- vapply(x, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1))
  x <- x[, keep, drop = FALSE]
  if (ncol(x) == 0) stop("no variable in x has non-zero variance")

  cors <- abs(stats::cor(cbind(y, x), method = "pearson")[1, -1])
  xs <- x[, rev(order(cors)), drop = FALSE]      # strongest correlation first
  nxs <- ncol(xs)
  if (nxs == 1) return(xs)

  drop <- integer(0)
  for (i in 2:nxs) {
    k <- seq_len(i)
    if (length(drop) > 0) k <- setdiff(k, drop)
    if (length(k) < 2) next
    if (max(sda_vif(xs[, k, drop = FALSE])) > ctr.vif) drop <- c(drop, i)
  }
  kept <- setdiff(seq_len(nxs), drop)
  out <- xs[, kept, drop = FALSE]
  names(out) <- names(xs)[kept]                  # names follow the content
  out
}

#' Select across several variable sets: choose within each set first (so no one
#' surface can flood the model), prefix the survivors by set, then select again
#' across the pooled survivors.

sda_select <- function(y, xlist, ctr.vif = 10) {
  if (!is.list(xlist)) xlist <- list(xlist)
  nms <- names(xlist)
  if (is.null(nms)) nms <- paste0("v", seq_along(xlist))
  if (length(xlist) == 1) {
    out <- sda_select_one(y, xlist[[1]], ctr.vif)
    names(out) <- paste0(nms[1], "_", names(out))
    return(out)
  }
  per_set <- lapply(seq_along(xlist), function(i) {
    d <- sda_select_one(y, xlist[[i]], ctr.vif)
    names(d) <- paste0(nms[i], "_", names(d))
    d
  })
  sda_select_one(y, do.call(cbind, per_set), ctr.vif)
}

#' Name a full variable list the same way sda_select() does, so that new data
#' lines up with a fitted model column for column.

sda_newdata <- function(xlist) {
  if (!is.list(xlist)) xlist <- list(xlist)
  nms <- names(xlist)
  if (is.null(nms)) nms <- paste0("v", seq_along(xlist))
  out <- lapply(seq_along(xlist), function(i) {
    d <- as.data.frame(xlist[[i]])
    names(d) <- paste0(nms[i], "_", names(d))
    d
  })
  do.call(cbind, out)
}


# -- 4. Outliers --------------------------------------------------------------
#' Positions of values beyond coef standard deviations from the mean (or NA).

sda_rmoutlier <- function(x, coef = 2.5) {
  m <- mean(x, na.rm = TRUE); s <- stats::sd(x, na.rm = TRUE)
  which(is.na(x) | x > m + coef * s | x < m - coef * s)
}
