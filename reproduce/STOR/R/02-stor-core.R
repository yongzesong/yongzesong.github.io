# =============================================================================
# 02-stor-core.R — the spatial trade-off relation (STOR) model, standalone
# =============================================================================
# A self-contained implementation of the STOR model of Song et al. (2021),
# doi:10.1016/j.jag.2021.102585. Source this one file and the whole method is
# available; nothing here depends on a third-party package.
#
#   stor_standardize()    min-max standardization with a variable sign (Eq. 3)
#   stor_entropy_weights() entropy weights within a category         (Eq. 4-5)
#   stor_srii()           category, dimension and composite indices  (Eq. 6)
#   stor_utility()        LOESS utility u(Gamma_A) + marginal utility (Eq. 7-9)
#   stor_dmu()            IR / MR / NR stage boundaries and classifier
#   stor_grid_neighbors() queen contiguity on a regular grid of blocks
#   stor_local_moran()    local Moran's I with conditional permutations
#   stor_lisa_class()     HH / LL / other cluster labels at a given alpha
#   stor_tradeoff_groups() overlap of quantity and quality clusters
#   stor_gwr()            geographically weighted regression, adaptive kernel
#   stor_gwr_select()     bandwidth selection by AICc
#
# Only base R (stats::loess included) is required. spdep, when installed, is
# used by the tests to cross-check the local Moran implementation.
# =============================================================================


# -- 1. Index construction ----------------------------------------------------

#' Min-max standardization (Eq. 3). sign = +1 keeps orientation, sign = -1
#' flips it, so that larger standardized values always mean "more sustainable".
stor_standardize <- function(x, sign = +1) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))
  y <- (x - rng[1]) / (rng[2] - rng[1])
  if (sign < 0) y <- 1 - y
  y
}

#' Entropy weights of the columns of a standardized matrix Y (Eq. 4-5).
#' Columns carrying more variation (lower entropy) receive larger weights.
stor_entropy_weights <- function(Y) {
  Y <- as.matrix(Y)
  m <- nrow(Y)
  ent <- apply(Y, 2, function(y) {
    s <- sum(y, na.rm = TRUE)
    if (s == 0) return(1)                     # constant column: no information
    th <- y / s
    th <- th[th > 0]
    -sum(th * log(th)) / log(m)
  })
  phi <- 1 - ent
  if (sum(phi) == 0) return(rep(1 / ncol(Y), ncol(Y)))
  w <- phi / sum(phi)
  names(w) <- colnames(Y)
  w
}

#' Block-level SRII (Eq. 6). `data` holds the raw indicator columns;
#' `indicators` is a data.frame(name, category, dimension, sign) as in the
#' config. Weighting follows the paper: entropy weights within each category,
#' equal weights across categories within a dimension, equal weights across
#' the two dimensions.
#'
#' Returns list(weights, categories, gamma_a, gamma_b, gamma).
stor_srii <- function(data, indicators) {
  Y <- sapply(seq_len(nrow(indicators)), function(j)
    stor_standardize(data[[indicators$name[j]]], indicators$sign[j]))
  colnames(Y) <- indicators$name

  weights <- do.call(rbind, lapply(split(indicators, indicators$category), function(ind) {
    w <- stor_entropy_weights(Y[, ind$name, drop = FALSE])
    data.frame(name = ind$name, category = ind$category[1],
               dimension = ind$dimension[1], weight = as.numeric(w))
  }))
  rownames(weights) <- NULL

  cat_names <- unique(indicators$category)
  categories <- sapply(cat_names, function(cn) {
    w <- weights[weights$category == cn, ]
    as.numeric(Y[, w$name, drop = FALSE] %*% w$weight)
  })

  dim_index <- function(dm) {
    cats <- unique(indicators$category[indicators$dimension == dm])
    rowMeans(categories[, cats, drop = FALSE])
  }
  gamma_a <- dim_index("A")
  gamma_b <- dim_index("B")

  list(weights = weights, categories = as.data.frame(categories),
       gamma_a = gamma_a, gamma_b = gamma_b,
       gamma = (gamma_a + gamma_b) / 2)
}


# -- 2. Trade-off relation and diminishing marginal utility -------------------

#' The utility function u(Gamma_A) = f(Gamma_A) with Gamma_B as the observed
#' response (Eq. 7), fitted by LOESS on the raw blocks. The span is a
#' nearest-neighbour fraction, so the bandwidth adapts to the data: narrow
#' where blocks pile up at low Gamma_A — exactly where the utility rises
#' fastest — and wide in the sparse tail. The marginal utility eta is the
#' difference quotient of the fitted curve (Eq. 8-9). Equal-count bin means
#' are also returned; they are the black dots of the paper's Fig. 6b, for
#' display only.
#'
#' Returns list(bins, curve) where curve has columns ga, u, eta.
stor_utility <- function(gamma_a, gamma_b, n_bins = 40, span = 0.2,
                         n_eval = 200) {
  brk <- unique(stats::quantile(gamma_a, seq(0, 1, length.out = n_bins + 1)))
  bin <- cut(gamma_a, brk, include.lowest = TRUE)
  bins <- data.frame(
    ga = tapply(gamma_a, bin, mean),
    gb = tapply(gamma_b, bin, mean),
    n  = as.numeric(table(bin))
  )
  bins <- bins[bins$n > 0, ]

  fit <- stats::loess(gb ~ ga, data = data.frame(ga = gamma_a, gb = gamma_b),
                      span = span, degree = 2, family = "gaussian",
                      control = stats::loess.control(surface = "direct"))

  ga_eval <- seq(min(gamma_a), max(gamma_a), length.out = n_eval)
  u  <- stats::predict(fit, data.frame(ga = ga_eval))
  se <- stats::predict(fit, data.frame(ga = ga_eval), se = TRUE)$se.fit

  # Difference quotient of Eq. 9 as a centred difference over a small but
  # finite Delta Gamma_A (about 6% of the observed range), which keeps eta
  # from amplifying sub-Delta wiggles of the fitted curve.
  w <- max(1L, round(n_eval * 0.03))
  idx <- seq_len(n_eval)
  lo <- pmax(idx - w, 1L); hi <- pmin(idx + w, n_eval)
  eta <- (u[hi] - u[lo]) / (ga_eval[hi] - ga_eval[lo])

  list(bins = bins, fit = fit,
       curve = data.frame(ga = ga_eval, u = u, se = se, eta = eta))
}

#' DMU stage boundaries on a fitted utility curve. The IR-MR boundary t1 sits
#' at the maximum marginal utility; the MR-NR boundary t2 sits where the
#' utility peaks — max(u), the point where eta crosses zero for good
#' (Section 3.2 of the paper).
#'
#' Returns list(t1, t2, stage_of) where stage_of(ga) classifies values.
stor_dmu <- function(curve) {
  eta <- curve$eta[-nrow(curve)]
  ga  <- curve$ga[-nrow(curve)]

  i1 <- which.max(eta)
  t1 <- ga[i1]

  u <- curve$u[-nrow(curve)]
  i2 <- max(which.max(u), i1 + 1)
  t2 <- ga[i2]

  stage_of <- function(x)
    factor(ifelse(x < t1, "IR", ifelse(x < t2, "MR", "NR")),
           levels = c("IR", "MR", "NR"))

  list(t1 = t1, t2 = t2, i1 = i1, i2 = i2, stage_of = stage_of)
}


# -- 3. Spatial clusters (LISA) -----------------------------------------------

#' Queen-contiguity neighbours on a regular grid of blocks. `col` and `row`
#' are integer grid positions. Returns a list of integer index vectors.
stor_grid_neighbors <- function(col, row) {
  n <- length(col)
  key <- paste(col, row)
  idx <- seq_len(n)
  lookup <- new.env(hash = TRUE, size = n)
  for (i in idx) assign(key[i], i, envir = lookup)
  lapply(idx, function(i) {
    nb <- integer(0)
    for (dc in -1:1) for (dr in -1:1) {
      if (dc == 0 && dr == 0) next
      k <- paste(col[i] + dc, row[i] + dr)
      j <- lookup[[k]]
      if (!is.null(j)) nb <- c(nb, j)
    }
    sort(nb)
  })
}

#' Local Moran's I with conditional-permutation pseudo p-values (Anselin 1995).
#' `nb` is a neighbour list as returned by stor_grid_neighbors(); weights are
#' row-standardized. Matches spdep::localmoran() on Ii (the tests check this).
stor_local_moran <- function(x, nb, nsim = 999, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(x)
  z <- x - mean(x)
  s2 <- sum(z^2) / n
  lag <- vapply(seq_len(n), function(i) mean(z[nb[[i]]]), numeric(1))
  Ii <- z / s2 * lag

  p <- vapply(seq_len(n), function(i) {
    k <- length(nb[[i]])
    if (k == 0) return(NA_real_)
    zi <- z[-i]
    perm <- vapply(seq_len(nsim), function(s)
      z[i] / s2 * mean(sample(zi, k)), numeric(1))
    (1 + sum(abs(perm) >= abs(Ii[i]))) / (1 + nsim)
  }, numeric(1))

  data.frame(Ii = Ii, z = z, lag = lag, p = p)
}

#' HH / LL / HL / LH / Not significant labels from a local Moran result.
stor_lisa_class <- function(lm_res, alpha = 0.05) {
  cl <- rep("Not significant", nrow(lm_res))
  sig <- !is.na(lm_res$p) & lm_res$p < alpha
  cl[sig & lm_res$z > 0 & lm_res$lag > 0] <- "HH"
  cl[sig & lm_res$z < 0 & lm_res$lag < 0] <- "LL"
  cl[sig & lm_res$z > 0 & lm_res$lag < 0] <- "HL"
  cl[sig & lm_res$z < 0 & lm_res$lag > 0] <- "LH"
  factor(cl, levels = c("HH", "LL", "HL", "LH", "Not significant"))
}

#' Overlay the quantity and quality cluster labels (Section 3.3): a block is a
#' trade-off hotspot when both dimensions are HH, a cold spot when both are
#' LL. Cold-side blocks are also assigned a development strategy, following
#' Fig. 8b: a quantity-only cold spot calls for more quantity, a quality-only
#' cold spot for more quality, a joint cold spot for both.
stor_tradeoff_groups <- function(class_a, class_b) {
  cluster <- rep("Others", length(class_a))
  cluster[class_a == "HH" & class_b == "HH"] <- "Hotspot"
  cluster[class_a == "LL" & class_b == "LL"] <- "Cold spot"

  strategy <- rep("No cold spot", length(class_a))
  strategy[class_a == "LL" & class_b != "LL"] <- "Increase quantity"
  strategy[class_a != "LL" & class_b == "LL"] <- "Increase quality"
  strategy[class_a == "LL" & class_b == "LL"] <- "Increase both"

  data.frame(cluster = factor(cluster, c("Hotspot", "Others", "Cold spot")),
             strategy = factor(strategy, c("Increase quantity", "Increase both",
                                           "Increase quality", "No cold spot")))
}


# -- 4. Geographically weighted regression ------------------------------------

#' GWR with an adaptive bisquare kernel (Eq. 11). `k` is the number of
#' neighbours defining the local bandwidth. Returns location-wise coefficients,
#' fitted values, R2 and the AICc used for bandwidth selection.
stor_gwr <- function(y, X, coords, k) {
  X <- cbind(Intercept = 1, as.matrix(X))
  n <- nrow(X); p <- ncol(X)
  coords <- as.matrix(coords)
  beta <- matrix(NA_real_, n, p, dimnames = list(NULL, colnames(X)))
  fitted <- numeric(n)
  hat <- numeric(n)

  for (i in seq_len(n)) {
    d <- sqrt((coords[, 1] - coords[i, 1])^2 + (coords[, 2] - coords[i, 2])^2)
    h <- sort(d)[min(k, n)]
    w <- if (h > 0) ifelse(d < h, (1 - (d / h)^2)^2, 0) else rep(1, n)
    XtW <- t(X * w)
    XtWXi <- solve(XtW %*% X)
    bi <- XtWXi %*% (XtW %*% y)
    beta[i, ] <- bi
    fitted[i] <- X[i, ] %*% bi
    hat[i] <- (X[i, , drop = FALSE] %*% XtWXi %*% XtW)[i]
  }

  res <- y - fitted
  rss <- sum(res^2)
  r2 <- 1 - rss / sum((y - mean(y))^2)
  tr_s <- sum(hat)
  sigma2 <- rss / n
  aicc <- n * log(2 * pi * sigma2) + n * (n + tr_s) / (n - 2 - tr_s)

  list(beta = as.data.frame(beta), fitted = fitted, residuals = res,
       r2 = r2, aicc = aicc, tr_s = tr_s, k = k)
}

#' Adaptive bandwidth selection: fit at each candidate k, keep the lowest AICc.
stor_gwr_select <- function(y, X, coords, ks) {
  fits <- lapply(ks, function(k) stor_gwr(y, X, coords, k))
  aicc <- vapply(fits, `[[`, numeric(1), "aicc")
  best <- which.min(aicc)
  out <- fits[[best]]
  out$candidates <- data.frame(k = ks, aicc = aicc,
                               r2 = vapply(fits, `[[`, numeric(1), "r2"))
  out
}
