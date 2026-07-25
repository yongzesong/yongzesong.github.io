# =============================================================================
# 01-helpers.R — logging, IO, projection, neighbourhoods and figure helpers
# =============================================================================

log_info <- function(fmt, ...) cat(sprintf(paste0("   ", fmt, "\n"), ...))
log_warn <- function(fmt, ...) cat(sprintf(paste0("   ! ", fmt, "\n"), ...))
log_head <- function(fmt, ...)
  cat(sprintf(paste0("\n== ", fmt, " ",
                     strrep("=", max(2, 54 - nchar(sprintf(fmt, ...)))), "\n"), ...))

has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

need_pkg <- function(p, what) {
  if (!has_pkg(p)) stop(sprintf("package '%s' is required for %s", p, what))
  invisible(TRUE)
}

write_result <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE)
  log_info("wrote %s (%d rows)", sub(paste0(PROJ_ROOT, "/"), "", path), nrow(df))
  invisible(df)
}
read_result <- function(path) utils::read.csv(path, check.names = FALSE)

save_figure <- function(plot, name, width = FIG_WIDTH_DOUBLE, height = 12,
                        devices = FIG_DEVICES) {
  for (dev in devices) {
    path <- file.path(FIG_DIR, paste0(name, ".", dev))
    ggplot2::ggsave(path, plot, width = width, height = height, units = "cm",
                    dpi = FIG_DPI, device = dev)
  }
  log_info("wrote figs/%s.{%s}", name, paste(devices, collapse = ","))
  invisible(name)
}

# -- Geometry -----------------------------------------------------------------

#' Read the point table and attach planar coordinates in kilometres.
#'
#' Ripley's K counts pairs at a distance, so degrees have to become a metric
#' plane before anything else happens. An equirectangular scaling at the mean
#' latitude is enough over a study area of this size and is the convention that
#' recovers the published range.
read_points <- function(path = INPUT_FILE) {
  d <- utils::read.csv(file.path(PROJ_ROOT, path), check.names = FALSE)
  if (!all(c(COORD_X, COORD_Y) %in% names(d)))
    stop("Input file must contain the coordinate columns ", COORD_X, " and ", COORD_Y)
  lat0 <- mean(d[[COORD_Y]], na.rm = TRUE)
  if (identical(PROJECTION, "degrees")) {
    d$px <- d[[COORD_X]]; d$py <- d[[COORD_Y]]
    attr(d, "units") <- "degrees"
  } else {
    d$px <- d[[COORD_X]] * KM_PER_DEG_X * cos(lat0 * pi / 180)
    d$py <- d[[COORD_Y]] * KM_PER_DEG_Y
    attr(d, "units") <- "km"
  }
  attr(d, "lat0") <- lat0
  d
}

#' Every location within radius r of location i, itself included.
#'
#' This one line is what makes a path coefficient local: the model sees the
#' neighbourhood, never the whole plateau.
neighbours_within <- function(d, i, r) {
  which(sqrt((d$px - d$px[i])^2 + (d$py - d$py[i])^2) <= r)
}

#' Convex hull of the point cloud, closed, for drawing a study-area outline.
hull_outline <- function(d) {
  h <- grDevices::chull(d$px, d$py)
  data.frame(px = d$px[c(h, h[1])], py = d$py[c(h, h[1])])
}

# -- Ripley's K ---------------------------------------------------------------

#' Besag's L curve for one edge correction.
#'
#' Returns the r grid, K, L = sqrt(K/pi) - r, and the distance at which L peaks.
#' The peak is the optimal local range: the distance at which the points are
#' most clustered relative to complete spatial randomness.
ripley_L <- function(d, correction = K_CORRECTION, window = K_WINDOW,
                     r_max = K_R_MAX, r_step = K_R_STEP) {
  need_pkg("spatstat.geom", "Ripley's K")
  need_pkg("spatstat.explore", "Ripley's K")
  W <- if (identical(window, "hull"))
    spatstat.geom::convexhull.xy(d$px, d$py)
  else
    spatstat.geom::owin(range(d$px), range(d$py))
  pp <- spatstat.geom::ppp(d$px, d$py, window = W, checkdup = FALSE)
  K  <- spatstat.explore::Kest(pp, correction = correction,
                               r = seq(0, r_max, by = r_step))
  col <- setdiff(names(K), c("r", "theo"))[1]
  Kv  <- K[[col]]
  L   <- sqrt(Kv / pi) - K$r
  i   <- which.max(L)
  list(r = K$r, K = Kv, theo = K$theo, L = L,
       r_opt = K$r[i], L_max = L[i], correction = correction)
}

# -- Local SEM ----------------------------------------------------------------

#' Fit the structural equation model to one neighbourhood.
#'
#' Returns one row: the seven path coefficients, their p-values, the number of
#' observations the fit saw, and whether lavaan converged. A neighbourhood that
#' fails to converge returns NA rather than a number, which is why the published
#' output has gaps of its own.
fit_local_sem <- function(dat, model = SEM_MODEL, paths = PATHS,
                          solution = SEM_SOLUTION) {
  out <- list(lambda = rep(NA_real_, nrow(paths)),
              p      = rep(NA_real_, nrow(paths)),
              n      = nrow(dat), converged = FALSE, note = "")
  fit <- try(suppressWarnings(lavaan::sem(model, data = dat)), silent = TRUE)
  if (inherits(fit, "try-error")) {
    out$note <- "sem() error"; return(out)
  }
  if (!lavaan::lavInspect(fit, "converged")) {
    out$note <- "not converged"; return(out)
  }
  est <- try(suppressWarnings(
    if (identical(solution, "standardized")) lavaan::standardizedSolution(fit)
    else lavaan::parameterEstimates(fit)), silent = TRUE)
  if (inherits(est, "try-error")) { out$note <- "no solution"; return(out) }
  col_est <- if (identical(solution, "standardized")) "est.std" else "est"
  pick <- function(l, o, r, col) {
    v <- est[[col]][est$lhs == l & est$op == o & est$rhs == r]
    if (length(v)) v[1] else NA_real_
  }
  out$lambda <- mapply(pick, paths$lhs, paths$op, paths$rhs,
                       MoreArgs = list(col = col_est))
  out$p <- if ("pvalue" %in% names(est))
    mapply(pick, paths$lhs, paths$op, paths$rhs, MoreArgs = list(col = "pvalue"))
  else rep(NA_real_, nrow(paths))
  out$converged <- TRUE
  out
}

# -- Reporting ----------------------------------------------------------------

#' Table 2's summary of one lambda column: mean [min, max] over the plotted
#' range, and the share of locations whose p-value clears SIG_LEVEL.
summarise_lambda <- function(lambda, p, range = LAMBDA_PLOT_RANGE,
                             alpha = SIG_LEVEL) {
  v  <- lambda[is.finite(lambda) & lambda >= range[1] & lambda <= range[2]]
  pv <- p[is.finite(p)]
  data.frame(
    n_in_range = length(v),
    mean = if (length(v)) mean(v) else NA_real_,
    min  = if (length(v)) min(v)  else NA_real_,
    max  = if (length(v)) max(v)  else NA_real_,
    n_p  = length(pv),
    pct_significant = if (length(pv)) 100 * mean(pv < alpha) else NA_real_
  )
}

#' Classify lambda into the five printed classes of Fig. 5.
lambda_class <- function(v, breaks = LAMBDA_BREAKS) {
  cut(v, breaks = breaks, include.lowest = TRUE, dig.lab = 4)
}

#' Three-level significance label of Fig. 5.
sig_class <- function(p) {
  factor(ifelse(is.na(p), NA, ifelse(p < 0.01, "<0.01",
         ifelse(p < 0.05, "<0.05", "Not sig"))),
         levels = c("<0.01", "<0.05", "Not sig"))
}

fmt_pct <- function(x, d = 2) sprintf(paste0("%.", d, "f %%"), x)

#' Drop the collapsing tail of an edge-corrected K estimate.
#'
#' At its very last distances the border correction has almost no points with a
#' complete neighbourhood left, so K falls off a cliff and L with it. Those
#' distances are an artefact of the estimator rather than a feature of the point
#' pattern, and plotting them buries the whole curve under one spike.
trim_collapse <- function(r, L, drop = 60) {
  keep <- is.finite(L)
  if (!any(keep)) return(keep)
  last <- max(which(keep))
  # walk back while each step down from its predecessor is implausibly large
  while (last > 2 && (L[last - 1] - L[last]) > drop) last <- last - 1
  keep & seq_along(L) < last
}

#' Make sure the cheap preparation steps have run.
#'
#' The tests are meant to be the first thing anyone runs on a fresh unpack, so
#' they cannot assume a previous pipeline run left anything behind. Steps 10 and
#' 20 together take under a second; the expensive step 30 is never triggered
#' here, and results that depend on it stay optional.
ensure_prepared <- function() {
  needed <- c(F_POINTS, file.path(DERIVED, "r-opt.rds"),
              file.path(DERIVED, "neighbour-counts.rds"), F_KCURVE)
  if (all(file.exists(needed))) return(invisible(FALSE))
  log_info("preparing inputs the tests depend on (steps 10 and 20)")
  for (s in c("R/10-prepare-data.R", "R/20-optimal-range.R"))
    utils::capture.output(source(file.path(PROJ_ROOT, s), local = new.env()))
  invisible(TRUE)
}

write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID), paste("Seed:", SEED), ""), con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}
