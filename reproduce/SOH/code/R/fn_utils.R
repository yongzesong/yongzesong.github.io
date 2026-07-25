# =============================================================================
# fn_utils.R — paths, logging, IO, and the synthetic demo dataset
# =============================================================================

# --- paths -------------------------------------------------------------------

#' Locate the project root from any working directory
#'
#' The root is the folder that contains config/config.R. Scripts call this so
#' they run identically from RStudio, Rscript, or a scheduler.
soh_root <- function(start = getwd()) {
  p <- normalizePath(start, mustWork = FALSE)
  for (i in 1:8) {
    if (file.exists(file.path(p, "config", "config.R"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  stop("Project root not found. Run from inside the project folder, ",
       "or set the working directory to the folder containing config/config.R.")
}

soh_path <- function(...) file.path(soh_root(), ...)

soh_config <- function(file = NULL) {
  if (is.null(file)) file <- soh_path("config", "config.R")
  source(file, local = TRUE)$value
}

# --- logging -----------------------------------------------------------------

soh_log <- function(..., level = "INFO") {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", level, "  ", ...)
  cat(msg, "\n", sep = "")
  logfile <- soh_path("03_results", "logs", paste0("run_", Sys.Date(), ".log"))
  cat(msg, "\n", sep = "", file = logfile, append = TRUE)
  invisible(msg)
}

soh_step <- function(title) {
  soh_log(strrep("-", 62))
  soh_log(title)
}

# --- IO ----------------------------------------------------------------------

soh_write_table <- function(x, name, cfg = NULL) {
  f <- soh_path("03_results", "tables", paste0(name, ".csv"))
  utils::write.csv(x, f, row.names = FALSE)
  soh_log("table written: 03_results/tables/", name, ".csv  (",
          nrow(x), " rows)")
  invisible(f)
}

soh_save_figure <- function(plot, name, cfg,
                            width = NULL, height = NULL) {
  width  <- width  %||% cfg$run$figure_width
  height <- height %||% cfg$run$figure_height
  f <- soh_path("03_results", "figures",
                paste0(name, ".", cfg$run$figure_device))
  ggplot2::ggsave(f, plot, width = width, height = height, units = "mm",
                  dpi = cfg$run$figure_dpi, device = cfg$run$figure_device)
  soh_log("figure written: 03_results/figures/", basename(f))
  invisible(f)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

soh_cache <- function(name) soh_path("data", "interim", paste0(name, ".rds"))

# --- session capture ---------------------------------------------------------

soh_record_session <- function() {
  f <- soh_path("03_results", "logs", "session-info.txt")
  utils::capture.output(utils::sessionInfo(), file = f)
  soh_log("session info written: 03_results/logs/session-info.txt")
}

# =============================================================================
# Synthetic demo dataset
# =============================================================================
# The demo reproduces the structure the SOH model is designed for: a smooth
# covariate field (the first dimension) plus injected local anomalies (the
# second dimension), with a response that depends on both. Because the anomaly
# locations are known, the demo doubles as a correctness check — SOP variables
# should reach a higher power of determinant than the raw covariates when
# outlier_signal_weight is high, and a lower one when it is set to 0.

#' Smooth spatially autocorrelated field on a regular grid
#'
#' Built by summing a small number of low-frequency sine components, which is
#' cheap and needs no external package.
soh_smooth_field <- function(gx, gy, extent, n_wave = 4, seed = 1) {
  set.seed(seed)
  z <- numeric(length(gx))
  for (k in seq_len(n_wave)) {
    fx <- stats::runif(1, 0.5, 2.5) / extent
    fy <- stats::runif(1, 0.5, 2.5) / extent
    ph <- stats::runif(2, 0, 2 * pi)
    amp <- stats::runif(1, 0.5, 1)
    z <- z + amp * sin(2 * pi * fx * gx + ph[1]) * cos(2 * pi * fy * gy + ph[2])
  }
  as.numeric(scale(z))
}

#' Generate the synthetic demo dataset
#'
#' @return list(points, grids, truth) where truth records the anomaly centres
soh_make_demo_data <- function(cfg) {
  set.seed(cfg$run$seed)
  d <- cfg$demo
  codes <- cfg$variables$codes
  ext <- d$grid_extent
  n <- d$grid_n

  # regular grid ------------------------------------------------------------
  ax <- seq(0, ext, length.out = n)
  g <- expand.grid(x = ax, y = ax)
  g$id <- seq_len(nrow(g))

  # smooth first-dimension fields ------------------------------------------
  for (i in seq_along(codes)) {
    g[[codes[i]]] <- soh_smooth_field(g$x, g$y, ext, seed = cfg$run$seed + i)
  }

  # inject local anomalies into a subset of variables ------------------------
  # Anomalies are the second dimension: compact, high-magnitude departures
  # from the smooth field that a covariate value alone cannot express.
  anom_vars <- codes[seq_len(max(1, ceiling(length(codes) / 2)))]
  centres <- data.frame(
    x = stats::runif(d$n_anomaly, 0.05 * ext, 0.95 * ext),
    y = stats::runif(d$n_anomaly, 0.05 * ext, 0.95 * ext),
    sign = sample(c(1, -1), d$n_anomaly, replace = TRUE),
    var = sample(anom_vars, d$n_anomaly, replace = TRUE),
    stringsAsFactors = FALSE
  )

  anomaly_load <- numeric(nrow(g))   # per-cell total anomaly exposure
  for (k in seq_len(nrow(centres))) {
    dd <- sqrt((g$x - centres$x[k])^2 + (g$y - centres$y[k])^2)
    w <- exp(-(dd / d$anomaly_radius)^2)
    bump <- centres$sign[k] * d$anomaly_strength * w
    g[[centres$var[k]]] <- g[[centres$var[k]]] + bump
    anomaly_load <- anomaly_load + abs(bump)
  }

  # analysis units sampled from the grid ------------------------------------
  # Sampling is restricted to the interior so that every unit has a complete
  # neighbourhood at the largest buffer. The grid extends past the sampled
  # region by point_margin, which is what a real study achieves by clipping
  # covariates to the study area plus a buffer rather than to the boundary.
  m <- d$point_margin
  interior <- which(g$x >= m & g$x <= ext - m & g$y >= m & g$y <= ext - m)
  if (length(interior) < d$n_points) {
    stop("demo point_margin is too large for grid_extent; reduce it in config.R")
  }
  idx <- sample(interior, d$n_points)
  p <- g[idx, , drop = FALSE]

  # response: smooth part + outlier-driven part ------------------------------
  smooth_part <- 0.9 * p[[codes[1]]] + 0.6 * p[[codes[3]]] - 0.4 * p[[codes[5]]]
  outlier_part <- as.numeric(scale(anomaly_load[idx]))
  w <- d$outlier_signal_weight
  p$response <- (1 - w) * as.numeric(scale(smooth_part)) +
    w * outlier_part +
    stats::rnorm(nrow(p), 0, d$noise_sd)

  keep <- c("id", "x", "y", codes)
  list(
    points = p[, c(keep, "response")],
    grids  = g[, keep],
    truth  = list(centres = centres, anomaly_vars = anom_vars,
                  outlier_signal_weight = w)
  )
}
