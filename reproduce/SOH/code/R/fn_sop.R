# =============================================================================
# fn_sop.R — second-dimension outlier patterns (SOP)
# =============================================================================
# Implements Equations 3 and 4 of Ren et al. (2026), which in turn build on the
# second-dimension outlier model of Ren et al. (2025) and the second-dimension
# association model of Song (2022).
#
# For an analysis unit u and neighbourhood radius r, the positive and negative
# outlier patterns are the accumulated deviations of the covariate X over the
# grid cells v inside N_r(u) that exceed the local mean by more than s standard
# deviations:
#
#   SOP+(X, r, u) = sum over v in N_r(u) of z(X_v) where z(X_v) >  s
#   SOP-(X, r, u) = sum over v in N_r(u) of z(X_v) where z(X_v) < -s
#
# with z computed within the neighbourhood, not globally. Each covariate
# therefore yields 2 x length(buffers) SOP columns for each analysis unit.
#
# The reference implementation
# (github.com/renkaigis/Second-dimension_Outlier-driven_Heterogeneity, MIT)
# computes a full distance vector for every unit. This version adds a bounding
# box prefilter, a minimum-cell guard, and alternative summary statistics, and
# is otherwise numerically identical at statistic = "zsum", sd_threshold = 2.
# =============================================================================

#' Summarise the outlier component of one neighbourhood
#'
#' @param v numeric vector of covariate values inside the neighbourhood
#' @param s outlier threshold in standard deviations
#' @param statistic "zsum", "rawsum", or "count"
#' @return length-2 numeric vector: positive component, negative component
soh_outlier_component <- function(v, s = 2, statistic = "zsum") {
  m <- mean(v)
  sdv <- stats::sd(v)
  if (!is.finite(sdv) || sdv == 0) return(c(0, 0))
  z <- (v - m) / sdv
  pos <- z > s
  neg <- z < -s
  switch(statistic,
    zsum   = c(sum(z[pos]), sum(z[neg])),
    rawsum = c(sum(v[pos] - m), sum(v[neg] - m)),
    count  = c(sum(pos), -sum(neg)),
    stop("Unknown SOP statistic: ", statistic)
  )
}

#' Generate multi-scale SOP columns for a single covariate
#'
#' @param point_xy matrix or data.frame of analysis unit coordinates (n x 2)
#' @param grid_xy matrix or data.frame of grid cell coordinates (m x 2)
#' @param grid_value numeric vector of length m, the covariate on the grid
#' @param buffers numeric vector of neighbourhood radii, same unit as coords
#' @param sd_threshold outlier threshold in standard deviations
#' @param statistic summary statistic, see soh_outlier_component
#' @param min_cells neighbourhoods with fewer cells return 0
#' @param max_cells if > 0, neighbourhoods are subsampled to this many cells
#' @param progress print a progress line every 10 percent
#' @return data.frame with 2 x length(buffers) columns, n rows, named
#'   p_b<radius> and n_b<radius>
soh_generate_sop <- function(point_xy, grid_xy, grid_value,
                             buffers = seq(20, 200, 20),
                             sd_threshold = 2,
                             statistic = "zsum",
                             min_cells = 10,
                             max_cells = 0,
                             progress = TRUE) {

  pts <- as.matrix(point_xy)
  grd <- as.matrix(grid_xy)
  stopifnot(ncol(pts) == 2, ncol(grd) == 2,
            length(grid_value) == nrow(grd))

  ok <- is.finite(grid_value)
  grd <- grd[ok, , drop = FALSE]
  grid_value <- grid_value[ok]

  nb <- length(buffers)
  rmax <- max(buffers)
  np <- nrow(pts)

  pos <- matrix(0, np, nb, dimnames = list(NULL, paste0("p_b", buffers)))
  neg <- matrix(0, np, nb, dimnames = list(NULL, paste0("n_b", buffers)))

  gx <- grd[, 1]; gy <- grd[, 2]
  tick <- max(1, floor(np / 10))

  for (i in seq_len(np)) {
    # bounding box prefilter: only cells that could fall inside the largest
    # buffer enter the distance computation
    inbox <- which(abs(gx - pts[i, 1]) <= rmax & abs(gy - pts[i, 2]) <= rmax)
    if (!length(inbox)) next
    d <- sqrt((gx[inbox] - pts[i, 1])^2 + (gy[inbox] - pts[i, 2])^2)
    vals <- grid_value[inbox]

    for (t in seq_len(nb)) {
      sel <- which(d < buffers[t])
      if (length(sel) < min_cells) next
      if (max_cells > 0 && length(sel) > max_cells) {
        sel <- sample(sel, max_cells)
      }
      comp <- soh_outlier_component(vals[sel], sd_threshold, statistic)
      pos[i, t] <- comp[1]
      neg[i, t] <- comp[2]
    }
    if (progress && i %% tick == 0) {
      soh_log(sprintf("    SOP progress %3.0f%%", 100 * i / np))
    }
  }

  as.data.frame(cbind(pos, neg))
}

#' Generate SOP blocks for every covariate
#'
#' @return named list, one data.frame per covariate code
soh_generate_sop_all <- function(points, grids, cfg,
                                 buffers = NULL, sd_threshold = NULL,
                                 progress = TRUE) {
  buffers <- buffers %||% cfg$sop$buffers
  sd_threshold <- sd_threshold %||% cfg$sop$sd_threshold
  codes <- cfg$variables$codes

  pxy <- points[, c("x", "y")]
  gxy <- grids[, c("x", "y")]

  out <- vector("list", length(codes))
  names(out) <- codes
  for (k in seq_along(codes)) {
    soh_log("  SOP for ", codes[k], " (", k, "/", length(codes), ")")
    out[[k]] <- soh_generate_sop(
      pxy, gxy, grids[[codes[k]]],
      buffers = buffers,
      sd_threshold = sd_threshold,
      statistic = cfg$sop$statistic,
      min_cells = cfg$sop$min_cells,
      max_cells = cfg$sop$max_cells,
      progress = progress
    )
  }
  out
}

#' Buffer design diagnostic
#'
#' Ren et al. (2025) recommend 5 to 10 buffers spanning 10 to 20 percent of the
#' maximum pairwise distance between analysis units. This reports where the
#' configured buffers sit against that guidance.
soh_buffer_diagnostic <- function(points, cfg, buffers = NULL) {
  buffers <- buffers %||% cfg$sop$buffers
  xy <- as.matrix(points[, c("x", "y")])
  s <- xy[sample(seq_len(nrow(xy)), min(500, nrow(xy))), , drop = FALSE]
  dmax <- max(stats::dist(s))
  data.frame(
    n_buffers = length(buffers),
    smallest = min(buffers),
    largest = max(buffers),
    max_pairwise_distance = round(dmax, 1),
    largest_pct_of_max = round(100 * max(buffers) / dmax, 1),
    guidance = "5-10 buffers, largest 10-20 percent of max pairwise distance"
  )
}

#' Flatten a named list of SOP blocks into one wide data.frame
soh_sop_wide <- function(sopvars) {
  out <- do.call(cbind, lapply(names(sopvars), function(v) {
    b <- sopvars[[v]]
    names(b) <- paste0("SOP.", v, ".", names(b))
    b
  }))
  as.data.frame(out)
}

#' Subset an SOP block to the buffers not exceeding rmax
#'
#' Used by the scale-sensitivity experiment so SOPs are generated once at the
#' largest radius and then truncated, instead of being recomputed per radius.
soh_sop_subset_buffers <- function(sop_block, buffers, rmax) {
  keep <- buffers[buffers <= rmax]
  cols <- c(paste0("p_b", keep), paste0("n_b", keep))
  sop_block[, intersect(cols, names(sop_block)), drop = FALSE]
}
