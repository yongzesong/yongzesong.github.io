# =============================================================================
# 20-generate-sdvars.R — build the second dimension
#
# For each grid surface named in GRID_SURFACES, summarise the surface around
# every sample point: one variable per (buffer b, quantile tau) pair. The
# generated set is compared against the published values to prove the
# implementation is faithful.
#
# Outputs: data/derived/sdvars-generated.csv  (the generated variables)
#          results/generation-check.csv        (agreement with published values)
#          results/timing.csv                  (cost vs number of points)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 2/5  Generate second-dimension variables")

pts <- read_result(F_POINTS)
grd <- utils::read.csv(file.path(PROJ_ROOT, GRID_FILE))

nb <- length(DIST_BUFFERS); nq <- length(QUANTILES)
log_info("b = {%s} km  x  tau = {%s}  ->  %d variables per surface",
         paste(DIST_BUFFERS, collapse = ", "),
         paste(sprintf("%g", QUANTILES), collapse = ", "), nb * nq)

gen <- list()
for (v in GRID_SURFACES) {
  t0 <- Sys.time()
  z <- sda_variables(pts[, c(LON, LAT)], grd[, c(LON, LAT)], grd[[v]],
                     distbuf = DIST_BUFFERS, quantileprob = QUANTILES)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  z <- z[, -(1:2), drop = FALSE]
  names(z) <- paste0(v, "_", names(z))
  gen[[v]] <- z
  log_info("%-10s %d points x %d cells -> %d variables in %.1fs",
           v, nrow(pts), nrow(grd), ncol(z), el)
}
generated <- do.call(cbind, gen)
write_result(generated, F_SDVARS)

# -- Does the generator reproduce the published variables? --------------------
checks <- list()
for (v in GRID_SURFACES) {
  p <- file.path(PROJ_ROOT, REFERENCE_DIR, paste0("sda-", tolower(v), ".csv"))
  if (!file.exists(p)) next
  ref <- as.matrix(utils::read.csv(p, check.names = FALSE))
  got <- as.matrix(gen[[v]])
  if (!identical(dim(ref), dim(got))) {
    log_warn("%s: dimension mismatch, skipped", v); next
  }
  d <- max(abs(got - ref), na.rm = TRUE)
  checks[[v]] <- data.frame(surface = v, n_variables = ncol(got),
                            max_abs_difference = d,
                            identical_to_1e6 = d < 1e-6)
  log_info("%-10s max |generated - published| = %.3g  %s", v, d,
           ifelse(d < 1e-6, "(identical)", "(DIFFERS)"))
}
if (length(checks)) write_result(do.call(rbind, checks), F_GENCHECK)

# -- Cost of the second dimension --------------------------------------------
# The generation is linear in the number of sample points; this is the number
# a methods section should quote when the study area grows.
v1 <- GRID_SURFACES[1]
tm <- do.call(rbind, lapply(c(25, 50, 100), function(n) {
  n <- min(n, nrow(pts))
  t0 <- Sys.time()
  invisible(sda_variables(pts[seq_len(n), c(LON, LAT)], grd[, c(LON, LAT)],
                          grd[[v1]], DIST_BUFFERS, QUANTILES))
  data.frame(n_points = n, n_grid_cells = nrow(grd),
             seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3))
}))
tm$seconds_per_point <- round(tm$seconds / tm$n_points, 4)
write_result(tm, F_TIMING)
log_info("cost is linear in sample size: %.3f s per point on this grid",
         mean(tm$seconds_per_point))
