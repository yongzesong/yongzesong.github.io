# =============================================================================
# 10-prepare-data.R — read the point and grid tables, transform the response
#
# Output: data/derived/points.csv   (samples with the modelling response y)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 1/5  Prepare data")

pts <- utils::read.csv(file.path(PROJ_ROOT, POINT_FILE))
grd <- utils::read.csv(file.path(PROJ_ROOT, GRID_FILE))

need <- c(LON, LAT, RESPONSE)
miss <- setdiff(need, names(pts))
if (length(miss)) stop("POINT_FILE lacks columns: ", paste(miss, collapse = ", "))
miss <- setdiff(c(LON, LAT, GRID_SURFACES), names(grd))
if (length(miss)) stop("GRID_FILE lacks columns: ", paste(miss, collapse = ", "))

log_info("samples: %d rows | grid: %d cells (%.0fx denser than the samples)",
         nrow(pts), nrow(grd), nrow(grd) / nrow(pts))
log_info("grid surfaces available: %s", paste(GRID_SURFACES, collapse = ", "))

pts$y <- pts[[RESPONSE]]
if (isTRUE(LOG_RESPONSE)) {
  if (any(pts$y <= 0, na.rm = TRUE)) stop("LOG_RESPONSE needs a positive response")
  pts$y <- log(pts$y)
  log_info("response '%s' log-transformed (skew %.2f -> %.2f)", RESPONSE,
           mean((pts[[RESPONSE]] - mean(pts[[RESPONSE]]))^3) / stats::sd(pts[[RESPONSE]])^3,
           mean((pts$y - mean(pts$y))^3) / stats::sd(pts$y)^3)
}
log_info("response y: mean=%.3f sd=%.3f range=[%.3f, %.3f]",
         mean(pts$y), stats::sd(pts$y), min(pts$y), max(pts$y))

write_result(pts, F_POINTS)
