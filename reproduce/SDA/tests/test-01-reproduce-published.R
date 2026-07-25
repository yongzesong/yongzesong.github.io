# =============================================================================
# test-01-reproduce-published.R — the implementation is faithful
#
# The second-dimension variables generated here are compared against the values
# published with the method (Song 2022, distributed in the SecDim package). If
# this passes, every later number rests on the published definition.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("== test-01  Reproduce the published second-dimension variables ===========\n")

pts <- utils::read.csv(file.path(PROJ_ROOT, POINT_FILE))
grd <- utils::read.csv(file.path(PROJ_ROOT, GRID_FILE))
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-52s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}

v <- GRID_SURFACES[1]
gen <- sda_variables(pts[, c(LON, LAT)], grd[, c(LON, LAT)], grd[[v]],
                     distbuf = DIST_BUFFERS, quantileprob = QUANTILES)[, -(1:2)]
ref <- utils::read.csv(file.path(PROJ_ROOT, REFERENCE_DIR,
                                 paste0("sda-", tolower(v), ".csv")), check.names = FALSE)

check("generated variable count matches the published set", ncol(gen) == ncol(ref))
check("generated row count matches the sample count", nrow(gen) == nrow(pts))
d <- max(abs(as.matrix(gen) - as.matrix(ref)), na.rm = TRUE)
check(sprintf("values identical to published (max diff %.2e)", d), d < 1e-6)

# The built-in haversine must agree with the reference implementation, so the
# method runs with no third-party package at all.
if (has_pkg("geosphere")) {
  d1 <- sda_haversine(pts[1, c(LON, LAT)], grd[, c(LON, LAT)])
  d2 <- geosphere::distHaversine(pts[1, c(LON, LAT)], grd[, c(LON, LAT)], r = 6378137)
  check("built-in haversine matches geosphere (< 1e-6 m)", max(abs(d1 - d2)) < 1e-6)
} else {
  cat("   (geosphere not installed; distance cross-check skipped)\n")
}

## -- The published variable selection -----------------------------------------
# Fig. 5(a) of Song (2022) lists the second-dimension variables selected for Cr.
# Selecting from all 440 candidates must return that same set: the same
# surfaces, at the same searching ranges b, at the same quantiles tau.
published <- c("pH_b9t0.9", "pH_b9t0.7", "NDVI_b9t0.1", "pH_b3t0.9",
               "NDVI_b5t0.1", "NDVI_b7t0.4", "pH_b1t1", "Water_b9t1",
               "Road_b9t0.8", "SOC_b9t1")
y <- pts[[RESPONSE]]
if (isTRUE(LOG_RESPONSE)) y <- log(y)
krm <- sda_rmoutlier(y, OUTLIER_SD)
xall <- load_reference_vars()
got <- names(sda_select(if (length(krm)) y[-krm] else y,
                        if (length(krm)) lapply(xall, function(m) m[-krm, , drop = FALSE]) else xall,
                        ctr.vif = VIF_MAX))
check(sprintf("selection returns %d variables, as published", length(published)),
      length(got) == length(published))
check("the selected variables are exactly those of Fig. 5(a)",
      setequal(got, published))
if (!setequal(got, published)) {
  cat("      only here :", paste(setdiff(got, published), collapse = ", "), "\n")
  cat("      only in paper:", paste(setdiff(published, got), collapse = ", "), "\n")
}

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass == total)
  cat("   PASS - the implementation reproduces the published method\n") else
  stop("test-01 had failures")
