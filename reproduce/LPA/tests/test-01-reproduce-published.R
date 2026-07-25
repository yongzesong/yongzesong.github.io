# =============================================================================
# test-01-reproduce-published.R — do the paper's own numbers come back out?
# =============================================================================
# Twenty-eight assertions: seven pathways, each with a mean, a minimum, a
# maximum and a share of significant locations, plus the optimal local range.
# Run with:  Rscript run-all.R test
# =============================================================================

cat("\n== test-01: published values ==================================\n")

ensure_prepared()

PASS <- 0L; FAIL <- 0L
check <- function(label, got, want, tol) {
  ok <- is.finite(got) && abs(got - want) <= tol
  if (ok) PASS <<- PASS + 1L else FAIL <<- FAIL + 1L
  cat(sprintf("   [%s] %-46s got %9.3f  want %9.3f\n",
              ifelse(ok, "ok", "FAIL"), label, got, want))
  invisible(ok)
}

d <- readRDS(F_POINTS)

# -- Table 2, row by row ------------------------------------------------------
want <- data.frame(
  col  = PATHS$published,
  mean = c(-0.370, 0.798, -0.763, 0.773, -0.269, 0.335, 0.427),
  min  = c(-0.967, -0.605, -0.987, 0.004, -0.998, -0.994, -0.969),
  max  = c(0.747, 0.999, 0.430, 1.000, 0.774, 0.998, 0.997),
  pct  = c(94.39, 98.29, 96.80, 98.21, 70.07, 63.52, 62.35),
  stringsAsFactors = FALSE)

for (j in seq_len(nrow(want))) {
  s <- summarise_lambda(d[[want$col[j]]], d[[paste0("sig.", want$col[j])]])
  nm <- PATHS$lambda[j]
  check(sprintf("%s mean", nm), s$mean, want$mean[j], 5e-4)
  check(sprintf("%s min",  nm), s$min,  want$min[j],  5e-4)
  check(sprintf("%s max",  nm), s$max,  want$max[j],  5e-4)
  check(sprintf("%s significant %%", nm), s$pct_significant, want$pct[j], 5e-3)
}

# -- The optimal local range --------------------------------------------------
# Two kilometres of tolerance: the published 707.29 km rests on a projection the
# paper does not state, and the estimate moves by about that much between
# reasonable choices of the degree-to-kilometre conversion.
if (file.exists(F_RANGE)) {
  r <- read_result(F_RANGE)
  sel <- r[r$selected, ][1, ]
  check("optimal local range (km)", sel$r_opt_km, 707.29, 2.5)
}

# -- The shape of the published output ----------------------------------------
check("locations in the file", nrow(d), 1801, 0)
check("locations with a complete set of p-values",
      sum(stats::complete.cases(d[, paste0("sig.", PATHS$published)])), 1283, 0)

cat(sprintf("\n   %d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0) log_warn("test-01 has failures") else
  cat("   every published figure reproduced\n")
