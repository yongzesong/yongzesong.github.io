# =============================================================================
# test-02-method-properties.R — does the implementation behave like the method?
# =============================================================================
# These assertions do not depend on the published output. They check that the
# pieces of LPA hold together: that clustering is what the L function finds,
# that the neighbourhood is genuinely local, that a local fit differs from the
# global one, and that a coefficient estimated on more data is more stable.
# =============================================================================

cat("\n== test-02: method properties =================================\n")

ensure_prepared()

PASS <- 0L; FAIL <- 0L
expect <- function(label, ok, detail = "") {
  if (isTRUE(ok)) PASS <<- PASS + 1L else FAIL <<- FAIL + 1L
  cat(sprintf("   [%s] %s%s\n", ifelse(isTRUE(ok), "ok", "FAIL"), label,
              ifelse(nzchar(detail), paste0(" — ", detail), "")))
  invisible(ok)
}

d <- readRDS(F_POINTS)

# -- 1. The L function detects clustering that is really there ---------------
# Points spread at random over the same window should peak far lower than the
# observed pattern, and the observed K should sit above the CSR expectation.
if (file.exists(F_KCURVE)) {
  k <- read_result(F_KCURVE)
  # The border correction returns nothing beyond the largest distance at which
  # some point still has a complete neighbourhood inside the window, so the
  # curve ends before the search grid does.
  fin <- k[is.finite(k$K_observed), ]
  r_estimable <- max(fin$r)
  mid <- fin[fin$r > 100, ]
  expect("observed K exceeds the random expectation at mid-range distances",
         mean(mid$K_observed > mid$K_expected) > 0.9,
         sprintf("%.0f %% of the estimable grid", 100 * mean(mid$K_observed > mid$K_expected)))
  r_peak <- fin$r[which.max(fin$L)]
  expect("L has an interior maximum rather than rising to the grid edge",
         r_peak < r_estimable,
         sprintf("peak at r = %.0f km, estimator reaches %.0f km", r_peak, r_estimable))
  expect("the peak is not sitting on the last estimable distance",
         r_peak < r_estimable * 0.98,
         sprintf("peak is %.0f km inside the limit", r_estimable - r_peak))
}

# -- 2. The neighbourhood is local -------------------------------------------
r_opt <- readRDS(file.path(DERIVED, "r-opt.rds"))
nb <- readRDS(file.path(DERIVED, "neighbour-counts.rds"))
expect("every neighbourhood is a strict subset of the study area",
       max(nb) < nrow(d),
       sprintf("largest is %d of %d locations", max(nb), nrow(d)))
expect("every neighbourhood clears the minimum sample size",
       min(nb) >= MIN_LOCAL_N, sprintf("smallest is %d", min(nb)))
set.seed(SEED)
i <- sample(nrow(d), 1)
near <- neighbours_within(d, i, r_opt)
dist_i <- sqrt((d$px - d$px[i])^2 + (d$py - d$py[i])^2)
expect("the neighbourhood contains exactly the points inside the radius",
       identical(sort(near), sort(which(dist_i <= r_opt))))
expect("a larger radius never returns fewer neighbours",
       length(neighbours_within(d, i, r_opt * 1.5)) >= length(near))

# -- 3. A local fit is not the global fit ------------------------------------
if (has_pkg("lavaan")) {
  glob <- fit_local_sem(d[, SEM_VARS])
  expect("the global model converges", glob$converged)
  if (file.exists(F_LAMBDA)) {
    rec <- read_result(F_LAMBDA)
    spread <- sapply(PATHS$published, function(cc) {
      v <- rec[[cc]]; v <- v[is.finite(v) & abs(v) <= 1]
      if (length(v) > 1) stats::sd(v) else NA_real_ })
    expect("local coefficients vary across space",
           all(spread > 0.02, na.rm = TRUE),
           sprintf("smallest spread sd = %.3f", min(spread, na.rm = TRUE)))
    far <- sapply(seq_along(PATHS$published), function(j) {
      v <- rec[[PATHS$published[j]]]; v <- v[is.finite(v) & abs(v) <= 1]
      if (!length(v)) return(NA_real_)
      mean(abs(v - glob$lambda[j]) > 0.1) })
    expect("most locations depart from the global estimate",
           mean(far, na.rm = TRUE) > 0.5,
           sprintf("%.0f %% of locations on average", 100 * mean(far, na.rm = TRUE)))
  }
}

# -- 4. More data gives a steadier estimate ----------------------------------
# Not a property of the published output but of the estimator: coefficients
# fitted on larger neighbourhoods should scatter less.
if (file.exists(F_LAMBDA)) {
  rec <- read_result(F_LAMBDA)
  v <- rec[[PATHS$published[1]]]
  ok <- is.finite(v) & abs(v) <= 1
  small <- v[ok & rec$n_local <= stats::median(rec$n_local)]
  large <- v[ok & rec$n_local >  stats::median(rec$n_local)]
  if (length(small) > 30 && length(large) > 30)
    expect("estimates from larger neighbourhoods scatter less",
           stats::sd(large) <= stats::sd(small) * 1.05,
           sprintf("sd %.3f against %.3f", stats::sd(large), stats::sd(small)))
}

cat(sprintf("\n   %d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0) log_warn("test-02 has failures") else
  cat("   the implementation behaves like the method it claims to be\n")
