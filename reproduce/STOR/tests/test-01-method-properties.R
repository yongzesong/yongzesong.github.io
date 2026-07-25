# =============================================================================
# test-01-method-properties.R — the implementation satisfies the definitions
#
# Every component is checked against a property the published method requires:
# standardization bounds, entropy weight axioms, the DMU boundary definitions
# (max eta at t1, eta = 0 at t2), the local Moran statistic against spdep, and
# the GWR against ordinary least squares at an unlimited bandwidth.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("== test-01  Method properties =============================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-56s %s\n", label, if (ok) "pass" else "FAIL"))
  pass <<- pass + ok
}
set.seed(SEED)

## -- Standardization (Eq. 3) --------------------------------------------------
x <- rnorm(500, 10, 3)
yp <- stor_standardize(x, +1); yn <- stor_standardize(x, -1)
check("standardized values lie in [0, 1]",
      min(yp) >= 0 && max(yp) <= 1 && min(yn) >= 0 && max(yn) <= 1)
check("negative sign flips the orientation", all(abs(yp + yn - 1) < 1e-12))

## -- Entropy weights (Eq. 4-5) ------------------------------------------------
Y <- cbind(a = runif(400), b = runif(400), c = runif(400))
w <- stor_entropy_weights(Y)
check("entropy weights sum to 1", abs(sum(w) - 1) < 1e-12)
check("entropy weights are non-negative", all(w >= 0))
# A column with more variation must carry more information (larger weight).
Y2 <- cbind(flat = rep(0.5, 400) + runif(400, -0.01, 0.01), spread = runif(400))
w2 <- stor_entropy_weights(Y2)
check("higher-variation column gets the larger weight", w2["spread"] > w2["flat"])

## -- SRII composition (Eq. 6) -------------------------------------------------
blocks <- read_result(F_BLOCKS)
srii <- stor_srii(blocks, INDICATORS)
check("Gamma_A, Gamma_B and Gamma lie in [0, 1]",
      all(c(srii$gamma_a, srii$gamma_b, srii$gamma) >= 0) &&
      all(c(srii$gamma_a, srii$gamma_b, srii$gamma) <= 1))
check("Gamma is the mean of the two dimensions",
      max(abs(srii$gamma - (srii$gamma_a + srii$gamma_b) / 2)) < 1e-12)
check("weights sum to 1 within every category",
      all(abs(tapply(srii$weights$weight, srii$weights$category, sum) - 1) < 1e-12))

## -- DMU boundaries (Eq. 7-9, Section 3.2) ------------------------------------
util <- stor_utility(srii$gamma_a, srii$gamma_b,
                     n_bins = N_BINS, span = LOESS_SPAN, n_eval = N_EVAL)
dmu <- stor_dmu(util$curve)
eta <- util$curve$eta[-nrow(util$curve)]
check("t1 sits at the maximum marginal utility",
      abs(eta[dmu$i1] - max(eta, na.rm = TRUE)) < 1e-12)
check("t2 sits at the utility maximum, after t1",
      dmu$i2 > dmu$i1 &&
      abs(util$curve$u[dmu$i2] - max(util$curve$u)) < 1e-12)
# eta is a centred difference over ~6% of the range, so the sign change is
# checked just outside that window on either side of t2.
off <- max(2L, round(N_EVAL * 0.04))
check("eta changes sign around t2 (positive before, negative after)",
      eta[max(1L, dmu$i2 - off)] > 0 &&
      eta[min(length(eta), dmu$i2 + off)] < 0)
check("stage classifier respects the boundaries",
      all(dmu$stage_of(c(dmu$t1 - 0.01, dmu$t1 + 0.01, dmu$t2 + 0.01)) ==
          c("IR", "MR", "NR")))

## -- Local Moran against spdep ------------------------------------------------
sub <- blocks[blocks$col <= 12 & blocks$row <= 12, ]      # keep the test fast
gsub <- srii$gamma_a[blocks$col <= 12 & blocks$row <= 12]
nb <- stor_grid_neighbors(sub$col, sub$row)
ours <- stor_local_moran(gsub, nb, nsim = 99, seed = SEED)
if (has_pkg("spdep")) {
  nb_sp <- spdep::cell2nb(12, 12, type = "queen")
  # spdep orders cell2nb row-major by (row, col); reorder to match our blocks
  ord <- order(sub$row, sub$col)
  lw <- spdep::nb2listw(nb_sp, style = "W")
  ref <- spdep::localmoran(gsub[ord], lw)
  d <- max(abs(ours$Ii[ord] - ref[, "Ii"]))
  check(sprintf("local Moran Ii matches spdep (max diff %.2e)", d), d < 1e-9)
} else {
  cat("   (spdep not installed; local Moran cross-check skipped)\n")
}
check("local Moran pseudo p-values lie in (0, 1]",
      all(ours$p > 0 & ours$p <= 1, na.rm = TRUE))

## -- GWR sanity ---------------------------------------------------------------
z <- blocks[[INCOME_COL]]
X <- cbind(ga = srii$gamma_a, gb = srii$gamma_b)
# With every block at the same location all kernel weights equal 1, so the
# GWR must collapse to global OLS exactly.
co0 <- data.frame(x = rep(0, nrow(blocks)), y = rep(0, nrow(blocks)))
gwr_flat <- stor_gwr(z, X, co0, k = nrow(blocks))
ols <- stats::lm(z ~ ga + gb, data = data.frame(z = z, X))
d <- max(abs(as.numeric(gwr_flat$beta[1, ]) - as.numeric(stats::coef(ols))))
check(sprintf("co-located GWR equals global OLS (max diff %.2e)", d), d < 1e-8)
gwr_loc <- stor_gwr(z, X, blocks[, c(XCOL, YCOL)], k = 60)
check("local GWR fits at least as well as OLS",
      gwr_loc$r2 >= summary(ols)$r.squared - 1e-9)

cat(sprintf("   -- %d/%d checks passed --\n", pass, total))
if (pass < total) stop("test-01 failed")
