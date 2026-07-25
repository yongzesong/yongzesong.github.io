# =============================================================================
# test-02-model-properties.R — properties the model must have
#
# Checks on the ideas rather than on one dataset: that delta behaves like a
# difference, that the classification is exhaustive, and that the geographical
# detector scores a stratification the way it should.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("\n== test-02  Model properties ============================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-58s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}

## -- delta is a difference, and inherits the sign convention ------------------
set.seed(1)
a <- stats::rnorm(500); b <- stats::rnorm(500)
dl <- b - a
check("delta is positive exactly when accessibility exceeds access",
      all((dl > 0) == (b > a)))
check("adding a constant to both leaves delta unchanged",
      max(abs(((b + 5) - (a + 5)) - dl)) < 1e-12)
check("delta changes sign when the two measures are swapped",
      max(abs((a - b) + dl)) < 1e-12)

## -- the classification covers every block, once ------------------------------
eps <- stats::sd(dl) / 10
k <- ifelse(dl > eps, "positive", ifelse(dl < -eps, "negative", "zero"))
check("every observation receives exactly one class",
      length(k) == length(dl) && !any(is.na(k)) &&
        setequal(unique(k), c("positive", "zero", "negative")))
check("the near-zero band is centred on zero",
      abs(mean(dl[k == "zero"])) < eps)

## -- the geographical detector on a known stratification ----------------------
if (has_pkg("GD")) {
  n <- 400
  strata <- rep(letters[1:4], each = n / 4)
  y_sep <- as.numeric(factor(strata)) + stats::rnorm(n, 0, 1e-3)  # strata explain all
  y_rnd <- stats::rnorm(n)                                        # strata explain nothing
  q_sep <- as.numeric(GD::gd(df ~ strata,
             data.frame(df = y_sep, strata = strata))$Factor$qv)
  q_rnd <- as.numeric(GD::gd(df ~ strata,
             data.frame(df = y_rnd, strata = strata))$Factor$qv)
  check(sprintf("perfectly separated strata give PD near 1 (%.3f)", q_sep), q_sep > 0.99)
  check(sprintf("meaningless strata give PD near 0 (%.3f)", q_rnd), q_rnd < 0.05)
  check("PD is unchanged by rescaling the response",
        abs(as.numeric(GD::gd(df ~ strata,
              data.frame(df = y_sep * 7 + 3, strata = strata))$Factor$qv) - q_sep) < 1e-6)
} else {
  cat("   (GD not installed; detector checks skipped)\n")
}

## -- tree-based zoning respects its minimum node size -------------------------
if (has_pkg("rpart")) {
  x <- stats::runif(600); y <- sin(6 * x) + stats::rnorm(600, 0, 0.2)
  tr <- rpart::rpart(y ~ x, data = data.frame(y, x), minbucket = TREE_MINBUCKET)
  sizes <- as.integer(table(tr$where))
  check(sprintf("no terminal node is smaller than minbucket (smallest %d)", min(sizes)),
        min(sizes) >= TREE_MINBUCKET)
  check("the tree finds more than one zone in structured data", length(sizes) > 1)
} else {
  cat("   (rpart not installed; tree checks skipped)\n")
}

## -- walking scales are consistent with the stated speed ----------------------
expected <- round(WALK_SPEED_KMH * 1000 / 60 * WALK_TIMES)
check(sprintf("distances follow %g km/h to within 5 m", WALK_SPEED_KMH),
      max(abs(expected - WALK_DISTANCES)) <= 5)

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass != total) stop("test-02 had failures")
