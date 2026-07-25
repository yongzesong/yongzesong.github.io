# =============================================================================
# test-02-method-properties.R — invariants the method must satisfy
#
# Two of these guard against mistakes that are silent rather than loud: a
# selection whose column names do not follow its contents, and a variance
# inflation factor computed against the wrong sum of squares. Both fit without
# complaint and both corrupt prediction on new data.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("\n== test-02  Method properties ============================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-56s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}

set.seed(1); n <- 300

## -- second-dimension variable generation ------------------------------------
grd <- data.frame(Lon = runif(4000, 118, 118.5), Lat = runif(4000, 31, 31.4))
grd$z <- grd$Lon * 10
pt <- data.frame(Lon = runif(12, 118.1, 118.4), Lat = runif(12, 31.1, 31.3))
g <- sda_variables(pt, grd[, c("Lon", "Lat")], grd$z,
                   distbuf = c(2, 5), quantileprob = c(0, 0.5, 1))
check("generation returns buffers x quantiles columns", ncol(g) - 2 == 2 * 3)
check("column names encode b and tau", all(c("b2t0", "b5t1") %in% names(g)))
check("quantile 0 never exceeds quantile 1", all(g$b2t0 <= g$b2t1))
check("a wider buffer spans at least as much as a narrower one",
      all(g$b5t1 - g$b5t0 >= g$b2t1 - g$b2t0 - 1e-9))

## -- variance inflation factor -----------------------------------------------
x <- data.frame(a = rnorm(n), b = rnorm(n)); x$c <- x$a * 0.95 + rnorm(n, 0, 0.2)
vv <- sda_vif(x)
check("VIF of an independent variable is near 1", abs(vv[2] - 1) < 0.15)
check("VIF of a near-duplicate variable is large", vv[1] > 5 && vv[3] > 5)
# VIF must use the total sum of squares of the observed values. Using the
# fitted values instead returns exactly VIF - 1, which quietly loosens any
# threshold by one unit.
y <- x$a; X <- cbind(1, as.matrix(x[, -1])); f <- stats::lm.fit(X, y)
sse <- sum(f$residuals^2); sst <- sum((y - mean(y))^2)
check("VIF equals 1/(1 - SSE/SST), not the fitted-value variant",
      abs(vv[1] - 1 / (sse / sst)) < 1e-8)

## -- selection ---------------------------------------------------------------
set.seed(42)                                       # independent of the blocks above
xs <- as.data.frame(matrix(rnorm(n * 8), n, 8)); names(xs) <- paste0("q", 1:8)
xs$q3 <- xs$q1 * 0.98 + rnorm(n, 0, 0.1)          # near-duplicate of q1
yy <- xs$q1 * 2 + xs$q5 + rnorm(n, 0, 0.5)        # only q1 and q5 drive y
sel <- sda_select_one(yy, xs, ctr.vif = 10)
check("selection drops the collinear duplicate", ncol(sel) < ncol(xs))
check("every kept column carries the values of the variable it names",
      all(vapply(names(sel), function(k)
        isTRUE(all.equal(unname(sel[[k]]), unname(xs[[k]]))), logical(1))))
# q1 and q3 are interchangeable by construction, so either may be kept, but the
# signal they carry must survive, and so must the second driver.
check("the predictive signal survives selection",
      any(c("q1", "q3") %in% names(sel)) && "q5" %in% names(sel))
check("exactly one of the near-duplicate pair is kept",
      sum(c("q1", "q3") %in% names(sel)) == 1)

## -- prediction alignment ----------------------------------------------------
# The end-to-end guarantee: a model fitted on selected training columns must
# read the same variables out of new data.
tr <- 1:200; te <- 201:n
s <- sda_select_one(yy[tr], xs[tr, ], ctr.vif = 10)
fit <- stats::lm(y ~ ., cbind(y = yy[tr], s))
p <- stats::predict(fit, newdata = xs[te, names(s), drop = FALSE])
check("held-out predictions stay in the range of the response",
      max(p) < max(yy) + 3 * stats::sd(yy) && min(p) > min(yy) - 3 * stats::sd(yy))
check("held-out R2 is positive for a well-specified model", r2_score(yy[te], p) > 0.5)

## -- outliers ----------------------------------------------------------------
z <- c(rnorm(100), 99)
check("an extreme value is flagged as an outlier", 101 %in% sda_rmoutlier(z))
check("clean data flags nothing", length(sda_rmoutlier(rnorm(100), 10)) == 0)

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass != total) stop("test-02 had failures")
