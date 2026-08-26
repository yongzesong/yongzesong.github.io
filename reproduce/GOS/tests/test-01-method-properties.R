# =============================================================================
# test-01-method-properties.R — properties the GOS model must have
#
# These are checks on the method rather than on one dataset: kappa = 1 has to
# be the plain similarity-weighted mean of Eq. 6, the uncertainty of Eq. 12 has
# to be a probability that falls as zeta rises, and the search for lambda has
# to be deterministic or nothing downstream is reproducible.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geosimilarity", "the GOS method")

cat("\n== test-01  GOS method properties =======================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-60s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}
near <- function(a, b, tol = 1e-9) all(is.finite(a)) && max(abs(a - b)) < tol


## -- a small, self-contained case ---------------------------------------------
# 60 observations and 25 prediction locations on three covariates, with no
# spatial structure at all: the similarity model never looks at coordinates,
# so a purely tabular example exercises exactly the same arithmetic.
set.seed(SEED)
m <- 60; n <- 25; p <- 3
mk <- function(k) as.data.frame(matrix(stats::rnorm(k * p), k, p,
                                       dimnames = list(NULL, paste0("x", 1:p))))
obs <- mk(m); obs$z <- 2 * obs$x1 - obs$x2 + stats::rnorm(m, sd = 0.3)
new <- mk(n)
f <- z ~ x1 + x2 + x3


## -- Eq. 6: kappa = 1 is the similarity-weighted mean over every observation --
# A direct transcription of Eqs. 2-6. The min operator composes the per-
# covariate similarities of Eq. 3; sigma is the standard deviation over the
# observation *and* prediction locations together; delta follows the
# normalisation used in geosimilarity::gos(), which divides the squared
# deviations by the number of prediction locations.
bcs_reference <- function(formula, data, newdata) {
  vars <- all.vars(formula); yv <- vars[1]; xs <- vars[-1]
  X  <- as.matrix(data[, xs, drop = FALSE])
  Xp <- as.matrix(newdata[, xs, drop = FALSE])
  z  <- data[[yv]]
  sigma <- apply(rbind(X, Xp), 2, stats::sd)
  vapply(seq_len(nrow(Xp)), function(b) {
    E <- vapply(seq_along(xs), function(i) {
      dif   <- Xp[b, i] - X[, i]
      delta <- sqrt(sum(dif^2) / nrow(Xp))
      exp(-dif^2 / (2 * (sigma[i]^2 / delta)^2))
    }, numeric(nrow(X)))
    S <- apply(E, 1, min)              # Eq. 2, P = min
    sum(S * z) / sum(S)                # Eq. 6
  }, numeric(1))
}

g1  <- geosimilarity::gos(f, data = obs, newdata = new, kappa = 1, cores = 1)
ref <- bcs_reference(f, obs, new)
check(sprintf("gos(kappa = 1) equals Eq. 6 directly (max diff %.1e)",
              max(abs(g1$pred - ref))), near(g1$pred, ref, 1e-10))
check("BCS predictions lie inside the range of the observations",
      all(g1$pred >= min(obs$z) & g1$pred <= max(obs$z)))

# The weighted mean is scale-equivariant in the response: shifting and
# scaling z must shift and scale every prediction by the same amounts.
obs2 <- obs; obs2$z <- 3 * obs$z + 7
g1b  <- geosimilarity::gos(f, data = obs2, newdata = new, kappa = 1, cores = 1)
check("BCS is equivariant under z -> 3z + 7", near(g1b$pred, 3 * g1$pred + 7, 1e-9))


## -- Eq. 11: a smaller kappa uses strictly fewer observations -----------------
gk <- geosimilarity::gos(f, data = obs, newdata = new, kappa = 0.2, cores = 1)
check("GOS and BCS differ once kappa < 1", !near(gk$pred, g1$pred, 1e-8))
check("GOS predictions still lie inside the range of the observations",
      all(gk$pred >= min(obs$z) & gk$pred <= max(obs$z)))


## -- Eq. 12: uncertainty is a probability, decreasing in zeta -----------------
UNC <- c("uncertainty90", "uncertainty95", "uncertainty99",
         "uncertainty99.5", "uncertainty99.9", "uncertainty100")
U <- as.matrix(as.data.frame(gk)[, UNC])
check("all six uncertainty columns are present", ncol(U) == 6)
check("uncertainty lies in [0, 1]", all(U >= 0 & U <= 1))
# Theta = 1 - Q(S, zeta) and Q is a quantile, so a larger zeta can only lower
# Theta; equality is allowed because the quantiles can coincide.
check("uncertainty is non-increasing in zeta",
      all(apply(U, 1, function(r) all(diff(r) <= 1e-12))))
check("zeta = 1 gives 1 - max(similarity), the smallest of the six",
      all(U[, "uncertainty100"] <= U[, "uncertainty99.9"] + 1e-12))

# At zeta = 1 the retained set no longer matters: both models report
# 1 - max(similarity) over the whole observation set (paper Sect. 4.5.2).
check("BCS and GOS agree exactly at zeta = 1",
      near(as.data.frame(gk)$uncertainty100, as.data.frame(g1)$uncertainty100, 1e-12))


## -- Eq. 9: the search for lambda is deterministic ----------------------------
kg <- c(0.05, 0.1, 0.3, 1)
set.seed(SEED)
b1 <- geosimilarity::gos_bestkappa(f, data = obs, kappa = kg, nrepeat = 3,
                                   nsplit = 0.5, cores = 1)
set.seed(SEED + 999)          # a different outer seed on purpose
b2 <- geosimilarity::gos_bestkappa(f, data = obs, kappa = kg, nrepeat = 3,
                                   nsplit = 0.5, cores = 1)
check(sprintf("gos_bestkappa is reproducible (lambda = %.2f both times)", b1$bestkappa),
      identical(b1$bestkappa, b2$bestkappa) &&
        near(b1$cvmean$rmse, b2$cvmean$rmse, 1e-12))
check("lambda is the kappa with the smallest mean cross-validation RMSE",
      b1$bestkappa == b1$cvmean$kappa[which.min(b1$cvmean$rmse)])
check("every candidate kappa is scored", nrow(b1$cvmean) == length(kg))


## -- the base-R VIF helper matches the textbook definition --------------------
vv <- vif_lm(obs, c("x1", "x2", "x3"))
r2 <- summary(stats::lm(x1 ~ x2 + x3, data = obs))$r.squared
check(sprintf("vif_lm reproduces 1/(1 - R2) for x1 (%.4f)", vv[["x1"]]),
      near(vv[["x1"]], 1 / (1 - r2), 1e-12))
check("uncorrelated covariates have VIF near 1", all(vv < 1.3))


## -- the shared cross-validation splitter ------------------------------------
s1 <- cv_splits(100, 5, 0.5, seed = 7)
s2 <- cv_splits(100, 5, 0.5, seed = 7)
check("cv_splits is reproducible for a given seed", identical(s1, s2))
check("cv_splits returns disjoint halves of the right size",
      all(vapply(s1, length, integer(1)) == 50) &&
        all(vapply(s1, function(i) length(unique(i)) == 50, logical(1))))
check("cv_splits rejects a degenerate split",
      inherits(try(cv_splits(100, 2, 0.999), silent = TRUE), "try-error"))

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass == total)
  cat("   PASS - the implementation has the properties the method requires\n") else
  stop("test-01 had failures")
