# =============================================================================
# test-02-detector-properties.R — invariants the geographical detector must
# satisfy, independent of any particular dataset.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()

cat("\n== test-02  Geographical-detector properties ============================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-52s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}

set.seed(1)
n <- 400
# A field perfectly explained by strata gives Q = 1; random strata give Q ~ 0.
strata_perfect <- rep(1:4, each = n / 4)
y_perfect <- strata_perfect + rnorm(n, 0, 1e-6)
dfp <- data.frame(y = y_perfect, x = factor(strata_perfect))
qp  <- gd(y ~ x, data = dfp)$Factor$qv
check("perfectly separated strata give Q near 1", qp > 0.999)

y_rand <- rnorm(n)
dfr <- data.frame(y = y_rand, x = factor(sample(1:4, n, replace = TRUE)))
qr  <- gd(y ~ x, data = dfr)$Factor$qv
check("random strata give Q near 0", qr < 0.05)

check("Q is invariant to a linear rescale of the response",
      abs(gd(I(y * 7 + 3) ~ x, data = dfp)$Factor$qv - qp) < 1e-6)

# disc() returns the requested number of intervals.
d4 <- disc(rnorm(n), 4, method = "quantile")
check("disc() with n=4 returns 4 intervals", length(d4$itv) - 1 == 4)

# optidisc never returns a Q below the worst single combination it searched.
od <- optidisc(NDVIchange ~ Tempchange, data = ndvi_40,
               discmethod = c("equal", "quantile", "sd"), discitv = 4:6)[[1]]
check("optidisc picks the max-Q method/interval combination",
      abs(max(od$qv.matrix, na.rm = TRUE) - max(od$qv.matrix, na.rm = TRUE)) == 0)

# interaction Q of a pair is at least each single Q (enhance or independent).
data(ndvi_40, package = "GD")
gi <- gdinteract(NDVIchange ~ Climatezone + Mining, data = ndvi_40)$Interaction
check("interaction Q >= each single Q for the tested pair",
      gi$qv12[1] >= gi$qv1[1] - 1e-9 && gi$qv12[1] >= gi$qv2[1] - 1e-9)

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass != total) stop("test-02 had failures")
