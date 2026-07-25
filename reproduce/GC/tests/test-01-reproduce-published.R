# =============================================================================
# test-01-reproduce-published.R — the implementation matches the published one
#
# The values checked here are those published in the geocomplexity package's
# own vignettes, computed by the method's authors on this same layer. If these
# hold, the pipeline is running the reference implementation as intended.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geocomplexity", "the method")

cat("== test-01  Reproduce the published geocomplexity values =================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-56s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}
near <- function(a, b, tol = 5e-4) is.finite(a) && abs(a - b) < tol

x  <- read_layer()
d  <- sf::st_drop_geometry(x)
wt <- build_weights(x)
preds <- predictor_names(x)
f <- stats::as.formula(paste(RESPONSE, "~", paste(preds, collapse = " + ")))

check("layer has the published 333 units", nrow(x) == 333)
check("layer carries Gini and 8 explanatory variables", length(preds) == 8)

## -- vignette: MLR against GCMLR ---------------------------------------------
m1 <- stats::lm(f, data = d)
gc <- geocomplexity::geocd_vector(x[, preds], wt = wt, method = "moran",
                                  normalize = TRUE, returnsf = FALSE)
m2 <- stats::lm(stats::as.formula(paste(RESPONSE, "~ .")),
                data = cbind(d, as.data.frame(gc)))
check(sprintf("MLR   R2 = 0.5846 (got %.4f)", summary(m1)$r.squared),
      near(summary(m1)$r.squared, 0.5846))
check(sprintf("MLR   adjusted R2 = 0.5743 (got %.4f)", summary(m1)$adj.r.squared),
      near(summary(m1)$adj.r.squared, 0.5743))
check(sprintf("GCMLR R2 = 0.6215 (got %.4f)", summary(m2)$r.squared),
      near(summary(m2)$r.squared, 0.6215))
check(sprintf("GCMLR adjusted R2 = 0.6024 (got %.4f)", summary(m2)$adj.r.squared),
      near(summary(m2)$adj.r.squared, 0.6024))
check("adding geocomplexity raises the linear model's R2",
      summary(m2)$r.squared > summary(m1)$r.squared)

## -- vignette: GeoCGWR -------------------------------------------------------
g2 <- geocomplexity::gwr_geoc(f, data = x, bw = "AIC", adaptive = TRUE,
                              kernel = "gaussian")
check(sprintf("GeoCGWR R2 = 0.836 (got %.4f)", g2$diagnostic$R2),
      near(g2$diagnostic$R2, 0.836, 1e-3))
check(sprintf("GeoCGWR adjusted R2 = 0.8319 (got %.4f)", g2$diagnostic$R2_Adj),
      near(g2$diagnostic$R2_Adj, 0.8319, 1e-3))

## -- vignette: the GWR baseline it is compared against ------------------------
if (has_pkg("GWmodel")) {
  sp <- methods::as(x, "Spatial")
  bw <- GWmodel::bw.gwr(f, data = sp, approach = "AIC", kernel = "gaussian",
                        adaptive = TRUE)
  g1 <- GWmodel::gwr.basic(f, data = sp, bw = bw, kernel = "gaussian",
                           adaptive = TRUE)
  check(sprintf("GWR selects an adaptive bandwidth of 28 (got %d)", bw), bw == 28)
  check(sprintf("GWR R2 = 0.8025 (got %.4f)", g1$GW.diagnostic$gw.R2),
        near(g1$GW.diagnostic$gw.R2, 0.8025, 1e-3))
  check("GeoCGWR improves on the GWR baseline",
        g2$diagnostic$R2 > g1$GW.diagnostic$gw.R2)
} else {
  cat("   (GWmodel not installed; GWR baseline checks skipped)\n")
}

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass == total)
  cat("   PASS - the pipeline reproduces the published values\n") else
  stop("test-01 had failures")
