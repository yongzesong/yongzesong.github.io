# =============================================================================
# test-02-metric-properties.R — do the indicators behave as defined?
# =============================================================================
# The published values in test-01 pin the implementation to one dataset. These
# checks pin its behaviour: what the indicator must return in situations where
# the answer follows from the definition rather than from data.
#
#   Rscript tests/test-02-metric-properties.R
# =============================================================================

if (!exists("PROJ_ROOT")) {
  .f <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[
    grep("^--file=", commandArgs(trailingOnly = FALSE))])
  PROJ_ROOT <- if (length(.f)) dirname(dirname(normalizePath(.f))) else getwd()
  source(file.path(PROJ_ROOT, "R", "00-config.R"))
}
source(file.path(R_DIR, "01-helpers.R"))
source(file.path(R_DIR, "02-spatial-metrics.R"))
source(file.path(R_DIR, "03-dsi.R"))
source(file.path(R_DIR, "09-make-synthetic-data.R"))

cat("\n== test-02  Metric properties ===========================================\n")

PASS <- 0L; FAIL <- 0L
check <- function(label, condition, detail = "") {
  if (isTRUE(condition)) {
    PASS <<- PASS + 1L; cat(sprintf("   pass  %s\n", label))
  } else {
    FAIL <<- FAIL + 1L; cat(sprintf("   FAIL  %s %s\n", label, detail))
  }
}

# -- eta -----------------------------------------------------------------------

check("a residual field with no structure gives eta = 1",
      isTRUE(all.equal(dsi_eta(0.5, 0), 1)))
check("a residual field identical to the response gives eta = 0",
      isTRUE(all.equal(dsi_eta(0.5, 0.5), 0)))
check("an amplified residual field gives eta < 0",
      dsi_eta(0.4, 0.6) < 0)
check("a response with no structure gives NA rather than a number",
      is.na(suppressWarnings(dsi_eta(0, 0.2))))

# -- theta ---------------------------------------------------------------------

th <- dsi_theta(c(0.6, 0.8))
check("theta_min is the smallest component", isTRUE(all.equal(th$theta_min, 0.6)))
check("theta_probable is the largest component", isTRUE(all.equal(th$theta_probable, 0.8)))
check("theta_max exceeds every component", th$theta_max > 0.8,
      sprintf("(got %.3f)", th$theta_max))
check("theta_max of independent components equals 1 - prod(1 - eta)",
      isTRUE(all.equal(th$theta_max, 1 - 0.4 * 0.2)))
check("theta collapses to the single value when one component is given",
      isTRUE(all.equal(unlist(dsi_theta(0.7)), c(theta_min = 0.7,
                                                 theta_probable = 0.7,
                                                 theta_max = 0.7))))

# -- Q statistic ---------------------------------------------------------------

set.seed(1)
x_sep <- c(rnorm(50, 0, 0.01), rnorm(50, 10, 0.01))
s_sep <- factor(rep(c("a", "b"), each = 50))
check("perfectly separated strata give Q near 1", q_statistic(x_sep, s_sep) > 0.99,
      sprintf("(got %.4f)", q_statistic(x_sep, s_sep)))

x_rand <- rnorm(200)
s_rand <- factor(sample(letters[1:4], 200, replace = TRUE))
check("random strata give Q near 0", abs(q_statistic(x_rand, s_rand)) < 0.1,
      sprintf("(got %.4f)", q_statistic(x_rand, s_rand)))

check("Q is invariant to a linear rescaling of the values",
      isTRUE(all.equal(q_statistic(x_sep, s_sep), q_statistic(3 * x_sep + 7, s_sep))))

# -- Moran's I on a known field ------------------------------------------------

syn <- make_synthetic_data(n_side = 20, seed = 7)
coords <- projected_coords(syn)
listw <- build_knn_weights(coords, k = 8)
m_struct <- moran_i(syn$y, listw)
m_noise  <- moran_i(sample(syn$y), listw)
check("a structured field has positive Moran's I", m_struct$estimate > 0.3,
      sprintf("(got %.3f)", m_struct$estimate))
check("a shuffled field has Moran's I near zero", abs(m_noise$estimate) < 0.1,
      sprintf("(got %.3f)", m_noise$estimate))
check("the structured field is significant", m_struct$p_value < 0.01)

# -- End-to-end behaviour on the synthetic case --------------------------------
# A model that uses the predictors must explain more spatial structure than one
# that predicts the mean everywhere. If this fails, the indicator is not
# tracking spatial explanation on this project's data at all.

predictors <- setdiff(names(syn), c("lon", "lat", "y"))
strata <- make_strata(syn, response = "y", predictors = predictors)

resid_null <- syn$y - mean(syn$y)
fit_lm     <- stats::lm(y ~ ., data = syn[, c("y", predictors)])
resid_lm   <- stats::residuals(fit_lm)

dsi_null <- dsi_for_model("null", syn$y, resid_null, listw, strata)
dsi_lm   <- dsi_for_model("lm",   syn$y, resid_lm,   listw, strata)

check("a mean-only model scores eta = 0 for autocorrelation",
      abs(dsi_null$eta_autocorrelation) < 1e-6,
      sprintf("(got %.6f)", dsi_null$eta_autocorrelation))
check("a fitted model beats the mean-only model on theta_probable",
      dsi_lm$theta_probable > dsi_null$theta_probable,
      sprintf("(lm %.3f, null %.3f)", dsi_lm$theta_probable, dsi_null$theta_probable))

# -- Result --------------------------------------------------------------------

cat(sprintf("\n   %d passed, %d failed\n\n", PASS, FAIL))
if (FAIL > 0 && !interactive()) quit(status = 1)
