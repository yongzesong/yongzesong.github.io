# =============================================================================
# test-01-reproduce-dsi-paper.R — does this code reproduce the published paper?
# =============================================================================
# Runs the template's metric functions on the demo dataset released with
# Liu et al. (2026) and compares the output with the values printed in the
# paper: Table 3, row `lm`, and the lm entry of Figure 6.
#
#   delta^a_0 = 0.409   Moran's I of the response
#   delta^a_r = 0.189   Moran's I of the lm residuals
#   delta^h_0 = 0.513   Q value of the response
#   delta^h_r = 0.203   Q value of the lm residuals
#   eta^a     = 0.538   theta_min
#   eta^h     = 0.604   theta_probable
#   theta_max = 0.817
#
# Run this before trusting any number the pipeline produces on your own data,
# and again after changing anything in R/02-spatial-metrics.R or R/03-dsi.R.
#
#   Rscript tests/test-01-reproduce-dsi-paper.R
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

cat("\n== test-01  Reproduce Liu et al. (2026) Table 3, row lm ==================\n")

demo <- file.path(REF_DIR, "dsi-paper-demo-dataset.csv")
if (!file.exists(demo)) {
  stop("Reference dataset missing: ", demo,
       "\nDownload it from https://github.com/KevinOceanLiu/DSI")
}

df <- stats::na.omit(utils::read.csv(demo, check.names = FALSE))
cat(sprintf("   dataset: %d rows, %d columns\n", nrow(df), ncol(df)))

# The published settings, not the project's settings: this test reproduces the
# paper, so it hard-codes what the paper used.
coords <- projected_coords(df, data_crs = 4326, projected_crs = 3577)
listw  <- build_knn_weights(coords, k = 8)

predictors <- setdiff(names(df), c("lon", "lat", "y", "residual"))
strata <- make_strata(df, response = "y", predictors = predictors, method = "tree")

obs <- measure_field(df$y, listw, strata)
res <- measure_field(df$residual, listw, strata)
eta_a <- dsi_eta(obs$moran, res$moran)
eta_h <- dsi_eta(obs$q, res$q)
theta <- dsi_theta(c(eta_a, eta_h))

published <- c(delta_a_0 = 0.409, delta_a_r = 0.189,
               delta_h_0 = 0.513, delta_h_r = 0.203,
               eta_a = 0.538, eta_h = 0.604,
               theta_min = 0.538, theta_probable = 0.604, theta_max = 0.817)

computed <- c(delta_a_0 = obs$moran, delta_a_r = res$moran,
              delta_h_0 = obs$q,     delta_h_r = res$q,
              eta_a = eta_a, eta_h = eta_h,
              theta_min = theta$theta_min,
              theta_probable = theta$theta_probable,
              theta_max = theta$theta_max)

TOL <- 0.005
diffs <- abs(computed - published)
pass <- diffs < TOL

cat("\n   quantity        published   computed   difference   result\n")
cat("   ", strrep("-", 58), "\n", sep = "")
for (i in seq_along(published)) {
  cat(sprintf("   %-14s  %8.3f   %8.3f   %10.4f   %s\n",
              names(published)[i], published[i], computed[i], diffs[i],
              if (pass[i]) "pass" else "FAIL"))
}

cat(sprintf("\n   strata: %s method, %d strata\n",
            attr(strata, "method"), attr(strata, "n_strata")))
cat(sprintf("   Moran's I p-values: response %.2e, residual %.2e\n",
            obs$moran_p, res$moran_p))

if (all(pass)) {
  cat("\n   PASS — the template reproduces every published value within", TOL, "\n\n")
} else {
  cat("\n   FAIL —", sum(!pass), "value(s) outside tolerance.\n")
  cat("   Check, in order: PROJECTED_CRS is 3577, k is 8, the Q statistic uses\n")
  cat("   the sum-of-squares form in R/02-spatial-metrics.R, and the strata come\n")
  cat("   from a regression tree fitted to y on all predictors.\n\n")
  if (!interactive()) quit(status = 1)
}
