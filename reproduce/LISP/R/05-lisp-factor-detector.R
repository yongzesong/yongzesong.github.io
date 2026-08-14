# =============================================================================
# 05-lisp-factor-detector.R — LISP local q for every variable in {X, GC(X)}
#
# localsp::lisp() runs the geographical detector factor detector inside each
# location's local window (radius = threshold from Step 04), yielding a local
# q value and significance per variable per location (cc005 Step 3).
# NA imputation: q -> 0 (no explanatory power), p -> 1 (not significant).
#
# Outputs: results/step05-local-q-values.csv
#          results/step05-local-q-sig.csv
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-lpi-core.R")

library(sf)
library(localsp)
library(gdverse)
library(sdsfun)
library(dplyr)

cfg <- load_config()
ensure_dir("results")

d        <- build_model_frame(cfg)
all_vars <- all_vars_of(cfg)
n_cores  <- get_cores(cfg)

spatial   <- build_spatial(d, cfg)
threshold <- read_local_range(cfg)$threshold_m

cat(sprintf("LISP factor detector: %d variables (%d X + %d GC), %d obs, %d cores\n",
            length(all_vars), length(cfg$predictors$x_vars),
            length(gc_names_of(cfg)), nrow(d), n_cores))
cat(sprintf("Threshold = %.0f m\n", threshold))

system.time({
  lisp_out <- run_lisp_factor(
    d, cfg,
    vars      = all_vars,
    threshold = threshold,
    distmat   = spatial$distmat,
    cores     = n_cores
  )
})

write.csv(lisp_out$q,   "results/step05-local-q-values.csv", row.names = FALSE)
write.csv(lisp_out$sig, "results/step05-local-q-sig.csv",    row.names = FALSE)
cat("Saved: results/step05-local-q-values.csv, results/step05-local-q-sig.csv\n")

# Quick console summary: top variables by mean local q.
mean_q <- sort(colMeans(lisp_out$q[, all_vars], na.rm = TRUE),
               decreasing = TRUE)
cat("\nTop variables by mean local q:\n")
for (v in names(head(mean_q, 10))) {
  cat(sprintf("  %-24s %.4f\n", v, mean_q[v]))
}
