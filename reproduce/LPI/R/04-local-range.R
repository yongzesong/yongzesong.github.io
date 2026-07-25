# =============================================================================
# 04-local-range.R — local analysis extent from the response semivariogram
#
# LISP (Hu et al. 2024) fixes the local window as   d = multiplier x range,
# where range comes from a fitted semivariogram of the response on projected
# coordinates (automap::autofitVariogram). The multiplier (default 2) and the
# sensitivity of results to it are examined again in Step 10.
#
# Outputs: results/step04-local-range.csv       (range, multiplier, threshold)
#          results/step04-variogram-points.csv  (empirical variogram, for Fig)
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-lpi-core.R")

library(sf)
library(automap)

cfg <- load_config()
ensure_dir("results")

d  <- read_analysis_table(cfg)
rz <- read_response(cfg)
d[[cfg$response$response_name]] <- rz[[cfg$response$response_name]]

spatial <- build_spatial(d, cfg)
lr      <- estimate_local_range(d, cfg, spatial)

cat(sprintf("Variogram range = %.0f m (%.2f km)\n",
            lr$range, lr$range / 1000))
cat(sprintf("Local extent (threshold) = %.0f m (%.2f km)  [multiplier = %s]\n",
            lr$threshold, lr$threshold / 1000,
            cfg$params$lisp$threshold_multiplier))

dm <- spatial$distmat
cat(sprintf("Distance matrix (m): min=%.0f  median=%.0f  max=%.0f\n",
            min(dm[dm > 0]), median(dm[dm > 0]), max(dm)))

write.csv(
  data.frame(
    variogram_model      = as.character(lr$variogram$var_model$model[2]),
    range_m              = lr$range,
    threshold_multiplier = cfg$params$lisp$threshold_multiplier,
    threshold_m          = lr$threshold,
    n_obs                = nrow(d)
  ),
  "results/step04-local-range.csv", row.names = FALSE
)

# Empirical variogram points for the diagnostic figure (Step 11).
vg <- lr$variogram$exp_var
write.csv(
  data.frame(dist_m = vg$dist, gamma = vg$gamma, np = vg$np),
  "results/step04-variogram-points.csv", row.names = FALSE
)
cat("Saved: results/step04-local-range.csv, results/step04-variogram-points.csv\n")
