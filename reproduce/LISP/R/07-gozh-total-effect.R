# =============================================================================
# 07-gozh-total-effect.R — GOZH total effect of all variables together
#
# GOZH (Luo et al. 2022) zones the study area with a regression tree over all
# predictors, then measures the combined q of the leaf zones. Run globally
# (one q for the whole area) and locally (q inside each location's k-nearest
# neighbourhood), as in cc005 Step 4.
#
# Outputs: results/step07-gozh-global.csv
#          results/step07-local-gozh.csv
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-lpi-core.R")

library(sf)
library(rpart)
library(gdverse)
library(FNN)

cfg <- load_config()
ensure_dir("results")

d        <- build_model_frame(cfg)
all_vars <- all_vars_of(cfg)
resp     <- cfg$response$response_name

spatial <- build_spatial(d, cfg)
k_local <- max(cfg$params$gc$knn_min,
               round(nrow(d) * cfg$params$gc$knn_share))

# --- Global GOZH --------------------------------------------------------------
cat("Running global GOZH on the full dataset...\n")
gozh_global <- run_gozh(d, resp, all_vars)
cat(sprintf("Global GOZH q = %.4f  p = %.4g\n",
            gozh_global["q"], gozh_global["p"]))

write.csv(
  data.frame(model = "GOZH_global",
             q_value = gozh_global["q"], p_value = gozh_global["p"]),
  "results/step07-gozh-global.csv", row.names = FALSE
)

# --- Local GOZH ----------------------------------------------------------------
cat(sprintf("Running local GOZH for %d locations (k = %d)...\n",
            nrow(d), k_local))
local_gozh <- run_local_gozh(d, cfg, all_vars, spatial$coords_mat, k_local)

out <- cbind(
  d[, c(cfg$data$id_col, cfg$data$x_col, cfg$data$y_col)],
  local_gozh
)
write.csv(out, "results/step07-local-gozh.csv", row.names = FALSE)
cat("Saved: results/step07-gozh-global.csv, results/step07-local-gozh.csv\n")

cat(sprintf("Local GOZH q: mean=%.4f  min=%.4f  max=%.4f  (%%p<0.05: %.1f)\n",
            mean(local_gozh$local_gozh,   na.rm = TRUE),
            min(local_gozh$local_gozh,    na.rm = TRUE),
            max(local_gozh$local_gozh,    na.rm = TRUE),
            mean(local_gozh$local_gozh_p < 0.05, na.rm = TRUE) * 100))
