# =============================================================================
# 10-sensitivity-params.R — sensitivity of local q to the two key parameters
#
# Protocol from the cc005 R1 revision, on the top-N variables:
#   Experiment 1 — vary the local extent multiplier (default grid 1.5 / 2 / 3),
#                  discretization bins fixed at the config value
#   Experiment 2 — vary the discretization bin count (default grid 3 / 4 / 5),
#                  local extent fixed at the config multiplier
#
# Outputs: results/step10-sensitivity-threshold.csv
#          results/step10-sensitivity-discnum.csv
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

sens <- cfg$validation$sensitivity

d        <- build_model_frame(cfg)
all_vars <- all_vars_of(cfg)
n_cores  <- get_cores(cfg)

spatial  <- build_spatial(d, cfg)
vg_range <- read_local_range(cfg)$range_m

local_q_full <- read.csv("results/step05-local-q-values.csv")
mean_q_full  <- sort(colMeans(local_q_full[, all_vars], na.rm = TRUE),
                     decreasing = TRUE)
top_vars <- names(mean_q_full)[1:sens$top_n_vars]

cat(sprintf("Sensitivity analysis on top %d variables: %s\n",
            sens$top_n_vars, paste(top_vars, collapse = ", ")))

# --- Experiment 1: local extent multiplier -------------------------------------
cat("\nExperiment 1 — threshold multiplier grid:",
    paste(sens$threshold_multipliers, collapse = " / "), "\n")

exp1 <- do.call(rbind, lapply(sens$threshold_multipliers, function(m) {
  cat(sprintf("  multiplier = %.1f (threshold = %.0f m)\n", m, vg_range * m))
  lisp_out <- run_lisp_factor(
    d, cfg, vars = top_vars,
    threshold = vg_range * m,
    distmat   = spatial$distmat,
    cores     = n_cores
  )
  data.frame(
    threshold_multiplier = m,
    threshold_m = vg_range * m,
    Variable = top_vars,
    mean_local_q = sapply(top_vars, function(v) {
      mean(lisp_out$q[[v]], na.rm = TRUE)
    })
  )
}))
write.csv(exp1, "results/step10-sensitivity-threshold.csv", row.names = FALSE)

# --- Experiment 2: discretization bins ------------------------------------------
cat("\nExperiment 2 — discnum grid:",
    paste(sens$discnums, collapse = " / "), "\n")

threshold_fixed <- vg_range * cfg$params$lisp$threshold_multiplier
exp2 <- do.call(rbind, lapply(sens$discnums, function(k) {
  cat(sprintf("  discnum = %d\n", k))
  lisp_out <- run_lisp_factor(
    d, cfg, vars = top_vars,
    threshold = threshold_fixed,
    distmat   = spatial$distmat,
    discnum   = k,
    cores     = n_cores
  )
  data.frame(
    discnum = k,
    Variable = top_vars,
    mean_local_q = sapply(top_vars, function(v) {
      mean(lisp_out$q[[v]], na.rm = TRUE)
    })
  )
}))
write.csv(exp2, "results/step10-sensitivity-discnum.csv", row.names = FALSE)

cat("\nSaved: step10-sensitivity-threshold.csv, step10-sensitivity-discnum.csv\n")

cat("\nMean local q by threshold multiplier:\n")
print(reshape2::dcast(exp1, Variable ~ threshold_multiplier,
                      value.var = "mean_local_q"))
cat("\nMean local q by discnum:\n")
print(reshape2::dcast(exp2, Variable ~ discnum, value.var = "mean_local_q"))
