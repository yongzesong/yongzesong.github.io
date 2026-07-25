# =============================================================================
# 70-sesu-scale.R — spatial-scale effects (SESU)
#
# OPGD also optimises the spatial unit size. The same model is re-run on the
# response aggregated to a series of unit sizes; the size where the Q values
# stabilise is the recommended analysis scale. Uses the built-in ndvi_* series.
#
# Output: results/sesu-scale.csv  (unit size x variable x Q)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()
if (!isTRUE(RUN_SESU)) { log_info("RUN_SESU is FALSE; skipping"); } else {

log_head("Step 7/7  Spatial-scale effects (SESU)")

if (!is.null(INPUT_FILE)) {
  log_warn("SESU needs the multi-scale GD datasets; skipped for custom INPUT_FILE")
} else {
  rows <- list()
  for (i in seq_along(SESU_DATASETS)) {
    ds <- load_dataset(SESU_DATASETS[i])
    gm <- gdm(stats::as.formula(paste(RESPONSE, "~",
                paste(c(CATEGORICAL, CONTINUOUS), collapse = " + "))),
              continuous_variable = CONTINUOUS, data = ds,
              discmethod = "quantile", discitv = 6)
    f <- gm$Factor.detector$Factor
    rows[[i]] <- data.frame(unit_size = SESU_SIZES[i], variable = f$variable,
                            qv = round(f$qv, 4), row.names = NULL)
    log_info("unit size %d: mean Q = %.3f across %d variables",
             SESU_SIZES[i], mean(f$qv), nrow(f))
  }
  sesu_df <- do.call(rbind, rows)
  write_result(sesu_df, F_SESU)
}
}
