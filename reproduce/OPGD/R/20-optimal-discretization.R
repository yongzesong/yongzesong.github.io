# =============================================================================
# 20-optimal-discretization.R — the OPGD enhancement
#
# For every continuous variable, search the grid (method x break-number) and
# keep the combination with the highest factor-detector Q value. The optimally
# discretised table feeds every detector downstream.
#
# Outputs: results/optimal-discretization.csv  (best method/n/qv per variable)
#          results/discretization-curves.csv    (qv of every method x n, for Fig)
#          data/derived/discretized-data.csv    (analysis table, continuous
#                                                 variables replaced by strata)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()

log_head("Step 2/7  Optimal parameter discretisation")
d <- read_result(F_ANALYSIS)

best <- list(); curves <- list()
dcz  <- d

for (v in CONTINUOUS) {
  od <- optidisc(stats::as.formula(paste(RESPONSE, "~", v)),
                 data = d, discmethod = DISC_METHODS, discitv = DISC_INTERVALS)[[1]]
  bestq <- max(od$qv.matrix, na.rm = TRUE)
  best[[v]] <- data.frame(variable = v, method = od$method,
                          n_intervals = od$n.itv, qv = round(bestq, 4))

  # tidy the full interval x method Q surface for the optimisation figure
  # (qv.matrix rows = number of intervals, columns = discretisation methods)
  m <- od$qv.matrix
  cv <- expand.grid(n_intervals = as.integer(rownames(m)),
                    method = colnames(m), stringsAsFactors = FALSE)
  cv$variable <- v
  cv$qv <- round(as.vector(m), 4)
  curves[[v]] <- cv[, c("variable", "method", "n_intervals", "qv")]

  # replace the continuous column with its optimal strata
  dcz[[v]] <- cut(d[[v]], unique(od$itv), include.lowest = TRUE)
  log_info("%-14s best: %-9s x %d intervals  -> Q = %.4f",
           v, od$method, od$n.itv, bestq)
}

best_df <- do.call(rbind, best)
best_df <- best_df[order(-best_df$qv), ]
write_result(best_df, F_DISC)
write_result(do.call(rbind, curves), F_DISC_CURVE)
utils::write.csv(dcz, file.path(DERIVED, "discretized-data.csv"), row.names = FALSE)
log_info("wrote data/derived/discretized-data.csv (%d rows)", nrow(dcz))
