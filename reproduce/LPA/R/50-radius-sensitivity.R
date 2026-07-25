# =============================================================================
# 50-radius-sensitivity.R — how much the radius decides the answer
# =============================================================================
# The paper's own limitation section says the choice of local range rests partly
# on judgement. This step measures the cost of that judgement: the same model,
# the same locations, six radii.
#
# Fitting every location at six radii would take the best part of an hour, so
# the check runs on a fixed random subsample.
#
# Writes results/radius-sensitivity.csv.
# =============================================================================

log_head("Step 50 — radius sensitivity")
need_pkg("lavaan", "local structural equation models")

d <- readRDS(F_POINTS)
set.seed(SEED)
n_sample <- min(150, nrow(d))
idx <- sort(sample(nrow(d), n_sample))
log_info("%d locations at %d radii: %s km", n_sample, length(RADIUS_SENSITIVITY),
         paste(RADIUS_SENSITIVITY, collapse = ", "))

rows <- list()
for (r in RADIUS_SENSITIVITY) {
  L <- matrix(NA_real_, n_sample, nrow(PATHS))
  nlocal <- integer(n_sample); ok <- logical(n_sample)
  for (k in seq_along(idx)) {
    sub <- d[neighbours_within(d, idx[k], r), SEM_VARS, drop = FALSE]
    sub <- sub[stats::complete.cases(sub), , drop = FALSE]
    nlocal[k] <- nrow(sub)
    if (nrow(sub) < MIN_LOCAL_N) next
    f <- fit_local_sem(sub)
    L[k, ] <- f$lambda; ok[k] <- f$converged
  }
  for (j in seq_len(nrow(PATHS))) {
    v <- L[, j]; v <- v[is.finite(v) & abs(v) <= LAMBDA_PLOT_RANGE[2]]
    b <- d[[PATHS$published[j]]][idx]
    a <- L[, j]
    both <- is.finite(a) & is.finite(b) & abs(a) <= 1 & abs(b) <= 1
    rows[[length(rows) + 1]] <- data.frame(
      radius_km = r, median_neighbours = stats::median(nlocal),
      converged_pct = 100 * mean(ok),
      lambda = PATHS$lambda[j], path = PATHS$label[j],
      n_in_range = length(v),
      mean = if (length(v)) mean(v) else NA_real_,
      sd = if (length(v) > 1) stats::sd(v) else NA_real_,
      correlation_with_published =
        if (sum(both) > 3) stats::cor(a[both], b[both]) else NA_real_,
      stringsAsFactors = FALSE)
  }
  log_info("r = %6.2f km: median %3.0f neighbours, %.0f %% converged, mean |lambda| sd %.3f",
           r, stats::median(nlocal), 100 * mean(ok),
           mean(sapply(seq_len(nrow(PATHS)), function(j) {
             v <- L[, j]; v <- v[is.finite(v) & abs(v) <= 1]
             if (length(v) > 1) stats::sd(v) else NA_real_ }), na.rm = TRUE))
}
sens <- do.call(rbind, rows)
write_result(sens, F_SENS)

# The headline: does the coefficient keep its sign as the window grows?
for (j in seq_len(nrow(PATHS))) {
  s <- sens[sens$lambda == PATHS$lambda[j], ]
  log_info("%-24s mean lambda %6.3f to %6.3f across radii, %s",
           PATHS$label[j], min(s$mean, na.rm = TRUE), max(s$mean, na.rm = TRUE),
           ifelse(all(sign(s$mean) == sign(s$mean[1]), na.rm = TRUE),
                  "sign stable", "SIGN FLIPS"))
}
