# =============================================================================
# 30-local-sem.R — one structural equation model per location
# =============================================================================
# The second and third steps of LPA in one loop: extract the neighbourhood, fit
# the SEM inside it, keep the seven path coefficients, move to the next
# location. Everything spatial about the method lives in the radius; everything
# causal lives in SEM_MODEL.
#
# Roughly 0.25 s per location, so about 8 minutes for the full 1,801.
#
# Writes results/local-lambda-recomputed.csv and
#        results/local-fit-diagnostics.csv.
# =============================================================================

log_head("Step 30 — local path coefficients")
need_pkg("lavaan", "local structural equation models")

d     <- readRDS(F_POINTS)
R_OPT <- readRDS(file.path(DERIVED, "r-opt.rds"))
nb    <- readRDS(file.path(DERIVED, "neighbour-counts.rds"))

idx <- seq_len(nrow(d))
if (!is.null(N_LOCATIONS) && N_LOCATIONS < nrow(d)) {
  idx <- sort(sample(idx, N_LOCATIONS))
  log_info("fitting a random subsample of %d locations", length(idx))
}
log_info("radius %.2f km, %d locations, model:%s", R_OPT, length(idx),
         gsub("\n\\s*", " ", SEM_MODEL))

L <- matrix(NA_real_, length(idx), nrow(PATHS))
P <- matrix(NA_real_, length(idx), nrow(PATHS))
diag_n <- integer(length(idx)); diag_ok <- logical(length(idx))
diag_note <- character(length(idx))

t0 <- Sys.time()
for (k in seq_along(idx)) {
  i   <- idx[k]
  sub <- d[neighbours_within(d, i, R_OPT), SEM_VARS, drop = FALSE]
  sub <- sub[stats::complete.cases(sub), , drop = FALSE]
  if (nrow(sub) < MIN_LOCAL_N) {
    diag_n[k] <- nrow(sub); diag_note[k] <- "too few neighbours"; next
  }
  f <- fit_local_sem(sub)
  L[k, ] <- f$lambda; P[k, ] <- f$p
  diag_n[k] <- f$n; diag_ok[k] <- f$converged; diag_note[k] <- f$note
  if (k %% 200 == 0)
    log_info("  %d/%d locations, %.0f s elapsed", k, length(idx),
             as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
log_info("finished in %.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs")))

colnames(L) <- PATHS$published
colnames(P) <- paste0("sig.", PATHS$published)
out <- cbind(data.frame(id = idx, x = d[[COORD_X]][idx], y = d[[COORD_Y]][idx],
                        px = d$px[idx], py = d$py[idx], n_local = diag_n),
             as.data.frame(L), as.data.frame(P))
write_result(out, F_LAMBDA)

diagnostics <- data.frame(id = idx, n_local = diag_n, converged = diag_ok,
                          note = diag_note, neighbours_at_radius = nb[idx],
                          stringsAsFactors = FALSE)
write_result(diagnostics, F_FITLOG)

log_info("converged at %d of %d locations (%.1f %%)",
         sum(diag_ok), length(idx), 100 * mean(diag_ok))
notes <- table(diag_note[!diag_ok])
if (length(notes))
  for (nm in names(notes)) log_info("  %-20s %d", nm, notes[[nm]])

# A quick look at what came out, before any filtering hides the tails.
for (j in seq_len(nrow(PATHS))) {
  v <- L[, j]; v <- v[is.finite(v)]
  inr <- v[v >= LAMBDA_PLOT_RANGE[1] & v <= LAMBDA_PLOT_RANGE[2]]
  log_info("%-24s n=%4d  mean=%7.3f  in [-1,1]: n=%4d mean=%7.3f",
           PATHS$label[j], length(v), mean(v), length(inr),
           if (length(inr)) mean(inr) else NA_real_)
}
