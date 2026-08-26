# =============================================================================
# 30-singularity.R — covariate singularity features (paper Eq. 1-4, Sec. 4.3)
#
# For every retained covariate of every element, the local intensity C_k(A(s,r))
# is measured at each of the ten 2-20 km scales, and the singularity index is
# the slope of log C on log r plus two. Because the reference set is the sample
# points themselves and the covariate — not the response — supplies the values,
# the same indices could be evaluated at any unsampled location, which is what
# lets SRK survive spatial block cross-validation.
#
# The per-scale intensity matrices are cached so step 60 can re-cut the scale
# ladder without recomputing any neighbourhoods.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("30  Covariate singularity features")
t0 <- Sys.time()

diag_rows <- list()

for (elem in names(ELEMENTS)) {
  cfg  <- ELEMENTS[[elem]]
  yvar <- cfg$yvar
  d    <- read_result(F_SAMPLES(elem))

  sv <- d[, c("x", "y")]
  Cs <- list()
  for (xv in cfg$xvars) {
    C <- srk_intensity(d$x, d$y, d[[xv]], d$x, d$y, SV_SCALES,
                       min_pts_per_scale = MIN_PTS_PER_SCALE)
    a <- srk_alpha(C, SV_SCALES, min_valid_scales = MIN_VALID_SCALES)
    n_na <- sum(!is.finite(a))
    a[!is.finite(a)] <- SV_NEUTRAL
    Cs[[xv]] <- C
    sv[[paste0("sv_", xv)]] <- a

    diag_rows[[length(diag_rows) + 1L]] <- data.frame(
      element = elem, covariate = xv,
      sv_mean = mean(a), sv_sd = stats::sd(a),
      sv_min = min(a), sv_max = max(a),
      share_below_2 = mean(a < 2),           # local enrichment
      n_neutral = n_na,
      cor_with_response = stats::cor(a, d[[yvar]]),
      cor_with_covariate = stats::cor(a, d[[xv]]),
      retained = stats::sd(a) >= SV_SD_THRESHOLD)
  }
  saveRDS(Cs, file.path(DERIVED, sprintf("intensity-%s.rds", elem)))
  write_result(sv, F_SV(elem))

  keep <- srk_select_features(sv, paste0("sv_", cfg$xvars), SV_SD_THRESHOLD)
  log_info("%s: keeps %s", elem,
           if (length(keep)) paste(keep, collapse = ", ") else "(none)")
  log_info("%s: drops %s", elem,
           paste(setdiff(paste0("sv_", cfg$xvars), keep), collapse = ", "))
}

diag <- do.call(rbind, diag_rows)
write_result(diag, F_SV_DIAG)

log_info("SD separates the two covariate families cleanly:")
log_info("  lithology proximity  SD = %.2f - %.2f",
         min(diag$sv_sd[diag$retained]), max(diag$sv_sd[diag$retained]))
log_info("  terrain              SD = %.2f - %.2f",
         min(diag$sv_sd[!diag$retained]), max(diag$sv_sd[!diag$retained]))

record_runtime("30-singularity", as.numeric(difftime(Sys.time(), t0, units = "secs")))
