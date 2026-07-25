# =============================================================================
# 30-dmu.R — LOESS utility function, marginal utility and DMU stages (Eq. 7-9)
# =============================================================================
# The trade-off relation itself: Gamma_B as a utility of Gamma_A, its marginal
# utility as a difference quotient, and the IR / MR / NR stage boundaries at
# max(eta) and at eta = 0. This step reproduces the logic behind Fig. 6.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("30  DMU: utility curve and stage boundaries")

srii <- read_result(F_SRII)

util <- stor_utility(srii$gamma_a, srii$gamma_b,
                     n_bins = N_BINS, span = LOESS_SPAN, n_eval = N_EVAL)
dmu <- stor_dmu(util$curve)

write_result(util$bins, F_BINS)
write_result(util$curve, F_UTILITY)
write_result(data.frame(boundary = c("t1 (IR-MR)", "t2 (MR-NR)"),
                        gamma_a = c(dmu$t1, dmu$t2),
                        eta = util$curve$eta[c(dmu$i1, dmu$i2)],
                        u = util$curve$u[c(dmu$i1, dmu$i2)]), F_BOUNDS)

stages <- data.frame(srii[, c("block_id", "col", "row", "x_km", "y_km")],
                     gamma_a = srii$gamma_a, gamma_b = srii$gamma_b,
                     gamma = srii$gamma,
                     stage = dmu$stage_of(srii$gamma_a))
write_result(stages, F_STAGES)

log_info("t1 = %.3f (max eta), t2 = %.3f (eta = 0); embedded truth %.3f / %.3f",
         dmu$t1, dmu$t2, TRUE_T1, TRUE_T2)
log_info("blocks per stage: %s",
         paste(sprintf("%s %d", levels(stages$stage), table(stages$stage)),
               collapse = ", "))
