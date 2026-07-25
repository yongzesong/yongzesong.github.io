# =============================================================================
# test-02-recover-simulation.R — the pipeline recovers what the simulation hid
#
# The simulation embeds a known utility curve (boundaries TRUE_T1, TRUE_T2),
# latent quantity / quality fields, and drifting income coefficients. If the
# pipeline recovers them, the whole chain — entropy SRII, LOESS + marginal
# utility, DMU staging, LISA overlay, GAM / GWR — works end to end.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("== test-02  Recover the simulated ground truth ============================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-56s %s\n", label, if (ok) "pass" else "FAIL"))
  pass <<- pass + ok
}

for (f in c(F_TRUTH, F_SRII, F_BOUNDS, F_STAGES, F_LISA, F_CONTRIB, F_GWRCOEF))
  if (!file.exists(f)) stop("missing ", f, " — run `Rscript run-all.R` first")

truth  <- read_result(F_TRUTH)
srii   <- read_result(F_SRII)
bounds <- read_result(F_BOUNDS)
stages <- read_result(F_STAGES)
lisa   <- read_result(F_LISA)
contrib<- read_result(F_CONTRIB)
gw     <- read_result(F_GWRCOEF)

## -- The indices see the latent dimensions ------------------------------------
r_a <- stats::cor(srii$gamma_a, truth$q_true)
r_b <- stats::cor(srii$gamma_b, truth$qual_true)
check(sprintf("Gamma_A tracks latent quantity (r = %.3f)", r_a), r_a > 0.95)
check(sprintf("Gamma_B tracks latent quality (r = %.3f)", r_b), r_b > 0.90)

## -- The DMU boundaries are recovered -----------------------------------------
t1 <- bounds$gamma_a[1]; t2 <- bounds$gamma_a[2]
check(sprintf("t1 = %.3f is within 0.05 of the embedded %.3f", t1, TRUE_T1),
      abs(t1 - TRUE_T1) < 0.05)
check(sprintf("t2 = %.3f is within 0.08 of the embedded %.3f", t2, TRUE_T2),
      abs(t2 - TRUE_T2) < 0.08)

## -- Stage classification agrees with the truth -------------------------------
true_stage <- ifelse(truth$q_true < TRUE_T1, "IR",
                     ifelse(truth$q_true < TRUE_T2, "MR", "NR"))
agree <- mean(stages$stage == true_stage)
check(sprintf("stage labels agree with the truth (%.0f%%)", 100 * agree),
      agree > 0.85)

## -- Clusters sit where the simulation put them -------------------------------
# Joint hotspots ring the city rather than covering its core: past t2 the
# quality dimension declines again (the NR stage), so the most urban blocks
# are not HH in both dimensions — the trade-off itself keeps the ratio modest.
hot <- lisa$cluster == "Hotspot"
check("hotspots are urban blocks (mean urbanisation > 1.5x overall)",
      sum(hot) > 0 &&
      mean(truth$urbanisation[hot]) > 1.5 * mean(truth$urbanisation))
cold <- lisa$cluster == "Cold spot"
check("cold spots are remote blocks (below-average urbanisation)",
      sum(cold) > 0 &&
      mean(truth$urbanisation[cold]) < mean(truth$urbanisation))

## -- Income attribution -------------------------------------------------------
mean_c <- contrib[contrib$model == "Mean", ]
c_a <- mean_c$contribution[mean_c$dimension == "Gamma_A"]
c_b <- mean_c$contribution[mean_c$dimension == "Gamma_B"]
check(sprintf("quality contributes more than quantity (%.1f%% vs %.1f%%)",
              100 * c_b, 100 * c_a), c_b > c_a)
# The simulated quality coefficient grows northward; the GWR surface must too.
r_bb <- stats::cor(gw$beta_b, truth$beta_b)
check(sprintf("GWR recovers the northward drift of beta_B (r = %.2f)", r_bb),
      r_bb > 0.5)

cat(sprintf("   -- %d/%d checks passed --\n", pass, total))
if (pass < total) stop("test-02 failed")
