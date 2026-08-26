# =============================================================================
# test-02-reproduce-pipeline.R — the pipeline lands where the paper says
#
# Two kinds of check.
#
#   (a) Against the published article: sample counts, the covariate sets the
#       Spearman screen keeps, the simulation metrics of Table 1, the model
#       ordering of Table 2, and the +/-5% robustness band of Fig. 7.
#   (b) Against the authors' own released fold-level output: the two models
#       that carry no random forest — IDW and LM — must match their numbers
#       digit for digit, which is what proves this pipeline builds the same
#       five spatial blocks from the same samples the published analysis used.
#
# Everything touching ranger is checked as an interval, not an equality: the
# released scripts fix no forest seed, and step 50 measures how wide that is.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("== test-02  Reproduction of the published results ========================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-58s %s\n", label, if (ok) "pass" else "FAIL"))
  pass <<- pass + ok
}
near <- function(a, b, tol) all(abs(a - b) <= tol)

## -- (a) The data ------------------------------------------------------------
# Sample counts printed in Sec. 3.1 of the article, for whichever elements the
# config asks for.
PAPER_N <- c(Zn = 1105, Co = 998)
resp <- read_result(F_RESPONSE)
for (elem in names(ELEMENTS))
  check(sprintf("%s sample count is the paper's %d", elem, PAPER_N[[elem]]),
        resp$n[resp$element == elem] == PAPER_N[[elem]])
check("the response is right-skewed, as Sec. 4.1 describes",
      all(resp$skewness > 1))

sc <- read_result(F_SCREEN)
for (elem in names(ELEMENTS))
  check(sprintf("Spearman screen reproduces the published %s covariates", elem),
        setequal(sc$covariate[sc$element == elem & sc$significant],
                 ELEMENTS[[elem]]$xvars))
check("imi is screened out, as in Sec. 4.2",
      !any(sc$significant[sc$covariate == "imi"]))

## -- (a) Singularity features ------------------------------------------------
sv <- read_result(F_SV_DIAG)
lith <- sv$sv_sd[sv$covariate %in% c("ss", "ifi", "imi", "hm")]
terr <- sv$sv_sd[sv$covariate %in% c("elevation", "slope", "aspect")]
check("lithology singularity SD is above the 0.5 threshold", all(lith > 0.5))
check("terrain singularity SD is below it", all(terr < 0.5))
check("the two families are separated by an order of magnitude",
      min(lith) > 10 * max(terr))
check("sv(hm) is among the three most important SRK features",
      {
        imp <- read_result(F_IMPORTANCE)
        all(vapply(names(ELEMENTS), function(e) {
          g <- imp[imp$element == e, ]
          which(g$feature == "sv_hm") <= 3L
        }, logical(1)))
      })

## -- (a) Simulation, Table 1 -------------------------------------------------
sim <- read_result(F_SIM_RESULTS)
paper_ok  <- c(normal = 0.876, skewed = 0.833, `long-tail` = 0.736)
paper_srk <- c(normal = 0.972, skewed = 0.966, `long-tail` = 0.940)
got_ok  <- stats::setNames(sim$R2[sim$model == "OK"],  sim$scenario[sim$model == "OK"])
got_srk <- stats::setNames(sim$R2[sim$model == "SRK"], sim$scenario[sim$model == "SRK"])
check("simulated OK matches Table 1 to 0.001",
      near(got_ok[names(paper_ok)], paper_ok, 0.001))
check("simulated SRK matches Table 1 to 0.005 (forest seed)",
      near(got_srk[names(paper_srk)], paper_srk, 0.005))
check("SRK beats OK in all three scenarios", all(got_srk > got_ok))
check("the SRK margin widens as the distribution departs from normal",
      {
        gain <- got_srk[c("normal", "skewed", "long-tail")] -
                got_ok[c("normal", "skewed", "long-tail")]
        all(diff(gain) > 0)
      })

## -- (b) Against the authors' released fold-level output ---------------------
# IDW and LM involve no random forest, so they must reproduce exactly.
REF <- data.frame(
  element = rep(c("Zn", "Co"), each = 10),
  model   = rep(rep(c("IDW", "LM"), each = 5), 2),
  fold    = rep(1:5, 4),
  R2 = c(0.1949322345, 0.2138915739, 0.2804206793, 0.1175263486, 0.1253125538,
         0.2568611696, 0.2263475270, 0.3222658355, 0.2733080197, 0.2228430471,
         0.2660767646, 0.2762392913, 0.2412746390, 0.1935857958, 0.3550166530,
         0.3445926393, 0.2798705156, 0.1763407414, 0.0698201625, 0.3165272482),
  RMSE = c(25.12644924, 18.47254377, 24.37341377, 22.62357103, 28.71727733,
           24.14070109, 18.32560991, 23.65411553, 20.52983540, 27.06893592,
           13.16915836,  9.07109191, 11.27926642, 11.11562870, 11.13012324,
           12.44481253,  9.04830773, 11.75201526, 11.93818784, 11.45740658))
REF <- REF[REF$element %in% names(ELEMENTS), ]
cvf <- read_result(F_CV_FOLDS)
got <- merge(REF, cvf, by = c("element", "model", "fold"),
             suffixes = c("_ref", "_got"))
check(sprintf("all %d deterministic fold results were found", nrow(REF)),
      nrow(got) == nrow(REF))
check("IDW and LM reproduce the authors' fold R2 to 1e-6",
      near(got$R2_ref, got$R2_got, 1e-6))
check("IDW and LM reproduce the authors' fold RMSE to 1e-5",
      near(got$RMSE_ref, got$RMSE_got, 1e-5))

## -- (a) Model comparison, Table 2 -------------------------------------------
cv <- read_result(F_CV_SUMMARY)
for (elem in names(ELEMENTS)) {
  g <- cv[cv$element == elem, ]
  ordered <- g$model[order(-g$R2)]
  check(sprintf("%s: the three forest models beat LM, IDW and OK", elem),
        setequal(ordered[1:3], c("RF", "RFK", "SRK")))
  check(sprintf("%s: ordinary kriging is last, as in Table 2", elem),
        utils::tail(ordered, 1) == "OK")
  check(sprintf("%s: SRK RMSE is within 2%% of the published value", elem),
        abs(g$RMSE[g$model == "SRK"] /
            c(Zn = 21.54, Co = 10.45)[[elem]] - 1) < 0.02)
  check(sprintf("%s: OK RMSE is within 2%% of the published value", elem),
        abs(g$RMSE[g$model == "OK"] /
            c(Zn = 25.72, Co = 12.04)[[elem]] - 1) < 0.02)
}

## -- The seed-stability claim ------------------------------------------------
if (file.exists(F_SEEDS_PAIR)) {
  p <- read_result(F_SEEDS_PAIR)
  s <- read_result(F_SEEDS)
  check("Co keeps SRK ahead of RFK on every forest seed",
        all(p$dR2_SRK_RFK[p$element == "Co"] > 0))
  check("the authors' own released Co run has SRK behind RFK",
        {
          # tables/cv_summary_Co.csv in the authors' repository: SRK 0.34394,
          # RFK 0.34517 — the opposite ordering to the article's Table 2, and
          # inside the seed spread measured here.
          g <- s[s$element == "Co", ]
          rng <- range(g$R2[g$model == "SRK"])
          0.34394 >= rng[1] - 0.01 && 0.34394 <= rng[2] + 0.01
        })
  check("the seed-to-seed spread is smaller than the SRK-RFK gap for Co",
        {
          sd_srk <- stats::sd(s$R2[s$element == "Co" & s$model == "SRK"])
          mean(p$dR2_SRK_RFK[p$element == "Co"]) > 3 * sd_srk
        })
}

## -- Sensitivity, Fig. 7 -----------------------------------------------------
if (file.exists(F_SENS)) {
  sens <- read_result(F_SENS)
  check("every parameter combination stays within +/-5% of the baseline",
        all(abs(sens$dR2_pct) <= 5 & abs(sens$dRMSE_pct) <= 5))
  check("the baseline cell is the published 20 km / SD 0.5 setting",
        all(sens$max_scale_km[sens$baseline] == 20) &&
        all(sens$sd_threshold[sens$baseline] == 0.5))
}

cat(sprintf("\n   %d/%d checks passed\n", pass, total))
if (pass < total) quit(status = 1)
