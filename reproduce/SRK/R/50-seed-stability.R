# =============================================================================
# 50-seed-stability.R — how much of the SRK-RFK gap is random forest noise?
#
# Table 2 separates SRK from RFK by 0.003 in R2 and 0.06 in RMSE for Zn. The
# released scripts call ranger() without a seed, so those three forest-based
# rows move every time the analysis is run. This step repeats the whole block
# cross-validation over SEED_REPEATS forest seeds, holding the folds fixed, and
# reports the spread of each model and how often SRK actually finishes ahead.
#
# Nothing here is in the paper. It is the honest error bar the comparison needs.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("sp", "gstat", "automap", "ranger")
log_head("50  Random-forest seed stability")
t0 <- Sys.time()

MODELS <- c("RF", "RFK", "SRK")           # the seed-dependent three
# Published Table 2 SRK-minus-RFK gap in R2, for the log line and Fig. 5.
PAPER_GAP <- list(Zn = 0.003, Co = 0.008)
rows   <- list(); pairs <- list()

for (elem in names(ELEMENTS)) {
  cfg  <- ELEMENTS[[elem]]
  yvar <- cfg$yvar
  d    <- read_result(F_SAMPLES(elem))
  sv   <- read_result(F_SV(elem))
  fold <- srk_block_folds(d, CV_FOLDS, CV_BLOCK_SIZE, CV_SEED)

  for (s in seq_len(SEED_REPEATS)) {
    per_model <- list()
    for (mname in MODELS) {
      fm <- do.call(rbind, lapply(seq_len(CV_FOLDS), function(k) {
        tr <- d[fold != k, ]; te <- d[fold == k, ]
        out <- SRK_MODELS[[mname]](tr, te, yvar, cfg$xvars, sv = sv, seed = s)
        srk_metrics(te[[yvar]], out$pred)
      }))
      per_model[[mname]] <- data.frame(element = elem, seed = s, model = mname,
                                       R2 = mean(fm$R2), RMSE = mean(fm$RMSE),
                                       MAE = mean(fm$MAE))
    }
    pm <- do.call(rbind, per_model)
    rows[[length(rows) + 1L]] <- pm
    pairs[[length(pairs) + 1L]] <- data.frame(
      element = elem, seed = s,
      dR2_SRK_RFK   = pm$R2[pm$model == "SRK"]   - pm$R2[pm$model == "RFK"],
      dRMSE_SRK_RFK = pm$RMSE[pm$model == "SRK"] - pm$RMSE[pm$model == "RFK"],
      dR2_SRK_RF    = pm$R2[pm$model == "SRK"]   - pm$R2[pm$model == "RF"],
      dRMSE_SRK_RF  = pm$RMSE[pm$model == "SRK"] - pm$RMSE[pm$model == "RF"])
    if (s %% 5L == 0L) log_info("%s: %d/%d seeds", elem, s, SEED_REPEATS)
  }
}

seeds <- do.call(rbind, rows);  write_result(seeds, F_SEEDS)
prs   <- do.call(rbind, pairs); write_result(prs,   F_SEEDS_PAIR)

# -- What the spread means ---------------------------------------------------
spread <- do.call(rbind, lapply(split(seeds, list(seeds$element, seeds$model)),
  function(g) data.frame(element = g$element[1], model = g$model[1],
                         R2_mean = mean(g$R2), R2_sd = stats::sd(g$R2),
                         R2_min = min(g$R2), R2_max = max(g$R2),
                         RMSE_mean = mean(g$RMSE), RMSE_sd = stats::sd(g$RMSE))))
spread <- spread[order(spread$element, -spread$R2_mean), ]
rownames(spread) <- NULL
write_result(spread, file.path(RES_DIR, "seed-stability-spread.csv"))

for (elem in names(ELEMENTS)) {
  p <- prs[prs$element == elem, ]
  log_info("%s: SRK beats RFK on R2 in %d of %d seeds (mean dR2 = %+.4f, range %+.4f to %+.4f)",
           elem, sum(p$dR2_SRK_RFK > 0), nrow(p), mean(p$dR2_SRK_RFK),
           min(p$dR2_SRK_RFK), max(p$dR2_SRK_RFK))
  g <- spread[spread$element == elem, ]
  log_info("%s: seed-to-seed sd of R2 is %.4f (SRK), the published SRK-RFK gap is %.3f",
           elem, g$R2_sd[g$model == "SRK"], PAPER_GAP[[elem]])
}

record_runtime("50-seed-stability",
               as.numeric(difftime(Sys.time(), t0, units = "secs")))
