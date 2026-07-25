# =============================================================================
# 05_validation.R — does the second dimension actually add anything?
# =============================================================================
# Three checks, in increasing strength.
#
#   1. Augmentation. For each covariate and each thematic category, compare PD
#      of the covariate alone against PD of the covariate plus its own SOP
#      block. A positive gain is the core claim of the SOH model.
#   2. Overall scenarios. A1 SOPs only, A2 both dimensions, A3 covariates only.
#   3. Null check. Shuffle the SOP rows and repeat scenario A1. Because CART
#      can manufacture strata from any high-dimensional predictor set, a
#      permuted SOP set establishes the PD a study should treat as zero.
#
# The null check is not in the reference study. It is added here because the
# number of SOP columns grows as 2 x variables x buffers, and a reviewer will
# ask whether the gain is information or dimensionality.
#
# Output tables
#   T09_augmentation      per variable and per category gain
#   T10_overall           the three scenarios
#   T11_permutation_null  permuted SOP baseline
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("05 VALIDATION")

d <- readRDS(soh_cache("data"))
points <- d$points
sopvars <- readRDS(soh_cache("sopvars"))

aug <- NULL
if (isTRUE(cfg$experiments$augmentation)) {
  aug <- soh_exp_augmentation(points, sopvars, cfg)
  soh_write_table(aug, "T09_augmentation")
  print(aug)
  gained <- sum(aug$delta > 0)
  soh_log(sprintf("SOPs raise PD for %d of %d units, median gain %+.3f",
                  gained, nrow(aug), stats::median(aug$delta)))
  saveRDS(aug, soh_cache("augmentation"))
}

overall <- NULL
if (isTRUE(cfg$experiments$overall)) {
  overall <- soh_exp_overall(points, sopvars, cfg)
  soh_write_table(overall, "T10_overall")
  print(overall[, c("label", "qv", "sig", "n_strata", "n_predictors")])
  a1 <- overall$qv[overall$scenario == "A1"]
  a2 <- overall$qv[overall$scenario == "A2"]
  a3 <- overall$qv[overall$scenario == "A3"]
  soh_log(sprintf("A1 SOP only %.3f | A2 both %.3f | A3 covariates only %.3f",
                  a1, a2, a3))
  soh_log(sprintf("adding the second dimension changes overall PD by %+.3f (%+.1f percent)",
                  a2 - a3, 100 * (a2 - a3) / a3))
  if (a2 < a1) {
    soh_log("the combined model scores below the SOP-only model. CART selects ",
            "splits greedily, so a larger predictor set does not guarantee a ",
            "higher PD. Report both scenarios rather than the larger one.",
            level = "WARN ")
  }
  saveRDS(overall, soh_cache("overall"))
}

# --- permutation nulls -------------------------------------------------------
# Two nulls, and the second is the one that decides the paper.
#
# Marginal null. Shuffle the SOP rows and recompute the SOP-only PD. This asks
# whether SOPs beat noise of the same dimensionality. It is necessary but far
# too weak on its own, because SOPs summarise the covariate field around each
# unit and are therefore correlated with the covariate at the unit. A study
# whose response depends only on the covariates still produces a high SOP-only
# PD and still clears this null.
#
# Conditional null. Shuffle the SOP rows and recompute PD of covariates plus
# shuffled SOPs, then compare against covariates plus real SOPs. This asks the
# question the paper actually claims: do the outlier patterns add anything
# BEYOND the first dimension, once dimensionality is controlled for? A study
# that fails here has no SOH result, whatever the marginal null says.

n_perm <- 50
y <- points$response
Xs <- soh_sop_wide(sopvars)
Xv <- points[, cfg$variables$codes, drop = FALSE]

obs_a1 <- if (!is.null(overall)) overall$qv[overall$scenario == "A1"] else
  soh_pd_of(y, Xs, cfg)$qv
obs_a2 <- if (!is.null(overall)) overall$qv[overall$scenario == "A2"] else
  soh_pd_of(y, cbind(Xv, Xs), cfg)$qv
obs_a3 <- if (!is.null(overall)) overall$qv[overall$scenario == "A3"] else
  soh_pd_of(y, Xv, cfg)$qv

soh_log("permutation nulls: ", n_perm, " shuffles, marginal and conditional")
set.seed(cfg$run$seed)
perm <- lapply(seq_len(n_perm), function(i) {
  idx <- sample(nrow(Xs))
  Xp <- Xs[idx, , drop = FALSE]
  c(marginal = soh_pd_of(y, Xp, cfg)$qv,
    conditional = soh_pd_of(y, cbind(Xv, Xp), cfg)$qv)
})
null_marg <- vapply(perm, `[`, numeric(1), "marginal")
null_cond <- vapply(perm, `[`, numeric(1), "conditional")

null_tab <- data.frame(
  test = c(rep("marginal (SOP vs shuffled SOP)", 4),
           rep("conditional (covariates + SOP vs covariates + shuffled SOP)", 5)),
  quantity = c("observed", "null mean", "null 95th percentile", "empirical p",
               "observed", "null mean", "null 95th percentile", "empirical p",
               "covariates only (A3)"),
  value = c(obs_a1, mean(null_marg), stats::quantile(null_marg, 0.95),
            (1 + sum(null_marg >= obs_a1)) / (1 + n_perm),
            obs_a2, mean(null_cond), stats::quantile(null_cond, 0.95),
            (1 + sum(null_cond >= obs_a2)) / (1 + n_perm),
            obs_a3)
)
soh_write_table(null_tab, "T11_permutation_null")
print(null_tab)

if (obs_a1 <= stats::quantile(null_marg, 0.95)) {
  soh_log("marginal null NOT cleared: SOP-only PD sits inside the shuffled ",
          "range, so the SOP columns behave like noise", level = "WARN ")
} else {
  soh_log(sprintf("marginal null cleared: %.3f against a null mean of %.3f",
                  obs_a1, mean(null_marg)))
}

if (obs_a2 <= stats::quantile(null_cond, 0.95)) {
  soh_log("CONDITIONAL NULL NOT CLEARED. Adding real SOPs to the covariates ",
          "does no better than adding shuffled ones. The apparent explanatory ",
          "power of the second dimension is dimensionality plus its correlation ",
          "with the covariate field, not local anomaly information. Report this ",
          "as a null result rather than reporting the SOP-only PD.",
          level = "WARN ")
} else {
  soh_log(sprintf("conditional null cleared: %.3f against a null mean of %.3f, ",
                  obs_a2, mean(null_cond)),
          "so the second dimension adds information beyond the first")
}

saveRDS(list(null_marginal = null_marg, null_conditional = null_cond,
             observed = c(A1 = obs_a1, A2 = obs_a2, A3 = obs_a3)),
        soh_cache("null"))
