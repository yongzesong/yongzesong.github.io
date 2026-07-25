# =============================================================================
# 04_pd_interactions.R — interactions among SOPs and between SOPs and covariates
# =============================================================================
# Answers the second question of the SOH model: do second-dimension patterns
# combine with each other, and with the first dimension, to explain more than
# either alone? Each pair is classified in geographical detector terms
# (enhance bivariate, enhance nonlinear, weaken, independent).
#
# Output tables
#   T05_pairs_sop_sop    ranked SOP x SOP interactions
#   T06_pairs_var_sop    ranked covariate x SOP interactions
#   T07_categories       category-level blocks and their interactions
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("04 INTERACTIONS")

d <- readRDS(soh_cache("data"))
points <- d$points
sopvars <- readRDS(soh_cache("sopvars"))
individual <- readRDS(soh_cache("individual"))

pairs_ss <- NULL
if (isTRUE(cfg$experiments$pairs_sop_sop)) {
  soh_log("SOP x SOP pairs: ", choose(length(cfg$variables$codes), 2), " combinations")
  pairs_ss <- soh_exp_pairs(points, sopvars, individual, cfg, mode = "sop_sop")
  soh_write_table(pairs_ss, "T05_pairs_sop_sop")
  print(utils::head(pairs_ss[, c("label", "qv", "q1", "q2", "interaction")],
                    min(cfg$pd$n_top_pairs, nrow(pairs_ss))))
  saveRDS(pairs_ss, soh_cache("pairs_sop_sop"))
}

pairs_vs <- NULL
if (isTRUE(cfg$experiments$pairs_var_sop)) {
  soh_log("covariate x SOP pairs: ", length(cfg$variables$codes)^2, " combinations")
  pairs_vs <- soh_exp_pairs(points, sopvars, individual, cfg, mode = "var_sop")
  soh_write_table(pairs_vs, "T06_pairs_var_sop")
  print(utils::head(pairs_vs[, c("label", "qv", "q1", "q2", "interaction")], 10))
  saveRDS(pairs_vs, soh_cache("pairs_var_sop"))
}

categories <- NULL
if (isTRUE(cfg$experiments$categories)) {
  soh_log("category-level blocks and interactions")
  categories <- soh_exp_categories(points, sopvars, cfg)
  soh_write_table(categories, "T07_categories")
  saveRDS(categories, soh_cache("categories"))
}

# --- interaction summary -----------------------------------------------------
# The distribution of interaction types is a result in itself: a model in which
# every pair enhances nonlinearly behaves differently from one in which most
# pairs are merely bivariate.

allp <- rbind(
  if (!is.null(pairs_ss)) pairs_ss[, c("label", "qv", "interaction", "mode")],
  if (!is.null(pairs_vs)) pairs_vs[, c("label", "qv", "interaction", "mode")]
)
if (!is.null(allp)) {
  tab <- as.data.frame(table(mode = allp$mode, interaction = allp$interaction))
  tab <- tab[tab$Freq > 0, ]
  soh_write_table(tab, "T08_interaction_types")
  print(tab)
  top <- allp[order(-allp$qv), ][1, ]
  soh_log(sprintf("strongest interaction overall: %s, PD = %.3f (%s)",
                  top$label, top$qv, top$interaction))
}
