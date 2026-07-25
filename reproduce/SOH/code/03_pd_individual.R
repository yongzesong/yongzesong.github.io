# =============================================================================
# 03_pd_individual.R — power of determinant of single factors
# =============================================================================
# Answers the first question of the SOH model: does the second dimension of a
# covariate explain more heterogeneity than the covariate itself?
#
# Reference study result: the strongest single covariate reached PD 0.335,
# while the strongest single SOP block reached PD 0.467.
#
# Output tables
#   T03_moran            global spatial autocorrelation of the response
#   T04_individual_pd    PD and significance of every variable and SOP block
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("03 INDIVIDUAL POWER OF DETERMINANT")

d <- readRDS(soh_cache("data"))
points <- d$points
sopvars <- readRDS(soh_cache("sopvars"))

# --- spatial autocorrelation of the response ---------------------------------
# Reported for two reasons: it establishes that the response is spatially
# structured at all, and it is the caveat attached to every PD value, because
# positive autocorrelation inflates between-stratum contrast.

moran <- NULL
if (isTRUE(cfg$experiments$moran)) {
  moran <- soh_morans_i(points, k = 8, nsim = 999, seed = cfg$run$seed)
  if (!is.null(moran)) {
    soh_write_table(data.frame(statistic = moran$statistic,
                               p_value = moran$p_value,
                               n_sim = moran$n_sim, k = moran$k),
                    "T03_moran")
    soh_log(sprintf("Moran's I = %.3f (p = %.3f, %d permutations)",
                    moran$statistic, moran$p_value, moran$n_sim))
    if (moran$p_value > 0.05) {
      soh_log("the response shows no significant spatial autocorrelation; ",
              "spatial stratified heterogeneity may still exist, but check ",
              "that the analysis units are the right support", level = "WARN ")
    }
    saveRDS(moran, soh_cache("moran"))
  }
}

# --- individual PD -----------------------------------------------------------

individual <- soh_exp_individual(points, sopvars, cfg)
soh_write_table(individual, "T04_individual_pd")
print(individual[, c("label", "type", "qv", "sig", "n_strata")])

best_var <- individual[individual$type == "variable", ][1, ]
best_sop <- individual[individual$type == "SOP", ][1, ]
soh_log(sprintf("strongest covariate: %s, PD = %.3f", best_var$label, best_var$qv))
soh_log(sprintf("strongest SOP block: %s, PD = %.3f", best_sop$label, best_sop$qv))
soh_log(sprintf("second dimension changes the best single-factor PD by %+.3f",
                best_sop$qv - best_var$qv))

saveRDS(individual, soh_cache("individual"))
