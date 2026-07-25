# =============================================================================
# 02_generate_sops.R — build the second dimension
# =============================================================================
# Generates multi-scale spatial outlier patterns for every covariate, following
# Equations 3 and 4 of Ren et al. (2026). This is the step that turns local
# anomalies from noise into explanatory variables, and it is the most expensive
# step in the pipeline.
#
# Output: data/interim/sopvars.rds, a named list with one data.frame per
# covariate, each holding 2 x length(buffers) columns.
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("02 GENERATE SPATIAL OUTLIER PATTERNS")

d <- readRDS(soh_cache("data"))
points <- d$points; grids <- d$grids

diag <- soh_buffer_diagnostic(points, cfg)
soh_log("buffers: ", paste(cfg$sop$buffers, collapse = ", "), " ", cfg$data$dist_unit)
soh_log("largest buffer is ", diag$largest_pct_of_max,
        " percent of the maximum pairwise distance between units")
if (diag$largest_pct_of_max > 40) {
  soh_log("the largest buffer covers most of the study area, so SOPs at that ",
          "scale approach a global summary and lose local meaning", level = "WARN ")
}
soh_log("outlier threshold: ", cfg$sop$sd_threshold, " SD    statistic: ",
        cfg$sop$statistic)

t0 <- Sys.time()
sopvars <- soh_generate_sop_all(points, grids, cfg)
soh_log("SOP generation took ",
        round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " seconds")

# --- diagnostics -------------------------------------------------------------
# An SOP column that is entirely zero carries no information. A high zero share
# usually means the buffer is too small to contain min_cells grid cells, or the
# threshold is too strict for a smooth covariate.

sop_diag <- do.call(rbind, lapply(names(sopvars), function(v) {
  b <- sopvars[[v]]
  data.frame(
    variable = v,
    n_columns = ncol(b),
    zero_share = round(mean(as.matrix(b) == 0), 3),
    mean_positive = round(mean(as.matrix(b)[as.matrix(b) > 0]), 3),
    mean_negative = round(mean(as.matrix(b)[as.matrix(b) < 0]), 3),
    stringsAsFactors = FALSE
  )
}))
soh_write_table(sop_diag, "T02_sop_diagnostics")
print(sop_diag)

flat <- sop_diag$variable[sop_diag$zero_share > 0.9]
if (length(flat)) {
  soh_log("covariates with more than 90 percent empty SOP cells: ",
          paste(flat, collapse = ", "),
          ". Lower sd_threshold, widen the smallest buffer, or accept that ",
          "these covariates have no local anomaly structure.", level = "WARN ")
}

saveRDS(sopvars, soh_cache("sopvars"))
soh_log("cached: data/interim/sopvars.rds")
