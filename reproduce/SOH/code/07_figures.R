# =============================================================================
# 07_figures.R — every figure in the manuscript
# =============================================================================
# Figure numbering follows the reference study so panels can be compared.
# See 00_docs/04-figure-table-plan.md for what each figure has to prove.
#
#   F01  response map                     Fig 2  in Ren et al. (2026)
#   F02  covariate maps                   Fig 3
#   F03  Moran's I                        Fig 5
#   F04  SOP maps across buffers          Fig 6
#   F05  individual PD                    Fig 7
#   F06  interaction heatmaps             Fig 8
#   F07  augmentation gain                Fig 9
#   F08  overall scenarios                Fig 10a
#   F09  scale sensitivity                Fig 10b
#   F10  threshold sensitivity            new, supplementary
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("07 FIGURES")

d <- readRDS(soh_cache("data"))
points <- d$points; grids <- d$grids
sopvars <- readRDS(soh_cache("sopvars"))

rd <- function(name) {
  f <- soh_cache(name)
  if (file.exists(f)) readRDS(f) else NULL
}

soh_save_figure(soh_fig_response_map(points, cfg), "F01_response_map", cfg,
                width = 110, height = 100)

soh_save_figure(soh_fig_covariate_maps(grids, cfg), "F02_covariate_maps", cfg,
                width = 180, height = 120)

moran <- rd("moran")
if (!is.null(moran)) {
  soh_save_figure(soh_fig_moran(moran, cfg), "F03_moran", cfg,
                  width = 180, height = 80)
}

# SOP maps for the covariate with the strongest SOP block
individual <- rd("individual")
if (!is.null(individual)) {
  top_sop_var <- individual$variable[individual$type == "SOP"][1]
  soh_save_figure(soh_fig_sop_maps(points, sopvars, cfg, top_sop_var),
                  "F04_sop_maps", cfg, width = 180, height = 100)

  soh_save_figure(soh_fig_individual_pd(individual, cfg), "F05_individual_pd",
                  cfg, width = 130, height = 110)
}

pss <- rd("pairs_sop_sop")
if (!is.null(pss)) {
  soh_save_figure(soh_fig_interaction_heatmap(pss, cfg, "(a) SOP x SOP"),
                  "F06a_interaction_sop_sop", cfg, width = 120, height = 105)
}
pvs <- rd("pairs_var_sop")
if (!is.null(pvs)) {
  soh_save_figure(soh_fig_interaction_heatmap(pvs, cfg, "(b) Covariate x SOP"),
                  "F06b_interaction_var_sop", cfg, width = 120, height = 105)
}

aug <- rd("augmentation")
if (!is.null(aug)) {
  soh_save_figure(soh_fig_augmentation(aug, cfg, "variable"),
                  "F07a_augmentation_variable", cfg, width = 180, height = 95)
  soh_save_figure(soh_fig_augmentation(aug, cfg, "category"),
                  "F07b_augmentation_category", cfg, width = 180, height = 80)
}

overall <- rd("overall")
if (!is.null(overall)) {
  soh_save_figure(soh_fig_overall(overall, cfg), "F08_overall_scenarios", cfg,
                  width = 110, height = 95)
}

scale_df <- rd("scale")
if (!is.null(scale_df)) {
  soh_save_figure(soh_fig_scale(scale_df, cfg), "F09_scale_sensitivity", cfg,
                  width = 130, height = 105)
}

thr_df <- rd("threshold")
if (!is.null(thr_df)) {
  soh_save_figure(soh_fig_threshold(thr_df, cfg), "F10_threshold_sensitivity",
                  cfg, width = 130, height = 100)
}

soh_log("figures complete")
