# =============================================================================
# p06-sensitivity.R — Figure: DSI across neighbourhood sizes
# =============================================================================
# Manuscript role: Section 5 (Discussion) or the supplementary material. Flat
# lines are the claim: the model ranking is a property of the models, not of
# the neighbourhood size chosen to measure them.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping figures")
} else if (!file.exists(F_SENSITIVITY)) {
  log_info("no sensitivity results on disk; skipping")
} else {
  library(ggplot2)

  sens <- read_result(F_SENSITIVITY)

  p <- ggplot(sens, aes(k, theta_probable, colour = model, group = model)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.2) +
    geom_vline(xintercept = K_MORAN, linetype = "dashed",
               colour = "grey50", linewidth = 0.3) +
    scale_colour_manual(values = PALETTE_MODELS, name = NULL) +
    labs(x = "Number of nearest neighbours (k)", y = expression(theta[probable])) +
    theme_bw(base_size = 8)

  save_figure(p, "fig10-sensitivity-k", width = FIG_WIDTH_SINGLE + 3, height = 8)

  strata_path <- file.path(RES_DIR, "sensitivity-strata.csv")
  if (file.exists(strata_path)) {
    ss <- read_result(strata_path)
    p_s <- ggplot(ss, aes(eta_heterogeneity_primary, eta_heterogeneity_alternative)) +
      geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.3) +
      geom_point(aes(colour = model), size = 2) +
      geom_text(aes(label = model), size = 2, vjust = -0.9) +
      scale_colour_manual(values = PALETTE_MODELS, guide = "none") +
      labs(x = sprintf("eta (heterogeneity), %s strata", ss$strata_primary[1]),
           y = sprintf("eta (heterogeneity), %s strata", ss$strata_alternative[1])) +
      theme_bw(base_size = 8)
    save_figure(p_s, "fig11-sensitivity-strata", width = FIG_WIDTH_SINGLE, height = 8)
  }
}
