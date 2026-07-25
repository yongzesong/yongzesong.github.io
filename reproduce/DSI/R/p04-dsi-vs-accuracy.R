# =============================================================================
# p04-dsi-vs-accuracy.R — Figure: interpretability against accuracy
# =============================================================================
# Manuscript role: Section 4.3, the argument of the paper in one panel. Models
# in the upper-left quadrant are accurate and spatially interpretable; models
# in the lower-right achieve accuracy while leaving spatial structure in their
# residuals, which is the case the indicator exists to detect.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping figures")
} else {
  library(ggplot2)

  cmp <- read_result(file.path(RES_DIR, "accuracy-vs-dsi.csv"))

  panel <- function(metric, xlab) {
    ggplot(cmp, aes(.data[[metric]], theta_probable)) +
      geom_segment(aes(xend = .data[[metric]], y = theta_min, yend = theta_max),
                   colour = "grey75", linewidth = 0.5) +
      geom_point(aes(colour = model), size = 2) +
      geom_text(aes(label = model), size = 2, vjust = -0.9, show.legend = FALSE) +
      scale_colour_manual(values = PALETTE_MODELS, guide = "none") +
      labs(x = xlab, y = expression(theta[probable])) +
      theme_bw(base_size = 8)
  }

  p_r2   <- panel("R2", expression(R^2))
  p_rmse <- panel("RMSE", "RMSE")
  p_mae  <- panel("MAE", "MAE")

  save_figure(p_r2,   "fig05a-dsi-vs-r2",   width = FIG_WIDTH_SINGLE, height = 8)
  save_figure(p_rmse, "fig05b-dsi-vs-rmse", width = FIG_WIDTH_SINGLE, height = 8)
  save_figure(p_mae,  "fig05c-dsi-vs-mae",  width = FIG_WIDTH_SINGLE, height = 8)

  if (has_pkg("patchwork")) {
    combined <- patchwork::wrap_plots(p_r2, p_rmse, p_mae, nrow = 1)
    save_figure(combined, "fig05-dsi-vs-accuracy",
                width = FIG_WIDTH_DOUBLE, height = 7)
  }

  # The rank disagreement stated as a number, for the Results sentence.
  rho <- suppressWarnings(stats::cor(cmp$R2, cmp$theta_probable, method = "spearman"))
  writeLines(c(
    sprintf("Spearman rho between R2 and theta_probable: %.3f", rho),
    sprintf("Highest R2: %s (%.3f), theta_probable = %.3f",
            cmp$model[which.max(cmp$R2)], max(cmp$R2),
            cmp$theta_probable[which.max(cmp$R2)]),
    sprintf("Highest theta_probable: %s (%.3f), R2 = %.3f",
            cmp$model[which.max(cmp$theta_probable)], max(cmp$theta_probable),
            cmp$R2[which.max(cmp$theta_probable)])
  ), file.path(RES_DIR, "accuracy-dsi-disagreement.txt"))
  log_info("wrote results/accuracy-dsi-disagreement.txt")
}
