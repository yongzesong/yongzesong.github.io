# =============================================================================
# p03-dsi-theta.R — Figure: the theta range per model
# =============================================================================
# Manuscript role: Section 4.2, the signature DSI figure. Each model is one
# horizontal span from theta_min to theta_max with theta_probable marked, and
# models are ordered by theta_max. The width of a span is the finding: a narrow
# span means the model handles both spatial characteristics alike, a wide span
# means it is strong on one and weak on the other.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping figures")
} else {
  library(ggplot2)

  source(file.path(R_DIR, "05-models.R"))

  dsi <- read_result(F_DSI)
  dsi <- dsi[order(-dsi$theta_max), ]      # best model on the top row
  n   <- nrow(dsi)
  dsi$row <- n:1

  # Triangle per model, following DSI paper Fig 6: base from theta_min (θ1) to
  # theta_max (θ3) on the row baseline, apex above at theta_probable (θ2), a
  # dashed drop line at θ2, and the three values annotated around the triangle.
  tri <- do.call(rbind, lapply(seq_len(n), function(i) {
    data.frame(model = dsi$model[i],
               x = c(dsi$theta_min[i], dsi$theta_max[i], dsi$theta_probable[i]),
               y = c(dsi$row[i] - 0.22, dsi$row[i] - 0.22, dsi$row[i] + 0.30))
  }))

  long_label <- vapply(as.character(dsi$model), function(m) {
    if (!is.null(MODEL_REGISTRY[[m]])) MODEL_REGISTRY[[m]]$label else m
  }, character(1))
  y_labels <- paste0(long_label, "\n(", dsi$model, ")")

  xr  <- range(c(dsi$theta_min, dsi$theta_max))
  off <- 0.035 * diff(xr)

  p_theta <- ggplot() +
    geom_hline(yintercept = dsi$row - 0.22, colour = "grey88",
               linewidth = 0.3) +
    geom_polygon(data = tri, aes(x, y, group = model),
                 fill = "#cfe8f3", colour = "black", linewidth = 0.35) +
    geom_segment(data = dsi,
                 aes(x = theta_probable, xend = theta_probable,
                     y = row - 0.22, yend = row + 0.30),
                 linetype = "22", linewidth = 0.35, colour = "grey20") +
    geom_text(data = dsi,
              aes(x = theta_min - off, y = row - 0.05,
                  label = sprintf("theta[1]~'(%.3f)'", theta_min)),
              parse = TRUE, hjust = 1, size = 2.4) +
    geom_text(data = dsi,
              aes(x = theta_max + off, y = row - 0.05,
                  label = sprintf("theta[3]~'(%.3f)'", theta_max)),
              parse = TRUE, hjust = 0, size = 2.4) +
    geom_text(data = dsi,
              aes(x = theta_probable, y = row + 0.44,
                  label = sprintf("theta[2]~'(%.3f)'", theta_probable)),
              parse = TRUE, hjust = 0.5, size = 2.4) +
    scale_y_continuous(breaks = dsi$row, labels = y_labels,
                       limits = c(0.62, n + 0.62)) +
    scale_x_continuous(limits = c(xr[1] - 5.5 * off, xr[2] + 5.5 * off)) +
    labs(x = expression(theta), y = "Models") +
    theme_minimal(base_size = 8) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black", linewidth = 0.35),
          axis.ticks = element_line(colour = "black", linewidth = 0.35))

  save_figure(p_theta, "fig03-dsi-theta",
              width = FIG_WIDTH_SINGLE + 4, height = 1.4 * n + 3)

  # Component view: the two eta values that theta is built from. A model can
  # reach a high theta_max from two mediocre components, and this panel is what
  # stops the Results section from reading that as uniform strength.
  eta_long <- rbind(
    data.frame(model = dsi$model, characteristic = "Spatial autocorrelation",
               eta = dsi$eta_autocorrelation),
    data.frame(model = dsi$model, characteristic = "Spatial heterogeneity",
               eta = dsi$eta_heterogeneity)
  )

  p_eta <- ggplot(eta_long, aes(eta, model, fill = characteristic)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_fill_manual(values = c("Spatial autocorrelation" = "#4E79A7",
                                 "Spatial heterogeneity"   = "#F28E2B"),
                      name = NULL) +
    labs(x = expression(eta), y = NULL) +
    theme_bw(base_size = 8) +
    theme(legend.position = "bottom")

  save_figure(p_eta, "fig04-dsi-components",
              width = FIG_WIDTH_SINGLE + 2, height = 1.0 * nrow(dsi) + 4)
}
