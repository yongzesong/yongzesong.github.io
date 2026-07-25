# =============================================================================
# p05-dg-maps.R — Figures: local geocomplexity and the locally optimal model
# =============================================================================
# Manuscript role: Section 4.4, the spatially explicit result. Only produced
# when the DG module ran.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping figures")
} else if (!file.exists(F_DG_POINT)) {
  log_info("no DG results on disk; skipping the local figures")
} else {
  library(ggplot2)

  dg  <- read_result(F_DG_POINT)
  sm  <- read_result(F_DG_SUMMARY)
  opt <- read_result(F_DG_OPTIMAL)

  # Winsorise for display only. The ratio has local complexity in its
  # denominator, so a few points dominate the colour scale and hide the pattern
  # everywhere else. Statistics elsewhere use the untrimmed values.
  lims <- stats::quantile(dg$dg, c(0.02, 0.98), na.rm = TRUE)
  dg$dg_display <- pmin(pmax(dg$dg, lims[1]), lims[2])
  dg$model <- factor(dg$model, levels = sm$model)

  p_dg <- ggplot(dg, aes(lon, lat, colour = dg_display)) +
    geom_point(size = 0.5) +
    scale_colour_gradientn(colours = PALETTE_DIV, name = "DG",
                           limits = lims, oob = scales::squish) +
    facet_wrap(~ model) +
    coord_quickmap() +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw(base_size = 7) +
    theme(legend.key.width = unit(0.25, "cm"))

  save_figure(p_dg, "fig06-dg-maps", width = FIG_WIDTH_DOUBLE,
              height = 5 * ceiling(nlevels(dg$model) / 3))

  p_dist <- ggplot(dg, aes(dg_display, model)) +
    geom_boxplot(outlier.size = 0.2, linewidth = 0.3, fill = "#F1F1F1") +
    geom_vline(xintercept = 0, colour = "#E15759", linewidth = 0.3) +
    labs(x = "Point-wise DG (2nd-98th percentile)", y = NULL) +
    theme_bw(base_size = 8)
  save_figure(p_dist, "fig07-dg-distribution",
              width = FIG_WIDTH_SINGLE + 2, height = 1.0 * nlevels(dg$model) + 4)

  p_opt <- ggplot(opt, aes(lon, lat, colour = best_model)) +
    geom_point(size = 0.7) +
    scale_colour_manual(values = PALETTE_MODELS, name = NULL) +
    coord_quickmap() +
    labs(x = "Longitude", y = "Latitude",
         subtitle = "Model with the highest local DG at each evaluation point") +
    theme_bw(base_size = 8)
  save_figure(p_opt, "fig08-optimal-model-map",
              width = FIG_WIDTH_SINGLE + 3, height = 9)

  share <- read_result(file.path(RES_DIR, "dg-optimal-share.csv"))
  share <- share[order(share$n_locations), ]
  share$model <- factor(share$model, levels = share$model)
  p_share <- ggplot(share, aes(n_locations, model, fill = model)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = PALETTE_MODELS, guide = "none") +
    labs(x = "Number of locations where the model is locally best", y = NULL) +
    theme_bw(base_size = 8)
  save_figure(p_share, "fig09-optimal-model-share",
              width = FIG_WIDTH_SINGLE, height = 1.0 * nrow(share) + 3)
}
