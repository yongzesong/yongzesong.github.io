# =============================================================================
# p03-methods-and-improvement.R — the method comparison, and the payoff
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping fig04/fig05")
} else {
library(ggplot2)

## -- do the three geocomplexity measures agree? -------------------------------
if (file.exists(F_GCMETHOD)) {
  m <- read_result(F_GCMETHOD)
  p <- ggplot(m, aes(variable, mean_gc, fill = method)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    scale_fill_manual(values = c(moran = "#4E79A7", spvar = "#E15759",
                                 shannon = "#59A14F"), name = "Method") +
    labs(x = NULL, y = "Mean geocomplexity") +
    theme_bw(base_size = 8) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "top")
  save_figure(p, "fig04-gc-methods", width = FIG_WIDTH_DOUBLE, height = 8)
}

## -- geocomplexity improves the models ----------------------------------------
if (file.exists(F_IMPROVE)) {
  im <- read_result(F_IMPROVE)
  im$pair <- ifelse(im$model %in% c("MLR", "GCMLR"), "Linear model",
                    "Geographically weighted regression")
  im$model <- factor(im$model, levels = c("MLR", "GCMLR", "GWR", "GeoCGWR"))
  p2 <- ggplot(im, aes(model, R2, fill = variant)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.3f", R2)), vjust = -0.4, size = 2.7) +
    facet_wrap(~ pair, scales = "free_x") +
    scale_fill_manual(values = c(baseline = "#9AA4AF", geocomplexity = "#B2182B"),
                      name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(x = NULL, y = expression(R^2)) +
    theme_bw(base_size = 8) + theme(legend.position = "top")
  save_figure(p2, "fig05-model-improvement", width = FIG_WIDTH_SINGLE + 4, height = 8)
}
}
