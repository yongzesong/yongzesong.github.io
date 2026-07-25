# =============================================================================
# p02-cross-validation.R — observed against predicted, SDA beside FDA
# The paper's Fig. 6 equivalent: the same test samples under both dimensions.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_PRED)) {
  log_warn("ggplot2 or prediction results missing; skipping fig02/fig03")
} else {
library(ggplot2)
pr <- read_result(F_PRED); cv <- read_result(F_CV)
pr$dimension <- factor(pr$dimension, levels = c("FDA", "SDA"))
lab <- merge(cv, unique(pr[, c("dimension", "model")]))
lab$txt <- sprintf("R² = %.3f", lab$R2)

g <- ggplot(pr, aes(observed, predicted, colour = dimension)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(size = 0.7, alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, formula = y ~ x) +
  geom_text(data = lab, aes(x = -Inf, y = Inf, label = txt), hjust = -0.15,
            vjust = 1.6, size = 2.6, show.legend = FALSE) +
  facet_grid(model ~ dimension) +
  scale_colour_manual(values = PALETTE_MODEL, guide = "none") +
  labs(x = "Observed (log ppm)", y = "Predicted (log ppm)") +
  theme_bw(base_size = 8)
save_figure(g, "fig02-cross-validation", width = FIG_WIDTH_SINGLE + 3, height = 10)

# -- R2 bar comparison -------------------------------------------------------
cv$dimension <- factor(cv$dimension, levels = c("FDA", "SDA"))
g2 <- ggplot(cv, aes(model, R2, fill = dimension)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", R2)),
            position = position_dodge(width = 0.7), vjust = -0.4, size = 2.6) +
  scale_fill_manual(values = PALETTE_MODEL, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = expression(R^2*" on held-out samples")) +
  theme_bw(base_size = 8) + theme(legend.position = "top")
save_figure(g2, "fig03-r2-comparison", width = FIG_WIDTH_SINGLE + 1, height = 7)
}
