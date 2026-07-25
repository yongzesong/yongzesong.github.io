# =============================================================================
# p02-accuracy.R — Figure: observed against predicted, per model
# =============================================================================
# Manuscript role: Section 4.1, the conventional accuracy result that the DSI
# result is then contrasted against.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping figures")
} else {
  library(ggplot2)

  pred <- read_result(F_PREDICTIONS)
  acc  <- read_result(F_ACCURACY)
  model_cols <- setdiff(names(pred), c("lon", "lat", "observed"))

  long <- do.call(rbind, lapply(model_cols, function(m) {
    data.frame(model = m, observed = pred$observed, predicted = pred[[m]])
  }))
  long$model <- factor(long$model, levels = acc$model)

  labs_df <- data.frame(
    model = factor(acc$model, levels = acc$model),
    label = sprintf("R2 = %.3f\nRMSE = %.2f", acc$R2, acc$RMSE),
    x = min(long$observed), y = max(long$predicted)
  )

  p <- ggplot(long, aes(observed, predicted)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(alpha = 0.35, size = 0.6, colour = "#4E79A7") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = "#E15759", linewidth = 0.4) +
    geom_text(data = labs_df, aes(x, y, label = label), hjust = 0, vjust = 1,
              size = 2, lineheight = 0.95) +
    facet_wrap(~ model) +
    labs(x = sprintf("Observed %s", RESPONSE), y = sprintf("Predicted %s", RESPONSE)) +
    theme_bw(base_size = 8)

  n_facet <- length(model_cols)
  save_figure(p, "fig02-accuracy-scatter",
              width = FIG_WIDTH_DOUBLE, height = 5 * ceiling(n_facet / 3))
}
