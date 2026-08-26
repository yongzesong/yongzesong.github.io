# =============================================================================
# p04-cv-comparison.R — six models under spatial block CV (paper Fig. 8, Table 2)
#
# Top rows: observed against cross-validated prediction, one panel per model.
# Bottom row: the fold-level R2 behind each column, so the reader can see how
# much of the ranking survives fold-to-fold variation.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("p04  Cross-validation comparison")

pred  <- read_result(F_CV_PRED)
folds <- read_result(F_CV_FOLDS)
summ  <- read_result(F_CV_SUMMARY)
MODEL_ORDER <- c("OK", "IDW", "LM", "RF", "RFK", "SRK")
elems <- names(ELEMENTS)

for (elem in elems) {
  p <- pred[pred$element == elem, ]
  s <- summ[summ$element == elem, ]
  f <- folds[folds$element == elem, ]
  lim <- range(c(p$obs, p$pred), na.rm = TRUE)

  save_figure(function() {
    graphics::layout(matrix(c(1, 1, 2, 2, 3, 3,
                              4, 4, 5, 5, 6, 6,
                              7, 7, 7, 8, 8, 8), nrow = 3, byrow = TRUE),
                     heights = c(1, 1, 0.95))
    for (m in MODEL_ORDER) {
      q <- p[p$model == m, ]
      hl <- m == "SRK"
      graphics::par(mar = c(3.4, 3.6, 1.9, 1.0))
      plot(q$obs, q$pred, pch = 16, cex = 0.35, asp = 1, xlim = lim, ylim = lim,
           col = if (hl) "#7d660880" else "#8fa3b880", xlab = "", ylab = "",
           cex.axis = 0.7, main = m, cex.main = 1.0,
           col.main = if (hl) "#7d6608" else "black")
      graphics::abline(0, 1, lty = 2, col = "grey35")
      graphics::mtext(sprintf("observed %s (%s)", elem, ELEMENTS[[elem]]$unit),
                      side = 1, line = 2.2, cex = 0.6)
      graphics::mtext("cross-validated prediction", side = 2, line = 2.3, cex = 0.6)
      r <- s[s$model == m, ]
      graphics::legend("topleft", bty = "n", cex = 0.68,
                       legend = c(sprintf("R2   = %.3f", r$R2),
                                  sprintf("RMSE = %.2f", r$RMSE),
                                  sprintf("MAE  = %.2f", r$MAE)))
    }
    # fold-level R2
    graphics::par(mar = c(3.2, 4.0, 2.0, 1.0))
    bx <- lapply(MODEL_ORDER, function(m) f$R2[f$model == m])
    graphics::boxplot(bx, names = MODEL_ORDER, cex.axis = 0.75, border = "grey35",
                      col = ifelse(MODEL_ORDER == "SRK", "#e8dfb8", "#e8edf2"),
                      main = "fold-level R2", cex.main = 0.95)
    graphics::points(seq_along(MODEL_ORDER),
                     vapply(bx, mean, numeric(1)), pch = 23, bg = "white", cex = 0.9)
    graphics::abline(h = mean(f$R2[f$model == "SRK"]), lty = 2, col = "#7d6608")
    graphics::mtext("R2 on the held-out block", side = 2, line = 2.5, cex = 0.65)
    # fold-level RMSE
    graphics::par(mar = c(3.2, 4.0, 2.0, 1.0))
    bx <- lapply(MODEL_ORDER, function(m) f$RMSE[f$model == m])
    graphics::boxplot(bx, names = MODEL_ORDER, cex.axis = 0.75, border = "grey35",
                      col = ifelse(MODEL_ORDER == "SRK", "#e8dfb8", "#e8edf2"),
                      main = "fold-level RMSE", cex.main = 0.95)
    graphics::points(seq_along(MODEL_ORDER),
                     vapply(bx, mean, numeric(1)), pch = 23, bg = "white", cex = 0.9)
    graphics::abline(h = mean(f$RMSE[f$model == "SRK"]), lty = 2, col = "#7d6608")
    graphics::mtext(sprintf("RMSE (%s)", ELEMENTS[[elem]]$unit), side = 2,
                    line = 2.5, cex = 0.65)
  }, sprintf("fig04-cv-comparison-%s", elem), width = FIG_WIDTH_DOUBLE, height = 19)
}
