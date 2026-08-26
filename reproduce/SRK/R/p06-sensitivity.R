# =============================================================================
# p06-sensitivity.R — parameter sensitivity heatmaps (paper Fig. 7)
#
# R2 and RMSE across the grid of maximum singularity scale and SD threshold,
# with the published baseline cell outlined, plus the relative-change panel
# that carries the paper's +/-5% robustness band.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("p06  Parameter sensitivity")

sens  <- read_result(F_SENS)
elems <- names(ELEMENTS)

heat <- function(g, value, main, pal, rev = TRUE, digits = 3) {
  ux <- sort(unique(g$max_scale_km)); uy <- sort(unique(g$sd_threshold))
  m  <- matrix(NA_real_, length(ux), length(uy))
  m[cbind(match(g$max_scale_km, ux), match(g$sd_threshold, uy))] <- g[[value]]
  cols <- grDevices::hcl.colors(64, pal, rev = rev)
  graphics::par(mar = c(3.4, 3.8, 2.0, 1.0))
  graphics::image(seq_along(ux), seq_along(uy), m, col = cols, axes = FALSE,
                  xlab = "", ylab = "", main = main, cex.main = 0.92)
  graphics::axis(1, at = seq_along(ux), labels = ux, cex.axis = 0.7, tick = FALSE)
  graphics::axis(2, at = seq_along(uy), labels = uy, cex.axis = 0.7, las = 1,
                 tick = FALSE)
  graphics::box()
  for (a in seq_along(ux)) for (b in seq_along(uy))
    graphics::text(a, b, formatC(m[a, b], format = "f", digits = digits), cex = 0.6)
  bl <- g[g$baseline, ]
  graphics::rect(match(bl$max_scale_km, ux) - 0.5, match(bl$sd_threshold, uy) - 0.5,
                 match(bl$max_scale_km, ux) + 0.5, match(bl$sd_threshold, uy) + 0.5,
                 border = "#7d6608", lwd = 2.4)
  graphics::mtext("maximum singularity scale (km)", side = 1, line = 2.2, cex = 0.65)
  graphics::mtext("sv SD threshold", side = 2, line = 2.6, cex = 0.65)
}

save_figure(function() {
  n <- length(elems)
  panels <- rbind(matrix(seq_len(2 * n), nrow = n, byrow = TRUE),
                  rep(2 * n + 1L, 2))
  graphics::layout(panels, heights = c(rep(1, n), 0.85))
  for (i in seq_along(elems)) {
    g <- sens[sens$element == elems[i], ]
    heat(g, "R2",   sprintf("(%s) %s: mean R2", letters[2 * i - 1], elems[i]),
         "Greens", rev = TRUE)
    heat(g, "RMSE", sprintf("(%s) %s: mean RMSE", letters[2 * i], elems[i]),
         "Reds", rev = FALSE, digits = 2)
  }
  # relative change against the baseline, every element on one axis
  graphics::par(mar = c(3.6, 4.0, 2.0, 1.0))
  ncell <- nrow(sens[sens$element == elems[1], ])
  plot(NA, xlim = c(1, ncell), ylim = range(c(sens$dR2_pct, sens$dRMSE_pct, -6, 6)),
       xlab = "", ylab = "", cex.axis = 0.72, xaxt = "n",
       main = sprintf("(%s) change from the 20 km / SD 0.5 baseline",
                      letters[2 * length(elems) + 1L]), cex.main = 0.92)
  graphics::abline(h = c(-5, 5), lty = 2, col = "grey45")
  graphics::abline(h = 0, col = "grey25")
  pch_of <- c(Zn = 16, Co = 17)
  for (elem in elems) {
    g <- sens[sens$element == elem, ]
    graphics::points(seq_len(nrow(g)), g$dR2_pct, pch = pch_of[elem],
                     col = "#1a6b4a", cex = 0.75)
    graphics::points(seq_len(nrow(g)), g$dRMSE_pct, pch = pch_of[elem],
                     col = "#b03a2e", cex = 0.75)
  }
  graphics::mtext("parameter combination", side = 1, line = 1.4, cex = 0.65)
  graphics::mtext("change from baseline (%)", side = 2, line = 2.5, cex = 0.65)
  graphics::legend("topleft", bty = "n", cex = 0.66, horiz = TRUE,
                   pch = c(pch_of[elems], 16, 16),
                   col = c(rep("grey35", length(elems)), "#1a6b4a", "#b03a2e"),
                   legend = c(elems, "R2", "RMSE"))
}, "fig06-sensitivity", width = FIG_WIDTH_DOUBLE,
   height = 9 * length(elems) + 7)
