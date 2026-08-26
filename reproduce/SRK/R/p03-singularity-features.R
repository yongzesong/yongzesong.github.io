# =============================================================================
# p03-singularity-features.R — case-study singularity features (paper Fig. 6)
#
# Row per element: where sv(hm) is anomalous, what the forest leans on, whether
# the singularity augmentation tightens the residuals, and how sv(hm) tracks the
# observed concentration.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("p03  Case study: singularity features")

imp  <- read_result(F_IMPORTANCE)
res  <- read_result(file.path(RES_DIR, "residuals-rf-vs-srk.csv"))
elems <- names(ELEMENTS)
tag  <- matrix(sprintf("(%s)", letters[seq_len(4 * length(elems))]),
               nrow = length(elems), byrow = TRUE)

save_figure(function() {
  graphics::layout(matrix(seq_len(4 * length(elems)), nrow = length(elems),
                          byrow = TRUE))
  for (i in seq_along(elems)) {
    elem <- elems[i]
    d  <- read_result(F_SAMPLES(elem))
    sv <- read_result(F_SV(elem))
    yv <- ELEMENTS[[elem]]$yvar
    a  <- sv$sv_hm

    # (1) the selected singularity feature in space
    zl <- stats::quantile(a, c(0.02, 0.98))
    plot_points(d$x / 1000, d$y / 1000, pmin(pmax(a, zl[1]), zl[2]), zlim = zl,
                cex = 0.42, main = sprintf("%s %s: sv(hm)", tag[i, 1], elem))
    graphics::mtext("easting (km, EPSG:3857)", side = 1, line = 2.1, cex = 0.6)
    graphics::mtext("northing (km)", side = 2, line = 2.3, cex = 0.6)

    # (2) variable importance, singularity features highlighted
    g <- imp[imp$element == elem, ]
    g <- g[order(g$importance), ]
    graphics::par(mar = c(3.4, 5.8, 1.8, 1.0))
    graphics::barplot(g$importance, horiz = TRUE, las = 1, names.arg = g$feature,
                      cex.names = 0.62, border = NA, cex.axis = 0.65,
                      col = c(coordinate = "#d5dbe1", covariate = "#b8c4d0",
                              singularity = "#7d6608")[g$kind],
                      main = sprintf("%s RF importance", tag[i, 2]), cex.main = 0.95)
    graphics::mtext("increase in node purity", side = 1, line = 2.2, cex = 0.65)
    graphics::legend("bottomright", bty = "n", cex = 0.6, fill = c("#7d6608", "#b8c4d0", "#d5dbe1"),
                     border = NA, legend = c("singularity", "covariate", "coordinate"))

    # (3) residual spread, plain RF against SRK
    r <- res[res$element == elem, ]
    graphics::par(mar = c(3.4, 3.6, 1.8, 1.0))
    d1 <- stats::density(r$RF); d2 <- stats::density(r$SRK)
    xl <- stats::quantile(c(r$RF, r$SRK), c(0.01, 0.99))
    plot(d1, main = sprintf("%s residual density", tag[i, 3]), cex.main = 0.95,
         xlab = "", ylab = "", col = "#8fa3b8", lwd = 2, cex.axis = 0.7,
         xlim = xl, ylim = c(0, max(d1$y, d2$y) * 1.05))
    graphics::lines(d2, col = "#7d6608", lwd = 2)
    graphics::abline(v = 0, lty = 3, col = "grey45")
    graphics::mtext(sprintf("residual (%s)", ELEMENTS[[elem]]$unit), side = 1,
                    line = 2.2, cex = 0.65)
    graphics::mtext("density", side = 2, line = 2.4, cex = 0.65)
    graphics::legend("topright", bty = "n", cex = 0.68, lwd = 2,
                     col = c("#8fa3b8", "#7d6608"),
                     legend = sprintf(c("RF   sd = %.2f", "SRK  sd = %.2f"),
                                      c(stats::sd(r$RF), stats::sd(r$SRK))))

    # (4) the feature against the observation
    graphics::par(mar = c(3.4, 3.6, 1.8, 1.0))
    plot(a, d[[yv]], pch = 16, cex = 0.4, col = "#7d660870", log = "y",
         xlab = "", ylab = "", cex.axis = 0.7,
         main = sprintf("%s sv(hm) vs %s", tag[i, 4], elem), cex.main = 0.95)
    graphics::mtext("sv(hm)", side = 1, line = 2.2, cex = 0.65)
    graphics::mtext(sprintf("%s (%s)", elem, ELEMENTS[[elem]]$unit), side = 2,
                    line = 2.4, cex = 0.65)
    graphics::abline(v = 2, lty = 3, col = "grey45")
    fit <- stats::lm(log(d[[yv]]) ~ a)
    ox <- sort(a)
    graphics::lines(ox, exp(stats::predict(fit, data.frame(a = ox))),
                    col = "#7d6608", lwd = 2)
    graphics::legend("topright", bty = "n", cex = 0.68,
                     legend = sprintf("r = %.2f", stats::cor(a, d[[yv]])))
  }
}, "fig03-singularity-features", width = FIG_WIDTH_DOUBLE,
   height = 7.5 * length(elems))
