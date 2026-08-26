# =============================================================================
# p05-seed-stability.R — the error bar the model comparison needs
#
# Left: the distribution of cross-validated R2 over SEED_REPEATS forest seeds
# for the three forest-based models, with the published Table 2 values marked.
# Right: the paired SRK-minus-RFK difference, seed by seed, against the gap the
# paper reports. Nothing in the paper corresponds to this figure.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("p05  Seed stability")

seeds <- read_result(F_SEEDS)
prs   <- read_result(F_SEEDS_PAIR)
elems <- names(ELEMENTS)
MODELS <- c("RF", "RFK", "SRK")
# Published Table 2 R2 for the three forest-based rows.
PAPER <- list(Zn = c(RF = 0.330, RFK = 0.340, SRK = 0.343),
              Co = c(RF = 0.342, RFK = 0.345, SRK = 0.353))
PAPER_GAP <- c(Zn = 0.003, Co = 0.008)

save_figure(function() {
  graphics::layout(matrix(seq_len(2 * length(elems)), nrow = length(elems),
                          byrow = TRUE))
  for (i in seq_along(elems)) {
    elem <- elems[i]
    g <- seeds[seeds$element == elem, ]
    graphics::par(mar = c(3.2, 4.2, 2.0, 1.0))
    bx <- lapply(MODELS, function(m) g$R2[g$model == m])
    graphics::boxplot(bx, names = MODELS, cex.axis = 0.75, border = "grey35",
                      col = c("#e8edf2", "#e8edf2", "#e8dfb8"),
                      main = sprintf("(%s) %s: R2 over %d forest seeds",
                                     letters[2 * i - 1], elem, SEED_REPEATS),
                      cex.main = 0.92)
    graphics::points(seq_along(MODELS), PAPER[[elem]][MODELS], pch = 4, cex = 1.1,
                     col = "#b03a2e", lwd = 2)
    graphics::mtext("mean R2 across the five blocks", side = 2, line = 2.6, cex = 0.65)
    graphics::legend("topleft", bty = "n", cex = 0.68, pch = 4, col = "#b03a2e",
                     pt.lwd = 2, legend = "published Table 2")

    p <- prs[prs$element == elem, ]
    graphics::par(mar = c(3.4, 4.2, 2.0, 1.0))
    plot(p$seed, p$dR2_SRK_RFK, type = "h", lwd = 3, col = "#7d6608",
         xlab = "", ylab = "", cex.axis = 0.75,
         ylim = range(c(p$dR2_SRK_RFK, 0, PAPER_GAP[elem])) * 1.15,
         main = sprintf("(%s) %s: SRK minus RFK, seed by seed",
                        letters[2 * i], elem), cex.main = 0.92)
    graphics::abline(h = 0, col = "grey35")
    graphics::abline(h = PAPER_GAP[elem], lty = 2, col = "#b03a2e", lwd = 1.6)
    graphics::abline(h = mean(p$dR2_SRK_RFK), lty = 3, col = "#7d6608", lwd = 1.6)
    graphics::mtext("forest seed", side = 1, line = 2.2, cex = 0.65)
    graphics::mtext("difference in R2", side = 2, line = 2.6, cex = 0.65)
    graphics::legend(if (mean(p$dR2_SRK_RFK) > 0) "bottomright" else "bottomleft",
                     bty = "n", cex = 0.66, lty = c(2, 3, 0), lwd = 1.6,
                     col = c("#b03a2e", "#7d6608", NA),
                     legend = c(sprintf("published gap  %+.3f", PAPER_GAP[elem]),
                                sprintf("mean here      %+.4f", mean(p$dR2_SRK_RFK)),
                                sprintf("SRK ahead in %d of %d seeds",
                                        sum(p$dR2_SRK_RFK > 0), nrow(p))))
  }
}, "fig05-seed-stability", width = FIG_WIDTH_DOUBLE,
   height = 7 * length(elems))
