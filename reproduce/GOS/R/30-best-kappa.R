# =============================================================================
# 30-best-kappa.R — find the optimal similarity threshold lambda (Eqs. 7-9)
#
# kappa is the share of observations kept at each prediction location, i.e.
# kappa = 1 - tau where tau is the quantile probability of the similarity
# vector (Eq. 7). For every candidate kappa the data are split 50/50, the test
# values are predicted from the training half, and the cross-validation RMSE
# is recorded (Eq. 8); lambda is the kappa with the smallest mean RMSE
# (Eq. 9). kappa = 1 keeps every observation and is exactly BCS.
#
# gos_bestkappa() seeds each repeat internally with 1..nrepeat, so the same
# 50/50 splits are used for every kappa and the whole step is deterministic
# regardless of the outer seed.
#
# Outputs: data/derived/best-kappa.rds
#          results/kappa-rmse.csv
#          figs/fig02-kappa-rmse.{png,pdf}   (paper Fig. 7 shape)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geosimilarity", "the GOS method")

log_head("Step 3/5  Optimal similarity threshold")

d        <- readRDS(D_SAMPLES)$samples
selected <- readRDS(D_SELECTED)$selected
f <- stats::as.formula(paste(RESPONSE_LOG, "~", paste(selected, collapse = " + ")))
log_info("formula: %s", deparse(f))
log_info("kappa grid: %d values from %.2f to %.2f, %d repeats of a %.0f/%.0f split",
         length(KAPPA_GRID), min(KAPPA_GRID), max(KAPPA_GRID),
         BESTKAPPA_NREPEAT, 100 * BESTKAPPA_NSPLIT, 100 * (1 - BESTKAPPA_NSPLIT))

set.seed(SEED)
bk <- timeit("gos_bestkappa", geosimilarity::gos_bestkappa(
  f, data = d, kappa = KAPPA_GRID, nrepeat = BESTKAPPA_NREPEAT,
  nsplit = BESTKAPPA_NSPLIT, cores = CORES))

lambda <- bk$bestkappa
per_rep <- as.data.frame(bk$cvrmse)          # one row per (kappa, repeat)
agg <- aggregate(rmse ~ kappa, per_rep, function(v) c(m = mean(v), s = stats::sd(v)))

kap <- data.frame(
  kappa    = agg$kappa,
  rmse     = round(agg$rmse[, "m"], 6),
  rmse_sd  = round(agg$rmse[, "s"], 6),
  nrepeat  = BESTKAPPA_NREPEAT,
  selected = agg$kappa == lambda)
kap <- kap[order(kap$kappa), ]
write_result(kap, F_KAPPA)

rmse_lambda <- kap$rmse[kap$selected]
rmse_bcs    <- kap$rmse[kap$kappa == 1]
red_bcs     <- 100 * (rmse_bcs - rmse_lambda) / rmse_bcs
log_info("lambda = %.2f  (cross-validation RMSE %.4f)", lambda, rmse_lambda)
log_info("RMSE at kappa = 1 (BCS) is %.4f — GOS lowers it by %.2f%%", rmse_bcs, red_bcs)
log_info("only %.0f%% of the %d observations are used at each prediction location",
         100 * lambda, nrow(d))

saveRDS(list(lambda = lambda, rmse = rmse_lambda, rmse_bcs = rmse_bcs,
             reduction_vs_bcs = red_bcs, curve = kap, formula = f), D_LAMBDA)
log_info("wrote data/derived/best-kappa.rds")


# -- Fig. 2: the curve that lambda comes from ----------------------------------
# Panel (a) is the paper's Fig. 7: RMSE falls steeply as the first few percent
# of observations are added, reaches a minimum, then drifts up again as
# dissimilar observations start contributing noise. The minimum sits below
# kappa = 0.1, so panel (b) zooms into that range where the grid is fine.
draw_fig02 <- function() {
  par(mfrow = c(1, 2), mar = c(3.6, 4.0, 2.0, 0.9), mgp = c(2.3, 0.6, 0),
      cex.axis = 0.8, cex.lab = 0.9, tcl = -0.3)

  curve_panel <- function(sub, main, tag, xlim) {
    plot(sub$kappa, sub$rmse, type = "n", axes = FALSE, xlim = xlim,
         ylim = range(sub$rmse) + c(-1, 1) * 0.06 * diff(range(sub$rmse)),
         xlab = "", ylab = "")
    axis(1); axis(2, las = 1)
    grid(col = "grey90", lty = 1, lwd = 0.6)
    lines(sub$kappa, sub$rmse, col = ACCENT, lwd = 1.6)
    points(sub$kappa, sub$rmse, pch = 16, cex = 0.6, col = ACCENT)
    abline(v = lambda, col = "grey35", lty = 2, lwd = 1.1)
    points(lambda, rmse_lambda, pch = 21, cex = 1.4, bg = "white",
           col = ACCENT, lwd = 1.8)
    box()
    title(xlab = expression(kappa ~ "(share of observations retained)"))
    title(ylab = "Cross-validation RMSE of log Zn")
    mtext(main, side = 3, line = 0.4, font = 2, cex = 0.85)
    panel_tag(tag)
    legend("topright", bty = "n", cex = 0.8,
           legend = as.expression(bquote(lambda == .(sprintf("%.2f", lambda)) *
                                         "," ~ RMSE == .(sprintf("%.4f", rmse_lambda)))))
  }

  curve_panel(kap, "All candidate thresholds", "(a)", c(0, 1))
  curve_panel(kap[kap$kappa <= 0.1, ], expression(bold("Zoom:") ~ kappa <= 0.1),
              "(b)", c(0, 0.105))
}
draw_figure("fig02-kappa-rmse", draw_fig02, width = FIG_W_DOUBLE, height = 9)
