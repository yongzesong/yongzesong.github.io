# =============================================================================
# 40-gos-prediction.R — predict log Zn on the 1 km grid (Eqs. 11-12)
#
# Two predictions are made over the same 13,132 cells:
#   GOS, kappa = lambda — only the observations whose similarity exceeds the
#                         optimal threshold S_lambda contribute (Eq. 11)
#   BCS, kappa = 1      — every observation contributes (Eq. 6)
# and the uncertainty of the GOS prediction is read at all six probability
# levels zeta of Eq. 12, Theta = 1 - Q(S_lambda, zeta).
#
# Outputs: results/grid-prediction.csv, results/grid-prediction-bcs.csv
#          figs/fig03-gos-prediction-uncertainty.{png,pdf}   (paper Fig. 9/11)
#          figs/fig04-uncertainty-zeta.{png,pdf}             (vignette Fig. 3)
#          figs/fig05-bcs-vs-gos.{png,pdf}                   (paper Fig. 10)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geosimilarity", "the GOS method")

log_head("Step 4/5  GOS prediction on the 1 km grid")

dat      <- readRDS(D_SAMPLES)
d        <- dat$samples
grid     <- dat$grid
selected <- readRDS(D_SELECTED)$selected
lambda   <- readRDS(D_LAMBDA)$lambda
f <- stats::as.formula(paste(RESPONSE_LOG, "~", paste(selected, collapse = " + ")))

log_info("%d observations -> %d grid cells, kappa = %.2f", nrow(d), nrow(grid), lambda)

UNC <- c("uncertainty90", "uncertainty95", "uncertainty99",
         "uncertainty99.5", "uncertainty99.9", "uncertainty100")
ZETA <- c(0.9, 0.95, 0.99, 0.995, 0.999, 1)

# -- GOS ----------------------------------------------------------------------
g <- timeit("gos (kappa = lambda)",
            as.data.frame(geosimilarity::gos(f, data = d, newdata = grid,
                                             kappa = lambda, cores = CORES)))
gos_out <- data.frame(GridID = grid$GridID, Lon = grid$Lon, Lat = grid$Lat,
                      pred = round(g$pred, 6),
                      pred_zn_ppm = round(exp(g$pred), 4),
                      round(g[, UNC], 8), check.names = FALSE)
write_result(gos_out, F_GRID)

# -- BCS, the kappa = 1 special case ------------------------------------------
b <- timeit("gos (kappa = 1, BCS)",
            as.data.frame(geosimilarity::gos(f, data = d, newdata = grid,
                                             kappa = 1, cores = CORES)))
bcs_out <- data.frame(GridID = grid$GridID, Lon = grid$Lon, Lat = grid$Lat,
                      pred = round(b$pred, 6),
                      pred_zn_ppm = round(exp(b$pred), 4),
                      round(b[, UNC], 8), check.names = FALSE)
write_result(bcs_out, F_GRIDBCS)

obs_mean <- mean(d[[RESPONSE_LOG]])
log_info("GOS predictions: mean %.3f, sd %.3f, range %.3f-%.3f (log Zn)",
         mean(g$pred), stats::sd(g$pred), min(g$pred), max(g$pred))
log_info("BCS predictions: mean %.3f, sd %.3f, range %.3f-%.3f (log Zn)",
         mean(b$pred), stats::sd(b$pred), min(b$pred), max(b$pred))
log_info("BCS spread is %.0f%% of the GOS spread — the clustering the paper reports",
         100 * stats::sd(b$pred) / stats::sd(g$pred))
log_info("observed mean of log Zn is %.3f; BCS predictions sit within +/-0.1 of it for %.1f%% of cells (GOS %.1f%%)",
         obs_mean, 100 * mean(abs(b$pred - obs_mean) < 0.1),
         100 * mean(abs(g$pred - obs_mean) < 0.1))

# Eq. 12 makes uncertainty fall as zeta rises, because Q() is a quantile of the
# retained similarities. Report the means so the tutorial can quote them.
unc_mean <- vapply(UNC, function(u) mean(g[[u]]), numeric(1))
for (i in seq_along(UNC))
  log_info("zeta = %-5s mean uncertainty %.4f (GOS) vs %.4f (BCS)",
           ZETA[i], unc_mean[i], mean(b[[UNC[i]]]))
log_info("at zeta = 1 the two models agree to %.2e, as the paper notes",
         max(abs(g$uncertainty100 - b$uncertainty100)))


# =============================================================================
# Figures
# =============================================================================
# A colour scale that ignores the extreme tail; uncertainty is heavily
# right-skewed (a few isolated cells reach 1) and a raw range would paint the
# whole map one shade. Values above the limit are clipped, not dropped.
robust_lim <- function(v, p = 0.99) c(0, unname(stats::quantile(v, p, na.rm = TRUE)))

pred_lim <- range(c(g$pred, b$pred))          # shared by the GOS and BCS maps
u99_lim  <- robust_lim(g$uncertainty99)

# -- Fig. 3: the prediction and its uncertainty --------------------------------
draw_fig03 <- function() {
  layout(matrix(1:4, nrow = 2, byrow = TRUE), heights = c(5, 1.15))
  par(mar = c(3.4, 3.6, 2.2, 0.8), mgp = c(2, 0.5, 0))

  tile_map(grid$Lon, grid$Lat, g$pred, PAL_PRED, zlim = pred_lim,
           main = "GOS prediction", xlab = LON_LAB, ylab = LAT_LAB)
  panel_tag("(a)")
  # R's == is non-associative, so "kappa == lambda == value" cannot be parsed;
  # the quoted "=" keeps the three-way equality readable in plotmath.
  mtext(bquote(kappa == lambda ~ "=" ~ .(sprintf("%.2f", lambda))),
        side = 3, line = -1.2, adj = 0.97, cex = 0.7)

  tile_map(grid$Lon, grid$Lat, g$uncertainty99, PAL_UNCER, zlim = u99_lim,
           main = "GOS prediction uncertainty", xlab = LON_LAB, ylab = LAT_LAB)
  panel_tag("(b)")
  mtext(bquote(zeta == 0.99), side = 3, line = -1.2, adj = 0.97, cex = 0.7)

  color_bar(pred_lim, PAL_PRED, "predicted log Zn", digits = 2)
  color_bar(u99_lim, PAL_UNCER, expression(Theta ~ "(uncertainty)"),
            clipped = TRUE, digits = 2)
}
draw_figure("fig03-gos-prediction-uncertainty", draw_fig03,
            width = FIG_W_DOUBLE, height = 12)

# -- Fig. 4: uncertainty at every probability level zeta -----------------------
# One shared scale so the panels can be compared: uncertainty shrinks
# monotonically as zeta rises, because a higher quantile of the retained
# similarities is a larger number and Theta = 1 - Q.
draw_fig04 <- function() {
  zl <- robust_lim(unlist(g[, UNC]))
  layout(rbind(matrix(1:6, nrow = 2, byrow = TRUE), c(7, 7, 7)),
         heights = c(5, 5, 1.5))
  par(mar = c(1.9, 2.6, 2.0, 0.6), mgp = c(2, 0.4, 0))
  for (i in seq_along(UNC)) {
    tile_map(grid$Lon, grid$Lat, g[[UNC[i]]], PAL_UNCER, zlim = zl,
             main = NULL, cex.main = 0.85)
    mtext(bquote(zeta == .(ZETA[i])), side = 3, line = 0.45, cex = 0.8, font = 2)
    mtext(sprintf("mean %.3f", mean(g[[UNC[i]]])), side = 3, line = -1.1,
          adj = 0.96, cex = 0.62)
  }
  par(mar = c(2.6, 12, 0.8, 12))
  color_bar(zl, PAL_UNCER, expression(Theta ~ "= 1 - Q(" * S[lambda] * ", " * zeta * ")"),
            clipped = TRUE, digits = 2)
}
draw_figure("fig04-uncertainty-zeta", draw_fig04,
            width = FIG_W_DOUBLE, height = 15)

# -- Fig. 5: why the optimal threshold matters ---------------------------------
# The paper's Fig. 10 finding: because BCS averages over every observation, its
# predictions collapse towards the study-area mean, while GOS keeps the local
# contrast. The map and the density are two views of the same fact.
draw_fig05 <- function() {
  # Column 1 holds the map above its colour bar; column 2 is the density,
  # spanning both rows. Column-major fill, hence c(1, 2, 3, 3).
  layout(matrix(c(1, 2, 3, 3), nrow = 2), widths = c(1, 1.1), heights = c(5, 1.15))
  par(mar = c(3.4, 3.6, 2.2, 0.8), mgp = c(2, 0.5, 0))

  tile_map(grid$Lon, grid$Lat, b$pred, PAL_PRED, zlim = pred_lim,
           main = "BCS prediction", xlab = LON_LAB, ylab = LAT_LAB)
  panel_tag("(a)")
  mtext(bquote(kappa == 1), side = 3, line = -1.2, adj = 0.97, cex = 0.7)
  color_bar(pred_lim, PAL_PRED, "predicted log Zn", digits = 2)

  par(pty = "m", mar = c(3.8, 3.8, 2.2, 1.0), mgp = c(2.3, 0.6, 0))
  dg <- stats::density(g$pred); db <- stats::density(b$pred)
  plot(dg, main = "", xlab = "", ylab = "", axes = FALSE, type = "n",
       xlim = pred_lim, ylim = c(0, max(dg$y, db$y) * 1.05))
  axis(1, cex.axis = 0.8); axis(2, las = 1, cex.axis = 0.8)
  polygon(c(db$x, rev(db$x)), c(db$y, rep(0, length(db$y))),
          col = adjustcolor(PAL_MODEL[["BCS"]], 0.25), border = NA)
  lines(db, col = PAL_MODEL[["BCS"]], lwd = 1.8)
  lines(dg, col = PAL_MODEL[["GOS"]], lwd = 1.8)
  abline(v = obs_mean, col = "grey30", lty = 2, lwd = 1.1)
  box()
  title(xlab = "predicted log Zn", ylab = "Density", cex.lab = 0.9)
  mtext("Distribution of the 13,132 predictions", side = 3, line = 0.4,
        font = 2, cex = 0.85)
  panel_tag("(b)")
  legend("topleft", bty = "n", cex = 0.78, lwd = c(1.8, 1.8, 1.1),
         lty = c(1, 1, 2), col = c(PAL_MODEL[["GOS"]], PAL_MODEL[["BCS"]], "grey30"),
         legend = c(sprintf("GOS (sd %.3f)", stats::sd(g$pred)),
                    sprintf("BCS (sd %.3f)", stats::sd(b$pred)),
                    sprintf("observed mean %.2f", obs_mean)))
}
draw_figure("fig05-bcs-vs-gos", draw_fig05, width = FIG_W_DOUBLE, height = 12)
