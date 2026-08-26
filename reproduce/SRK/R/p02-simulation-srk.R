# =============================================================================
# p02-simulation-srk.R — the SRK mechanism on simulated data (paper Fig. 2)
#
# One row per scenario, four panels across: where the covariate is singular,
# how that singularity tracks the response, what the forest makes of it, and
# what it buys against ordinary kriging.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("p02  Simulation: the SRK mechanism")

f   <- read_result(F_SIM_FIELDS)
p   <- read_result(F_SIM_DETAIL)
imp <- read_result(F_SIM_IMP)
met <- read_result(F_SIM_RESULTS)
sc  <- c("normal", "skewed", "long-tail")
tag <- matrix(sprintf("(%s)", letters[1:12]), nrow = 3, byrow = TRUE)

save_figure(function() {
  graphics::layout(matrix(1:12, nrow = 3, byrow = TRUE))
  for (i in seq_along(sc)) {
    d  <- f[f$scenario == sc[i], ]
    pp <- p[p$scenario == sc[i], ]
    im <- imp[imp$scenario == sc[i], ]
    mm <- met[met$scenario == sc[i], ]

    # (1) singularity of the covariate
    plot_grid_field(d$x, d$y, d$sv_x_cov,
                    main = sprintf("%s %s: singularity a(X)", tag[i, 1], sc[i]),
                    pal = PAL_DIV)

    # (2) singularity against the response
    graphics::par(mar = c(3.4, 3.6, 1.8, 1.0))
    plot(d$sv_x_cov, d$y_obs, pch = 16, cex = 0.45, col = "#7d660890",
         xlab = "", ylab = "", cex.axis = 0.7,
         main = sprintf("%s a(X) vs response", tag[i, 2]), cex.main = 0.95)
    graphics::mtext("singularity index a", side = 1, line = 2.2, cex = 0.65)
    graphics::mtext("response Z", side = 2, line = 2.4, cex = 0.65)
    graphics::abline(v = 2, lty = 3, col = "grey45")
    ll <- stats::loess(y_obs ~ sv_x_cov, data = d, span = 0.9)
    ox <- sort(d$sv_x_cov)
    graphics::lines(ox, stats::predict(ll, data.frame(sv_x_cov = ox)),
                    col = "#7d6608", lwd = 2)
    graphics::legend("topright", bty = "n", cex = 0.7,
                     legend = sprintf("r = %.2f", stats::cor(d$sv_x_cov, d$y_obs)))

    # (3) what the forest leans on
    graphics::par(mar = c(3.4, 5.6, 1.8, 1.0))
    im <- im[order(im$importance), ]
    bp <- graphics::barplot(im$importance, horiz = TRUE, las = 1,
                            names.arg = im$feature, cex.names = 0.7,
                            col = ifelse(grepl("^sv_", im$feature),
                                         "#7d6608", "#b8c4d0"),
                            border = NA, xlab = "", cex.axis = 0.7,
                            main = sprintf("%s RF importance", tag[i, 3]),
                            cex.main = 0.95)
    graphics::mtext("increase in node purity", side = 1, line = 2.2, cex = 0.65)

    # (4) observed vs predicted, OK and SRK on one pair of axes
    graphics::par(mar = c(3.4, 3.6, 1.8, 1.0))
    lim <- range(c(pp$obs, pp$OK, pp$SRK))
    plot(pp$obs, pp$OK, pch = 1, cex = 0.5, col = "#8fa3b8", xlim = lim,
         ylim = lim, xlab = "", ylab = "", cex.axis = 0.7, asp = 1,
         main = sprintf("%s validation, OK vs SRK", tag[i, 4]), cex.main = 0.95)
    graphics::points(pp$obs, pp$SRK, pch = 16, cex = 0.5, col = "#7d6608")
    graphics::abline(0, 1, lty = 2, col = "grey35")
    graphics::mtext("observed", side = 1, line = 2.2, cex = 0.65)
    graphics::mtext("predicted", side = 2, line = 2.4, cex = 0.65)
    graphics::legend("topleft", bty = "n", cex = 0.68, pch = c(1, 16),
                     col = c("#8fa3b8", "#7d6608"),
                     legend = sprintf(c("OK  R2 = %.3f", "SRK R2 = %.3f"),
                                      c(mm$R2[mm$model == "OK"],
                                        mm$R2[mm$model == "SRK"])))
  }
}, "fig02-simulation-mechanism", width = FIG_WIDTH_DOUBLE, height = 21)
