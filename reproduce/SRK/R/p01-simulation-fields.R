# =============================================================================
# p01-simulation-fields.R — the three simulated scenarios (paper Fig. 1)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("p01  Simulated fields")

f  <- read_result(F_SIM_FIELDS)
sc <- c("normal", "skewed", "long-tail")
lab <- c("(a) normal", "(b) skewed", "(c) long-tail")

save_figure(function() {
  graphics::layout(matrix(1:6, nrow = 2, byrow = TRUE), heights = c(1.25, 1))
  for (i in seq_along(sc)) {
    d <- f[f$scenario == sc[i], ]
    plot_grid_field(d$x, d$y, d$y_obs, main = lab[i])
  }
  for (i in seq_along(sc)) {
    d <- f[f$scenario == sc[i], ]
    graphics::par(mar = c(3.6, 3.8, 1.6, 1.2))
    h <- graphics::hist(d$y_obs, breaks = 24, col = "#c9d6e3", border = "white",
                        main = "", xlab = "", ylab = "", cex.axis = 0.7)
    graphics::mtext("response Z", side = 1, line = 2.2, cex = 0.7)
    graphics::mtext("count", side = 2, line = 2.4, cex = 0.7)
    graphics::abline(v = mean(d$y_obs), lty = 2, col = "#7d6608", lwd = 1.4)
    graphics::legend("topright", bty = "n", cex = 0.7,
                     legend = sprintf("skew = %.2f",
                       mean((d$y_obs - mean(d$y_obs))^3) / stats::sd(d$y_obs)^3))
  }
}, "fig01-simulated-fields", width = FIG_WIDTH_DOUBLE, height = 13)
