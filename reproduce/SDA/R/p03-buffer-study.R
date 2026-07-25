# =============================================================================
# p03-buffer-study.R — performance against the searching range b, and the
# generation cost against sample size.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping fig04/fig05")
} else {
library(ggplot2)

if (file.exists(F_BUFFER)) {
  b <- read_result(F_BUFFER)
  g <- ggplot(b, aes(buffer_km, R2)) +
    geom_line(colour = "#B2182B", linewidth = 0.5) +
    geom_point(aes(size = n_selected), colour = "#B2182B") +
    geom_text(aes(label = sprintf("%.3f", R2)), vjust = -1.1, size = 2.4) +
    scale_size_area(max_size = 5, name = "variables\nselected") +
    scale_x_continuous(breaks = DIST_BUFFERS) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.16))) +
    labs(x = expression("Searching range "*italic(b)*" (km)"),
         y = expression(R^2*" on held-out samples")) +
    theme_bw(base_size = 8)
  save_figure(g, "fig04-buffer-sensitivity", width = FIG_WIDTH_SINGLE + 2, height = 7)
}

if (file.exists(F_TIMING)) {
  tm <- read_result(F_TIMING)
  g2 <- ggplot(tm, aes(n_points, seconds)) +
    geom_line(colour = "#2166AC", linewidth = 0.5) +
    geom_point(colour = "#2166AC", size = 1.6) +
    labs(x = "Number of sample points",
         y = sprintf("Seconds (grid of %s cells)",
                     format(tm$n_grid_cells[1], big.mark = ","))) +
    theme_bw(base_size = 8)
  save_figure(g2, "fig05-generation-cost", width = FIG_WIDTH_SINGLE, height = 6.5)
}
}
