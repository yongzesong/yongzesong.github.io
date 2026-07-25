# =============================================================================
# p01-optimization.R — the optimal-discretisation figure (paper Fig 5a)
# Q value against number of intervals, one line per method, faceted by variable.
# The chosen point (max Q) is marked, mirroring the OPGD paper's optimisation panel.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2")) { log_warn("ggplot2 not installed; skipping figures"); } else {
library(ggplot2)

cv  <- read_result(F_DISC_CURVE)
opt <- read_result(F_DISC)
cv$variable <- factor(cv$variable, levels = CONTINUOUS)
opt$variable <- factor(opt$variable, levels = CONTINUOUS)

p <- ggplot(cv, aes(n_intervals, qv, colour = method, group = method)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.1) +
  geom_point(data = merge(opt, cv,
               by.x = c("variable", "method", "n_intervals"),
               by.y = c("variable", "method", "n_intervals")),
             aes(n_intervals, qv.y), inherit.aes = FALSE,
             shape = 21, size = 3, stroke = 1, colour = "black", fill = NA) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_colour_manual(values = PALETTE_DISC, name = "Method") +
  labs(x = "Number of intervals", y = "Q value") +
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom")

save_figure(p, "fig01-optimization", width = FIG_WIDTH_DOUBLE, height = 12)
}
