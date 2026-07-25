# =============================================================================
# p05-sesu.R — spatial-scale effects figure (paper Fig 9): Q value against
# spatial unit size, one line per variable, to read where Q stabilises.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_SESU)) {
  log_warn("ggplot2 or SESU results missing; skipping fig05")
} else {
library(ggplot2)
s <- read_result(F_SESU)
s$variable <- factor(s$variable, levels = c(CATEGORICAL, CONTINUOUS))

p <- ggplot(s, aes(unit_size, qv, colour = variable, group = variable)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.4) +
  labs(x = "Spatial unit size (km)", y = "Q value", colour = NULL) +
  theme_bw(base_size = 8) +
  theme(legend.position = "right")

save_figure(p, "fig05-sesu", width = FIG_WIDTH_SINGLE + 4, height = 8)
}
