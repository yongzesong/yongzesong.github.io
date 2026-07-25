# =============================================================================
# p02-factor.R — factor-detector bar chart: relative importance (Q) per variable
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_FACTOR)) {
  log_warn("ggplot2 or factor results missing; skipping fig02")
} else {
library(ggplot2)
fac <- read_result(F_FACTOR)
fac$variable <- factor(fac$variable, levels = fac$variable[order(fac$qv)])

p <- ggplot(fac, aes(qv, variable, fill = qv)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.3f", qv)), hjust = -0.15, size = 2.6) +
  scale_fill_gradientn(colours = PALETTE_SEQ, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Q value (relative importance)", y = NULL) +
  theme_bw(base_size = 8)

save_figure(p, "fig02-factor", width = FIG_WIDTH_SINGLE + 3, height = 7)
}
