# =============================================================================
# p03-interaction.R — interaction-detector bubble matrix (paper Fig 8a)
# Bubble size = joint Q(x1 n x2); colour = interaction type.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_INTERACT)) {
  log_warn("ggplot2 or interaction results missing; skipping fig03")
} else {
library(ggplot2)
it <- read_result(F_INTERACT)
lev <- unique(c(it$var1, it$var2))
it$var1 <- factor(it$var1, levels = lev)
it$var2 <- factor(it$var2, levels = rev(lev))

pal <- c("Enhance, nonlinear" = "#B2182B", "Enhance, bi-" = "#E6A23C",
         "Independent" = "#999999", "Weaken, uni-" = "#4E79A7",
         "Weaken, nonlinear" = "#59A14F")

p <- ggplot(it, aes(var1, var2, size = qv12, colour = interaction)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f", qv12)), size = 2, colour = "black",
            vjust = -1.5) +
  scale_size_area(max_size = 12, name = "Joint Q") +
  scale_colour_manual(values = pal, name = "Interaction") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "right")

save_figure(p, "fig03-interaction", width = FIG_WIDTH_DOUBLE, height = 11)
}
