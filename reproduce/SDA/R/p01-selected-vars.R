# =============================================================================
# p01-selected-vars.R — where in (b, tau) space does the signal live?
# The paper's Fig. 5 equivalent: each selected variable placed at its searching
# range and quantile, sized by |correlation| with the response.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_SELPARAM)) {
  log_warn("ggplot2 or selection results missing; skipping fig01")
} else {
library(ggplot2)
p <- read_result(F_SELPARAM)
p$sign <- ifelse(p$correlation >= 0, "positive", "negative")

g <- ggplot(p, aes(buffer_km, quantile, size = abs(correlation), colour = sign)) +
  geom_point(alpha = 0.85)
# Several selected variables can share a buffer, so labels are repelled apart.
g <- g + if (has_pkg("ggrepel")) {
  ggrepel::geom_text_repel(aes(label = surface), size = 2.2, seed = 1,
                           box.padding = 0.5, point.padding = 0.4,
                           min.segment.length = 0.2, segment.size = 0.2,
                           show.legend = FALSE)
} else {
  geom_text(aes(label = surface), size = 2, vjust = -1.8, show.legend = FALSE)
}
g <- g +
  scale_size_area(max_size = 9, name = "|r|") +
  scale_colour_manual(values = c(positive = "#B2182B", negative = "#2166AC"),
                      name = NULL) +
  scale_x_continuous(breaks = DIST_BUFFERS) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(-0.08, 1.12)) +
  labs(x = expression("Searching range "*italic(b)*" (km)"),
       y = expression("Quantile "*tau)) +
  theme_bw(base_size = 8) +
  theme(legend.position = "right")

save_figure(g, "fig01-selected-variables", width = FIG_WIDTH_SINGLE + 5, height = 8)
}
