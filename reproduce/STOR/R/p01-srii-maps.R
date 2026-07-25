# =============================================================================
# p01-srii-maps.R — block-level Gamma_A, Gamma_B and SRII maps
# The paper's Fig. 4 / Fig. 5 equivalent on the simulated study area.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_SRII)) {
  log_warn("ggplot2 or SRII results missing; skipping fig01")
} else {
library(ggplot2)
s <- read_result(F_SRII)
long <- rbind(
  data.frame(s[, c("x_km", "y_km")], value = s$gamma_a,
             panel = "Quantity~dimension~(Gamma[A])"),
  data.frame(s[, c("x_km", "y_km")], value = s$gamma_b,
             panel = "Quality~dimension~(Gamma[B])"),
  data.frame(s[, c("x_km", "y_km")], value = s$gamma,
             panel = "SRII~(Gamma)"))
long$panel <- factor(long$panel, unique(long$panel))

g <- ggplot(long, aes(x_km, y_km, fill = value)) +
  geom_tile() +
  facet_wrap(~panel, nrow = 1, labeller = label_parsed) +
  scale_fill_gradientn(colours = PALETTE_SEQ, limits = c(0, 1),
                       name = "Index\nvalue") +
  coord_equal(expand = FALSE) +
  labs(x = "x (km)", y = "y (km)") +
  theme_bw(base_size = 8) +
  theme(legend.position = "right", panel.grid = element_blank())

save_figure(g, "fig01-srii-maps", width = FIG_WIDTH_DOUBLE, height = 7.2)
}
