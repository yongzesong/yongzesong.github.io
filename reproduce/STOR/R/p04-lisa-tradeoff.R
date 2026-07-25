# =============================================================================
# p04-lisa-tradeoff.R — clusters and trade-off strategies (the paper's Fig. 8)
# (a) LISA classes of Gamma_A and Gamma_B  (b) joint clusters and strategies
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_LISA)) {
  log_warn("ggplot2 or LISA results missing; skipping fig04")
} else {
library(ggplot2)
l <- read_result(F_LISA)

pal_class <- c(HH = "#B2182B", LL = "#2166AC", HL = "#F4A582",
               LH = "#92C5DE", `Not significant` = "#E8E8E8")

long <- rbind(
  data.frame(l[, c("x_km", "y_km")], class = l$class_a,
             panel = "LISA~of~Gamma[A]"),
  data.frame(l[, c("x_km", "y_km")], class = l$class_b,
             panel = "LISA~of~Gamma[B]"))
long$class <- factor(long$class, names(pal_class))

pa <- ggplot(long, aes(x_km, y_km, fill = class)) +
  geom_tile() +
  facet_wrap(~panel, labeller = label_parsed) +
  scale_fill_manual(values = pal_class, name = "Cluster", drop = FALSE) +
  coord_equal(expand = FALSE) +
  labs(x = "x (km)", y = "y (km)",
       title = "(a) Local Moran clusters per dimension") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        panel.grid = element_blank())

pal_strat <- c(`Increase quantity` = "#56C1E8", `Increase both` = "#4CAF6E",
               `Increase quality` = "#F2A93B", `No cold spot` = "#F2F2F2")
l$strategy <- factor(l$strategy, names(pal_strat))

pb <- ggplot(l, aes(x_km, y_km, fill = strategy)) +
  geom_tile() +
  scale_fill_manual(values = pal_strat, name = "Strategy", drop = FALSE) +
  coord_equal(expand = FALSE) +
  labs(x = "x (km)", y = "y (km)",
       title = "(b) Development strategies for cold-side blocks") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        panel.grid = element_blank())

g <- if (has_pkg("patchwork"))
  patchwork::wrap_plots(pa, pb, nrow = 1, widths = c(2, 1.15)) else pa
save_figure(g, "fig04-lisa-tradeoff", width = FIG_WIDTH_DOUBLE, height = 7.2)
}
