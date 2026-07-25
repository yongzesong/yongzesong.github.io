# =============================================================================
# p03-dmu-stages.R — what each DMU stage looks like (the paper's Fig. 7)
# (a) index distributions by stage   (b) the stage map
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_STAGES)) {
  log_warn("ggplot2 or stage results missing; skipping fig03")
} else {
library(ggplot2)
s <- read_result(F_STAGES)
s$stage <- factor(s$stage, c("IR", "MR", "NR"))

long <- rbind(
  data.frame(stage = s$stage, value = s$gamma_a, index = "Gamma[A]"),
  data.frame(stage = s$stage, value = s$gamma_b, index = "Gamma[B]"),
  data.frame(stage = s$stage, value = s$gamma,   index = "Gamma"))
long$index <- factor(long$index, c("Gamma[A]", "Gamma[B]", "Gamma"))

pa <- ggplot(long, aes(stage, value, fill = stage)) +
  geom_boxplot(linewidth = 0.25, outlier.size = 0.4) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 1.6,
               fill = "white", colour = "#8B1A1A") +
  facet_wrap(~index, labeller = label_parsed) +
  scale_fill_manual(values = PALETTE_DMU, guide = "none") +
  labs(x = "DMU stage", y = "Index value",
       title = "(a) Index values by DMU stage") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"))

pb <- ggplot(s, aes(x_km, y_km, fill = stage)) +
  geom_tile() +
  scale_fill_manual(values = PALETTE_DMU, name = "DMU") +
  coord_equal(expand = FALSE) +
  labs(x = "x (km)", y = "y (km)", title = "(b) DMU stage map") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        panel.grid = element_blank())

g <- if (has_pkg("patchwork"))
  patchwork::wrap_plots(pa, pb, nrow = 1, widths = c(1.5, 1)) else pb
save_figure(g, "fig03-dmu-stages", width = FIG_WIDTH_DOUBLE, height = 7.5)
}
