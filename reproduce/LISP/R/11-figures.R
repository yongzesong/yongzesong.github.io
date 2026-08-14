# =============================================================================
# 11-figures.R — the eight core figures of an LPI application paper
# Reads results/step*.csv only; no recomputation. Figure plan and the
# argumentation task of each figure: figures/figure-plan.md
#
#   fig01 response index maps            fig05 interaction strength bars
#   fig02 X vs GC(X) trend curves        fig06 interaction type map + counts
#   fig03 local q histograms (X, GC)     fig07 local GOZH map + histogram
#   fig04 local q maps (top X + top GC)  fig08 variogram + local vs global q
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-plot-helpers.R")

library(ggplot2)
library(reshape2)
library(sf)
library(dplyr)
library(mgcv)
library(patchwork)

cfg <- load_config()
ensure_dir("figures")

x_vars   <- cfg$predictors$x_vars
gc_names <- gc_names_of(cfg)
all_vars <- all_vars_of(cfg)
labels   <- get_labels(cfg)
resp     <- cfg$response$response_name
w_full   <- cfg$figures$width_full
w_single <- cfg$figures$width_single

d_z    <- read.csv("results/step02-response.csv")
d_gc   <- read.csv("results/step03-geocomplexity.csv")
d_q    <- read.csv("results/step05-local-q-values.csv")
d_iall <- read.csv("results/step06-all-pairwise-interactions.csv")
d_ibst <- read.csv("results/step06-local-best-interaction.csv")
d_gozh <- read.csv("results/step07-local-gozh.csv")
d_vt   <- read.csv("results/step08-validation-table.csv")
d_vg   <- read.csv("results/step04-variogram-points.csv")
d_lr   <- read.csv("results/step04-local-range.csv")

xc <- cfg$data$x_col
yc <- cfg$data$y_col

# =============================================================================
# FIGURE 1: Spatial distribution of the response index (and sub-indices)
# =============================================================================

resp_cols <- intersect(
  c(grep(paste0("^", resp, "_"), names(d_z), value = TRUE), resp),
  names(d_z)
)
fig1_panels <- lapply(resp_cols, function(v) {
  make_value_map(d_z[[v]], d_z[[xc]], d_z[[yc]], cfg,
                 title = v, legend_title = "Index")
})
p_fig1 <- wrap_plots(fig1_panels, ncol = min(4, length(fig1_panels)))
ggsave("figures/fig01-response-maps.pdf", p_fig1,
       width = min(w_full, 3.5 * length(fig1_panels) + 1), height = 4.5,
       dpi = 300)
cat("Figure 1 saved.\n")

# =============================================================================
# FIGURE 2: X value vs GC(X), 5%-quantile binned means + GAM smoother
# =============================================================================

# Raw variable values live in the analysis table; GC values in step03.
d_raw  <- read.csv(cfg$data$analysis_table)
d_main <- merge(d_raw, d_gc[, c(cfg$data$id_col, gc_names)],
                by = cfg$data$id_col)
gc_summary <- do.call(rbind, lapply(x_vars, function(v) {
  df <- data.frame(x = d_main[[v]], y = d_main[[paste0("gc_", v)]])
  q_breaks <- quantile(df$x, probs = seq(0, 1, 0.05), na.rm = TRUE)
  df$group <- cut(df$x, breaks = unique(q_breaks),
                  labels = FALSE, include.lowest = TRUE)
  smry <- df %>%
    group_by(group) %>%
    summarise(mean_x = mean(x, na.rm = TRUE),
              mean_y = mean(y, na.rm = TRUE),
              se_y   = sd(y, na.rm = TRUE) / sqrt(sum(!is.na(y))),
              .groups = "drop")
  smry$variable <- labels[[v]]
  smry
}))
gc_summary$variable <- factor(gc_summary$variable, levels = labels[x_vars])

p_fig2 <- ggplot(gc_summary, aes(x = mean_x, y = mean_y)) +
  geom_errorbar(aes(ymin = mean_y - se_y, ymax = mean_y + se_y),
                width = 0, linewidth = 0.4, colour = "grey50") +
  geom_point(size = 1.2, colour = "steelblue") +
  geom_smooth(colour = "red", linewidth = 0.8, se = TRUE,
              fill = "pink", alpha = 0.3) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(x = "Variable value", y = "Geocomplexity (GC)") +
  facet_theme()
ggsave("figures/fig02-gc-trends.pdf", p_fig2, width = 8, height = 6, dpi = 300)
cat("Figure 2 saved.\n")

# =============================================================================
# FIGURE 3: Local q distributions — X variables (3a) and GC patterns (3b)
# =============================================================================

plot_q_histograms(d_q, x_vars,   labels, 3, "figures/fig03a-q-hist-x.pdf")
plot_q_histograms(d_q, gc_names, labels, 3, "figures/fig03b-q-hist-gc.pdf")
cat("Figure 3 saved.\n")

# =============================================================================
# FIGURE 4: Spatial maps of local q — top X and top GC variables
# =============================================================================

n_top    <- min(4, length(x_vars))
mean_q_x  <- sort(colMeans(d_q[, x_vars],   na.rm = TRUE), decreasing = TRUE)
mean_q_gc <- sort(colMeans(d_q[, gc_names], na.rm = TRUE), decreasing = TRUE)
top_map_vars <- c(names(mean_q_x)[1:n_top], names(mean_q_gc)[1:n_top])

fig4_panels <- lapply(top_map_vars, function(v) {
  make_value_map(d_q[[v]], d_q$Longitude, d_q$Latitude, cfg,
                 title = labels[[v]], legend_title = "Local q")
})
p_fig4 <- wrap_plots(fig4_panels, ncol = n_top)
ggsave("figures/fig04-local-q-maps.pdf", p_fig4,
       width = w_full, height = 7, dpi = 300)
cat("Figure 4 saved.\n")

# =============================================================================
# FIGURE 5: Interaction strength — top pairs by mean qv12, by pair category
# =============================================================================

pair_label_of <- function(v) {
  ifelse(v %in% names(labels), labels[v], v)
}

top_pairs <- d_iall %>%
  group_by(var1, var2) %>%
  summarise(mean_qv12 = mean(qv12, na.rm = TRUE),
            sd_qv12   = sd(qv12,   na.rm = TRUE),
            .groups = "drop") %>%
  mutate(vartype = vartype_of(var1, var2, x_vars, gc_names)) %>%
  arrange(desc(mean_qv12)) %>%
  slice_head(n = 15) %>%
  mutate(pair_label = paste(pair_label_of(var1), "×", pair_label_of(var2)))
top_pairs$pair_label <- factor(top_pairs$pair_label,
                               levels = rev(top_pairs$pair_label))

p_fig5 <- ggplot(top_pairs,
                 aes(x = pair_label, y = mean_qv12, fill = vartype)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_qv12 - sd_qv12, ymax = mean_qv12 + sd_qv12),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.0) +
  scale_fill_manual(values = interaction_palette(cfg),
                    name = "Interaction type") +
  coord_flip() +
  labs(x = NULL, y = "Mean interaction q-value (qv12) ± SD") +
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.minor = element_blank())
ggsave("figures/fig05-interaction-strength.pdf", p_fig5,
       width = 8, height = 6, dpi = 300)
cat("Figure 5 saved.\n")

# =============================================================================
# FIGURE 6: Dominant interaction category — spatial map + frequency bars
# =============================================================================

d_ibst$vartype <- vartype_of(d_ibst$var1, d_ibst$var2, x_vars, gc_names)

p_fig6a <- ggplot() +
  boundary_layer(cfg) +
  geom_point(data = d_ibst,
             aes(x = Longitude, y = Latitude, colour = vartype),
             size = cfg$figures$point_size, alpha = 0.8) +
  scale_colour_manual(values = interaction_palette(cfg),
                      name = "Dominant interaction") +
  labs(x = "Longitude", y = "Latitude") +
  map_theme() +
  guides(colour = guide_legend(override.aes = list(size = 3)))

type_count <- as.data.frame(table(d_ibst$vartype))
colnames(type_count) <- c("Type", "Count")
type_count$Pct <- round(type_count$Count / sum(type_count$Count) * 100, 1)

p_fig6b <- ggplot(type_count, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity", colour = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0(Pct, "%")), vjust = -0.4, size = 4) +
  scale_fill_manual(values = interaction_palette(cfg)) +
  labs(x = "Interaction type", y = "Number of locations") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

p_fig6 <- p_fig6a + p_fig6b + plot_layout(widths = c(1.4, 1))
ggsave("figures/fig06-interaction-type.pdf", p_fig6,
       width = 10, height = 4.5, dpi = 300)
cat("Figure 6 saved.\n")

# =============================================================================
# FIGURE 7: Local GOZH total effect — map + distribution
# =============================================================================

p_fig7a <- make_value_map(d_gozh$local_gozh, d_gozh[[xc]], d_gozh[[yc]], cfg,
                          title = NULL, legend_title = "GOZH q (all vars)")

mean_gozh <- mean(d_gozh$local_gozh, na.rm = TRUE)
p_fig7b <- ggplot(d_gozh, aes(x = local_gozh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "lightblue", colour = "black", alpha = 0.7) +
  geom_density(colour = "red", linewidth = 0.8) +
  geom_vline(xintercept = mean_gozh, colour = "blue",
             linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = Inf, y = Inf,
           label = paste("Mean:", round(mean_gozh, 3)),
           hjust = 1.15, vjust = 1.4, size = 4) +
  labs(x = "Local GOZH q-value (all variables)", y = "Density") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

p_fig7 <- p_fig7a + p_fig7b + plot_layout(widths = c(1.2, 1))
ggsave("figures/fig07-gozh-total-effect.pdf", p_fig7,
       width = 10, height = 4.5, dpi = 300)
cat("Figure 7 saved.\n")

# =============================================================================
# FIGURE 8: Method diagnostics — variogram (8a) + local vs global q (8b)
# =============================================================================

p_fig8a <- ggplot(d_vg, aes(x = dist_m / 1000, y = gamma)) +
  geom_point(size = 1.5, colour = "steelblue") +
  geom_vline(xintercept = d_lr$range_m[1] / 1000,
             colour = "red", linetype = "dashed") +
  geom_vline(xintercept = d_lr$threshold_m[1] / 1000,
             colour = "blue", linetype = "dashed") +
  annotate("text", x = d_lr$range_m[1] / 1000, y = Inf,
           label = "range", hjust = -0.1, vjust = 1.5,
           size = 3, colour = "red") +
  annotate("text", x = d_lr$threshold_m[1] / 1000, y = Inf,
           label = "local extent", hjust = 1.1, vjust = 1.5,
           size = 3, colour = "blue") +
  labs(x = "Distance (km)", y = "Semivariance") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

d_cmp <- d_vt %>%
  filter(Variable_type %in% c("X", "GC")) %>%
  filter(!is.na(Global_q))
d_cmp$label <- ifelse(d_cmp$Variable %in% names(labels),
                      labels[d_cmp$Variable], d_cmp$Variable)

p_fig8b <- ggplot(d_cmp, aes(x = Global_q, y = LPI_mean,
                             colour = Variable_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey60") +
  geom_errorbar(aes(ymin = LPI_min, ymax = LPI_max),
                width = 0, linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(aes(label = label), size = 2.5,
                           show.legend = FALSE) +
  scale_colour_manual(values = c("X" = unlist(cfg$figures$palette)[1],
                                 "GC" = unlist(cfg$figures$palette)[6]),
                      name = "Variable type") +
  labs(x = "Global q (OPGD benchmark)",
       y = "Local q, mean with [min, max]") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

p_fig8 <- p_fig8a + p_fig8b + plot_layout(widths = c(1, 1.3))
ggsave("figures/fig08-diagnostics-benchmark.pdf", p_fig8,
       width = 10, height = 4.5, dpi = 300)
cat("Figure 8 saved.\n")

cat("\n=== All figures saved to figures/ ===\n")
