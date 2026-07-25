# =============================================================================
# p01-ripleys-k.R — Fig. 4: observed against expected K, and where L peaks
# =============================================================================

log_head("Figure — Ripley's K and the optimal local range")
need_pkg("ggplot2", "figures")
library(ggplot2)

curve <- read_result(F_KCURVE)
rng   <- read_result(F_RANGE)
r_opt <- rng$r_opt_km[rng$selected][1]

# The estimator's final distances collapse; drawing them hides everything else.
curve <- curve[trim_collapse(curve$r, curve$L), ]
log_info("plotting r up to %.0f km, where the border correction stops being reliable",
         max(curve$r))

# How sharply the peak is defined: the width of the plateau within 5 units of it.
top <- curve[curve$L > max(curve$L) - 5, ]
log_info("L is within 5 of its maximum over r = %.0f to %.0f km", min(top$r), max(top$r))

kdat <- rbind(
  data.frame(r = curve$r, K = curve$K_observed, series = "Observed K"),
  data.frame(r = curve$r, K = curve$K_expected, series = "Expected K (CSR)"))

p_k <- ggplot(kdat, aes(r, K / 1e6, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("Observed K" = "#E03C31",
                                 "Expected K (CSR)" = "#2166AC"), name = NULL) +
  labs(x = "Distance r (km)", y = expression(K(r)~"("*10^6~km^2*")"),
       title = "Ripley's K against spatial randomness") +
  theme_minimal(base_size = 9) +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        panel.grid.minor = element_blank())

p_l <- ggplot(curve, aes(r, L)) +
  annotate("rect", xmin = min(top$r), xmax = max(top$r), ymin = -Inf, ymax = Inf,
           fill = "#E03C31", alpha = 0.08) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(colour = "#6C3483", linewidth = 0.7) +
  geom_vline(xintercept = r_opt, linetype = "22", colour = "#E03C31") +
  geom_vline(xintercept = 707.29, linetype = "33", colour = "grey40") +
  # Both labels sit to the LEFT of their lines: the lines are near the right
  # edge of the panel, and anything placed to their right is clipped away.
  annotate("text", x = min(top$r) - 12, y = max(curve$L), hjust = 1, vjust = 1,
           size = 2.7, colour = "#E03C31",
           label = sprintf("r_opt = %.2f km", r_opt)) +
  annotate("text", x = 707.29 - 12, y = 0, hjust = 1, vjust = -0.4,
           size = 2.7, colour = "grey40", label = "published 707.29 km") +
  labs(x = "Distance r (km)", y = expression(L(r)==sqrt(K(r)/pi)-r),
       title = "Besag's L peaks at the local range",
       subtitle = sprintf("shaded: within 5 of the maximum, r = %.0f to %.0f km",
                          min(top$r), max(top$r))) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank())

if (has_pkg("patchwork")) {
  p <- patchwork::wrap_plots(p_k, p_l, ncol = 2)
} else {
  p <- p_l
  log_warn("patchwork not installed — writing the L panel only")
}
save_figure(p, "fig01-ripleys-k", width = FIG_WIDTH_DOUBLE, height = 8)

# The sensitivity of the peak to the edge correction, as a small companion.
rng$label <- sprintf("%s / %s", rng$correction, rng$window)
p_corr <- ggplot(rng, aes(stats::reorder(label, r_opt_km), r_opt_km,
                          fill = selected)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 707.29, linetype = "33", colour = "#E03C31") +
  geom_text(aes(label = sprintf("%.0f", r_opt_km)), hjust = -0.15, size = 2.6) +
  scale_fill_manual(values = c("TRUE" = "#6C3483", "FALSE" = "#BFBFBF"),
                    guide = "none") +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Optimal local range (km)",
       title = "Edge correction decides the range",
       subtitle = "dashed line: the published 707.29 km") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
save_figure(p_corr, "fig02-edge-correction", width = FIG_WIDTH_SINGLE + 2, height = 6)
