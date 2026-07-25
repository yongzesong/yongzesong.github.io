# =============================================================================
# p02-dmu-tradeoff.R — the trade-off relation itself (the paper's Fig. 6)
# (a) joint distribution of Gamma_A and Gamma_B across blocks
# (b) LOESS utility curve, marginal utility and the IR / MR / NR boundaries
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_UTILITY)) {
  log_warn("ggplot2 or DMU results missing; skipping fig02")
} else {
library(ggplot2)
s      <- read_result(F_SRII)
bins   <- read_result(F_BINS)
curve  <- read_result(F_UTILITY)
bounds <- read_result(F_BOUNDS)
t1 <- bounds$gamma_a[1]; t2 <- bounds$gamma_a[2]

# -- (a) joint distribution ---------------------------------------------------
pa <- ggplot(s, aes(gamma_a, gamma_b)) +
  geom_bin2d(bins = 40) +
  scale_fill_gradientn(colours = c("#5B4FA0", "#3FA0C0", "#8CD07C",
                                   "#F2E34C", "#E8483C"),
                       name = "Number\nof blocks") +
  labs(x = expression("Quantity dimension ("*Gamma[A]*")"),
       y = expression("Quality dimension ("*Gamma[B]*")"),
       title = "(a) Joint distribution of blocks") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"))

# -- (b) utility curve and marginal utility -----------------------------------
eta <- curve$eta[!is.na(curve$eta)]
u_rng <- range(curve$u); e_rng <- range(eta)
b <- diff(u_rng) / diff(e_rng)            # map eta onto the utility axis
a <- u_rng[1] - b * e_rng[1]
curve$eta_plot <- a + b * curve$eta
eta0 <- a                                  # the eta = 0 gridline

stage_lab <- data.frame(
  x = c(t1 / 2, (t1 + t2) / 2, (t2 + max(curve$ga)) / 2),
  lab = c("IR", "MR", "NR"))

pb <- ggplot() +
  geom_ribbon(data = curve, aes(ga, ymin = u - 1.96 * se, ymax = u + 1.96 * se),
              fill = "#F8E3C2", alpha = 0.8) +
  geom_point(data = bins, aes(ga, gb), size = 0.9, colour = "black") +
  geom_line(data = curve, aes(ga, u, colour = "u"), linewidth = 0.7) +
  geom_line(data = curve[!is.na(curve$eta), ],
            aes(ga, eta_plot, colour = "eta"), linewidth = 0.6) +
  geom_hline(yintercept = eta0, linetype = "dashed",
             colour = "grey55", linewidth = 0.3) +
  geom_vline(xintercept = c(t1, t2), linetype = "dashed", linewidth = 0.4) +
  annotate("label", x = c(t1, t2), y = min(curve$u) + 0.004,
           label = sprintf("%.3f", c(t1, t2)), size = 2.4, fill = "#CFE3F0") +
  geom_text(data = stage_lab, aes(x, max(curve$u + 1.96 * curve$se) + 0.006,
                                  label = lab), size = 2.8, fontface = "bold") +
  scale_colour_manual(values = c(u = "#E8483C", eta = "#3B7FD4"),
                      labels = c(u = expression(u(Gamma[A])),
                                 eta = expression(eta)),
                      breaks = c("u", "eta"), name = NULL) +
  scale_y_continuous(
    name = expression(Gamma[B]),
    sec.axis = sec_axis(~ (. - a) / b, name = "Marginal utility")) +
  labs(x = expression(Gamma[A]),
       title = "(b) Utility, marginal utility and DMU stages") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        legend.position = c(0.86, 0.32),
        legend.background = element_rect(fill = alpha("white", 0.6)))

g <- if (has_pkg("patchwork")) patchwork::wrap_plots(pa, pb, nrow = 1) else pb
save_figure(g, "fig02-dmu-tradeoff", width = FIG_WIDTH_DOUBLE, height = 8)
}
