# =============================================================================
# p05-income-contribution.R — income effects (the paper's Fig. 10 / Fig. 11)
# (a) contribution bars  (b) GAM smooths  (c) GWR coefficient maps
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_CONTRIB)) {
  log_warn("ggplot2 or income results missing; skipping fig05")
} else {
library(ggplot2)
cc   <- read_result(F_CONTRIB)
gw   <- read_result(F_GWRCOEF)
srii <- read_result(F_SRII)
blocks <- read_result(F_BLOCKS)

# -- (a) contribution to income ----------------------------------------------
cc$dimension <- factor(cc$dimension, c("Gamma_A", "Gamma_B"))
cc$model <- factor(cc$model, c("GAM", "GWR", "Mean"))
pa <- ggplot(cc, aes(model, contribution, fill = dimension)) +
  geom_col(position = "stack", width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * contribution)),
            position = position_stack(vjust = 0.5), size = 2.3) +
  scale_fill_manual(values = c(Gamma_A = "#56B4E9", Gamma_B = "#F08A5D"),
                    labels = c(expression(Gamma[A]), expression(Gamma[B])),
                    name = NULL) +
  scale_y_continuous(labels = function(v) sprintf("%.0f%%", 100 * v)) +
  labs(x = NULL, y = "Contribution to income",
       title = "(a) Deviance explained") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        legend.position = "top")

# -- (b) GAM smooths ----------------------------------------------------------
dat <- data.frame(z = blocks[[INCOME_COL]],
                  ga = srii$gamma_a, gb = srii$gamma_b)
m <- mgcv::gam(z ~ s(ga) + s(gb), data = dat, method = "REML")
grid_a <- data.frame(ga = seq(0, 1, length.out = 100), gb = mean(dat$gb))
grid_b <- data.frame(ga = mean(dat$ga), gb = seq(0, 1, length.out = 100))
pr_a <- stats::predict(m, grid_a, se.fit = TRUE)
pr_b <- stats::predict(m, grid_b, se.fit = TRUE)
sm <- rbind(
  data.frame(x = grid_a$ga, fit = pr_a$fit, se = pr_a$se.fit, dim = "Gamma[A]"),
  data.frame(x = grid_b$gb, fit = pr_b$fit, se = pr_b$se.fit, dim = "Gamma[B]"))

pb <- ggplot(sm, aes(x, fit)) +
  geom_ribbon(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
              fill = "#D8E6F2") +
  geom_line(colour = "#2166AC", linewidth = 0.6) +
  facet_wrap(~dim, labeller = label_parsed, scales = "free_x") +
  labs(x = "Index value", y = "Income (1000 $ per person)",
       title = "(b) GAM partial effects") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"))

# -- (c) GWR coefficient surfaces --------------------------------------------
co <- rbind(
  data.frame(gw[, c("x_km", "y_km")], value = gw$beta_a,
             panel = "GWR~coefficient~of~Gamma[A]"),
  data.frame(gw[, c("x_km", "y_km")], value = gw$beta_b,
             panel = "GWR~coefficient~of~Gamma[B]"))
pc <- ggplot(co, aes(x_km, y_km, fill = value)) +
  geom_tile() +
  facet_wrap(~panel, labeller = label_parsed) +
  scale_fill_gradientn(colours = PALETTE_SEQ, name = "Coefficient") +
  coord_equal(expand = FALSE) +
  labs(x = "x (km)", y = "y (km)",
       title = "(c) Regional coefficients (GWR)") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        panel.grid = element_blank())

g <- if (has_pkg("patchwork")) {
  patchwork::wrap_plots(
    patchwork::wrap_plots(pa, pb, nrow = 1, widths = c(1, 1.6)),
    pc, ncol = 1, heights = c(1, 1.05))
} else pc
save_figure(g, "fig05-income-contribution", width = FIG_WIDTH_DOUBLE, height = 14)
}
