# =============================================================================
# p02-classification.R — the green city classification and the alpha-beta plane
# The paper's Fig. 5 equivalent.
# =============================================================================
if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2")) { log_warn("ggplot2 missing; skipping fig03/fig04") } else {
library(ggplot2)
d <- read_analysis()

## alpha against beta, coloured by delta
t <- INTERACTION_TIME
pl <- data.frame(alpha = d[[col_access(t)]], beta = d[[col_accessibility(t)]],
                 delta = d[[col_delta(t)]])
lim <- stats::quantile(abs(pl$delta), 0.98)
p <- ggplot(pl, aes(alpha, beta, colour = pmax(-lim, pmin(lim, delta)))) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_point(size = 0.4, alpha = 0.5) +
  scale_colour_gradient2(low = PALETTE_DELTA[["negative"]], mid = "grey85",
                         high = PALETTE_DELTA[["positive"]], midpoint = 0,
                         name = expression(delta)) +
  labs(x = expression("Standardised access "*(alpha)),
       y = expression("Standardised accessibility "*(beta)),
       subtitle = sprintf("%d-minute walk; above the line accessibility exceeds access", t)) +
  theme_bw(base_size = 8)
save_figure(p, "fig03-alpha-beta-plane", width = FIG_WIDTH_SINGLE + 3, height = 9)

## classification shares by walking time
if (file.exists(F_CLASS)) {
  cl <- read_result(F_CLASS)
  cl$class <- factor(cl$class, levels = c("negative", "zero", "positive"))
  p2 <- ggplot(cl, aes(factor(walk_minutes), share, fill = class)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = PALETTE_DELTA, name = NULL,
                      labels = c(expression(delta<0), expression(delta%~~%0), expression(delta>0))) +
    scale_y_continuous(labels = function(x) paste0(100*x, "%")) +
    labs(x = "Walking time (minutes)", y = "Share of blocks") +
    theme_bw(base_size = 8) + theme(legend.position = "top")
  save_figure(p2, "fig04-classification", width = FIG_WIDTH_SINGLE + 2, height = 8)
}
}
