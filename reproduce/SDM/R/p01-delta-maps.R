# =============================================================================
# p01-delta-maps.R — where accessibility exceeds access, and where it falls short
# The paper's Fig. 3 and Fig. 4 equivalents.
# =============================================================================
if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !has_pkg("sf")) {
  log_warn("ggplot2 or sf missing; skipping fig01/fig02")
} else {
library(ggplot2)
g <- sf::read_sf(file.path(DERIVED, "blocks-joined.gpkg"))

qbin <- function(v, k = 6) {
  br <- unique(stats::quantile(v, seq(0, 1, length.out = k + 1), na.rm = TRUE))
  cut(v, br, include.lowest = TRUE, labels = FALSE)
}

## alpha and beta side by side, at the demonstration scale
t <- DEMO_TIME
long <- rbind(
  transform(g[, 0], value = qbin(g[[col_access(t)]]),        measure = "Access (alpha)"),
  transform(g[, 0], value = qbin(g[[col_accessibility(t)]]), measure = "Accessibility (beta)"))
# 40% of blocks tie at the bottom of both measures, so quantile breaks collapse and
# the number of bins is data-dependent. Label the ends of whatever range results.
brk <- range(long$value, na.rm = TRUE)
p <- ggplot(long) + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_gradientn(colours = PALETTE_SEQ, breaks = brk,
                       labels = c("lowest", "highest"), name = NULL) +
  facet_wrap(~ measure) + labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8) + theme(axis.text = element_blank(), axis.ticks = element_blank())
save_figure(p, "fig01-access-accessibility", width = FIG_WIDTH_DOUBLE, height = 9)

## delta across the walking times
dl <- do.call(rbind, lapply(WALK_TIMES, function(tt) {
  v <- g[[col_delta(tt)]]
  lim <- stats::quantile(abs(v), 0.98, na.rm = TRUE)
  transform(g[, 0], value = pmax(-lim, pmin(lim, v)),
            scale_lab = factor(sprintf("%d min", tt), levels = sprintf("%d min", WALK_TIMES)))
}))
p2 <- ggplot(dl) + geom_sf(aes(fill = value), colour = NA) +
  scale_fill_gradient2(low = PALETTE_DELTA[["negative"]], mid = "white",
                       high = PALETTE_DELTA[["positive"]], midpoint = 0,
                       name = expression(delta)) +
  facet_wrap(~ scale_lab, nrow = 2) + labs(x = NULL, y = NULL) +
  theme_bw(base_size = 7) + theme(axis.text = element_blank(), axis.ticks = element_blank())
save_figure(p2, "fig02-delta-maps", width = FIG_WIDTH_DOUBLE, height = 12)
}
