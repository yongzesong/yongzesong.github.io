# =============================================================================
# p03-drivers.R — the Power of Determinant across variables and scales
# The paper's Fig. 6 equivalent, plus the interaction matrix.
# =============================================================================
if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2")) { log_warn("ggplot2 missing; skipping fig05/fig06") } else {
library(ggplot2)

if (file.exists(F_DRIVERS)) {
  pd <- read_result(F_DRIVERS)
  ref <- pd[pd$walk_minutes == 5, ]
  ord <- ref$variable[order(ref$PD)]
  pd$variable <- factor(pd$variable, levels = unique(c(ord, pd$variable)))
  pd$scale_lab <- factor(sprintf("%d min", pd$walk_minutes),
                         levels = sprintf("%d min", WALK_TIMES))
  p <- ggplot(pd, aes(PD, variable, fill = type)) +
    geom_col(width = 0.7) +
    facet_wrap(~ scale_lab, nrow = 2) +
    scale_fill_manual(values = c(raw = "#4E79A7", contextualised = "#B2182B",
                                 region = "#59A14F"), name = NULL) +
    labs(x = "Power of Determinant (PD)", y = NULL) +
    theme_bw(base_size = 7) + theme(legend.position = "top")
  save_figure(p, "fig05-drivers-pd", width = FIG_WIDTH_DOUBLE, height = 13)
}

if (file.exists(F_INTERACT)) {
  it <- read_result(F_INTERACT)
  lev <- unique(c(it$variable1, it$variable2))
  it$variable1 <- factor(it$variable1, levels = lev)
  it$variable2 <- factor(it$variable2, levels = rev(lev))
  p2 <- ggplot(it, aes(variable1, variable2, size = PID, colour = interaction)) +
    geom_point(alpha = 0.9) +
    scale_size_area(max_size = 8, name = "PID") +
    scale_colour_manual(values = c("Enhance, nonlinear" = "#B2182B",
                                   "Enhance, bi-" = "#E6A23C",
                                   "Independent" = "#999999",
                                   "Weaken, uni-" = "#4E79A7",
                                   "Weaken, nonlinear" = "#59A14F"), name = NULL) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 6) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")
  save_figure(p2, "fig06-interactions", width = FIG_WIDTH_DOUBLE, height = 13)
}
}
