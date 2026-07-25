# =============================================================================
# p04-risk.R — risk-detector figure: mean response per stratum, one panel per
# variable (paper Fig 6-style). Shows where the response is high vs low.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_RISKMEAN)) {
  log_warn("ggplot2 or risk-mean results missing; skipping fig04")
} else {
library(ggplot2)
rm_df <- read_result(F_RISKMEAN)
rm_df$variable <- factor(rm_df$variable, levels = c(CATEGORICAL, CONTINUOUS))
rm_df$stratum  <- factor(rm_df$stratum, levels = unique(rm_df$stratum))

p <- ggplot(rm_df, aes(stratum, mean_response, group = variable)) +
  geom_line(colour = "#4E79A7", linewidth = 0.5) +
  geom_point(colour = "#B2182B", size = 1.3) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Stratum", y = sprintf("Mean %s", RESPONSE)) +
  theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 5))

save_figure(p, "fig04-risk-means", width = FIG_WIDTH_DOUBLE, height = 12)
}
