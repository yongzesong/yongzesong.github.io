# =============================================================================
# p03-lambda-distributions.R — the boxplot of Fig. 5 and the authors' violin plot
# =============================================================================
# Two views of the same seven columns. The boxplot is the panel in the top right
# of Fig. 5; the violin plot is the figure the authors' own script draws, kept
# here in the form they wrote it so the filter it applies stays visible.
# =============================================================================

log_head("Figure — distribution of the local path coefficients")
need_pkg("ggplot2", "figures")
library(ggplot2)

d <- readRDS(F_POINTS)

long <- do.call(rbind, lapply(seq_len(nrow(PATHS)), function(j) {
  data.frame(lambda_id = PATHS$lambda[j], label = PATHS$short[j],
             kind = PATHS$kind[j], value = d[[PATHS$published[j]]],
             stringsAsFactors = FALSE)
}))
long$lambda_id <- factor(long$lambda_id, levels = PATHS$lambda)
long$label     <- factor(long$label, levels = PATHS$short)

# The filter that makes the paper's numbers appear. Everything printed in
# Table 2 and drawn in Fig. 5 lives inside this window.
kept <- long[is.finite(long$value) &
             long$value >= LAMBDA_PLOT_RANGE[1] &
             long$value <= LAMBDA_PLOT_RANGE[2], ]
log_info("%d of %d estimates fall inside [-1, 1] (%.1f %%)",
         nrow(kept), sum(is.finite(long$value)),
         100 * nrow(kept) / sum(is.finite(long$value)))

p_box <- ggplot(kept, aes(lambda_id, value)) +
  geom_hline(yintercept = 0, colour = "#E8A33D", linewidth = 0.4) +
  geom_boxplot(fill = "#BFBFBF", colour = "grey25", outlier.size = 0.2,
               linewidth = 0.3, width = 0.55) +
  scale_x_discrete(labels = parse(text = paste0("lambda[", 1:7, "]"))) +
  labs(x = NULL, y = "Value",
       title = "Local path coefficients, all locations",
       subtitle = "the panel in the top right of Fig. 5") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
save_figure(p_box, "fig05-lambda-boxplot", width = FIG_WIDTH_SINGLE + 2, height = 7)

# The authors' violin plot, same data, their jitter and their red mean marker.
p_violin <- ggplot(kept, aes(label, value)) +
  geom_jitter(width = 0.1, alpha = 0.12, size = 0.25, colour = "grey30") +
  geom_violin(trim = FALSE, fill = NA, colour = "purple", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", colour = "red", size = 2,
               position = position_nudge(y = 0.1)) +
  labs(x = "Variable", y = "Local Power Value",
       title = "Violin Plot of Local Power Values",
       subtitle = "reproduction of the figure drawn by the authors' script") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.minor = element_blank())
save_figure(p_violin, "fig06-lambda-violin", width = FIG_WIDTH_DOUBLE - 3, height = 9)

# What the filter removes. The published output runs to +3910 on one path, so
# the tails are worth showing once rather than passing over in silence.
tails <- do.call(rbind, lapply(seq_len(nrow(PATHS)), function(j) {
  v <- d[[PATHS$published[j]]]; v <- v[is.finite(v)]
  data.frame(lambda = PATHS$lambda[j], path = PATHS$label[j],
             n = length(v), n_kept = sum(abs(v) <= 1),
             pct_kept = 100 * mean(abs(v) <= 1),
             min = min(v), max = max(v), stringsAsFactors = FALSE)
}))
write_result(tails, file.path(RES_DIR, "lambda-range-filter.csv"))
print(tails, row.names = FALSE, digits = 4)

p_tail <- ggplot(tails, aes(stats::reorder(lambda, pct_kept), pct_kept)) +
  geom_col(fill = "#6C3483", width = 0.6) +
  geom_text(aes(label = sprintf("%.0f %%", pct_kept)), hjust = -0.15, size = 2.6) +
  scale_x_discrete(labels = function(x) x) +
  coord_flip(clip = "off", ylim = c(0, 108)) +
  labs(x = NULL, y = "Estimates inside [-1, 1] (%)",
       title = "How much of each pathway survives the plotting filter",
       subtitle = "Table 2 and Fig. 5 describe only the part shown here") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
save_figure(p_tail, "fig07-range-filter", width = FIG_WIDTH_SINGLE + 2, height = 6)
