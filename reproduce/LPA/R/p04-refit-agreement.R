# =============================================================================
# p04-refit-agreement.R — the independent refit beside the published surfaces
# =============================================================================
# Where the reproduction is honest about its limits. The refit recovers the sign
# and the broad spatial pattern of most pathways; it does not recover the
# individual estimates, because the specification behind the published numbers
# is a reconstruction rather than the authors' own file.
# =============================================================================

log_head("Figure — refit against published estimates")
need_pkg("ggplot2", "figures")
library(ggplot2)

if (!file.exists(F_LAMBDA)) {
  log_warn("no recomputed lambda — run step 30 first");
} else {

d   <- readRDS(F_POINTS)
rec <- read_result(F_LAMBDA)
agr <- if (file.exists(F_AGREE)) read_result(F_AGREE) else NULL
outline <- hull_outline(d)

pairs <- do.call(rbind, lapply(seq_len(nrow(PATHS)), function(j) {
  cc <- PATHS$published[j]
  data.frame(px = rec$px, py = rec$py,
             recomputed = rec[[cc]], published = d[[cc]][rec$id],
             panel = sprintf("%s. %s", PATHS$lambda[j], PATHS$label[j]),
             stringsAsFactors = FALSE)
}))
pairs$panel <- factor(pairs$panel, levels = unique(pairs$panel))
ok <- is.finite(pairs$recomputed) & is.finite(pairs$published) &
      abs(pairs$recomputed) <= 1 & abs(pairs$published) <= 1

p_sc <- ggplot(pairs[ok, ], aes(published, recomputed)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey65", linewidth = 0.3) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.2) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.2) +
  geom_point(size = 0.3, alpha = 0.3, colour = "#2166AC") +
  facet_wrap(~ panel, ncol = 4) +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(x = "Published lambda", y = "Recomputed lambda",
       title = "Independent refit against the published estimates",
       subtitle = "one point per location; the diagonal is exact agreement") +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 7))
save_figure(p_sc, "fig08-refit-scatter", width = FIG_WIDTH_DOUBLE, height = 11)

if (!is.null(agr)) {
  agr$panel <- sprintf("%s. %s", agr$lambda, agr$path)
  bars <- rbind(
    data.frame(panel = agr$panel, metric = "Sign agreement (%)",
               value = agr$sign_agreement),
    data.frame(panel = agr$panel, metric = "Correlation",
               value = 100 * agr$correlation))
  p_ag <- ggplot(bars, aes(stats::reorder(panel, value), value, fill = metric)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_hline(yintercept = 50, linetype = "33", colour = "grey50") +
    scale_fill_manual(values = c("Sign agreement (%)" = "#6C3483",
                                 "Correlation" = "#E8A33D"), name = NULL) +
    coord_flip() +
    labs(x = NULL, y = "Per cent (correlation shown as r x 100)",
         title = "How far the refit reproduces each pathway",
         subtitle = "dashed line: agreement expected by chance") +
    theme_minimal(base_size = 9) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom")
  save_figure(p_ag, "fig09-refit-agreement", width = FIG_WIDTH_DOUBLE - 3, height = 8)
}

# Side-by-side maps for the pathway that reproduces best, as the honest example.
if (!is.null(agr)) {
  best <- agr$lambda[which.max(agr$sign_agreement)]
  jj   <- match(best, PATHS$lambda)
  cc   <- PATHS$published[jj]
  side <- rbind(
    data.frame(px = rec$px, py = rec$py, value = rec[[cc]], source = "Recomputed"),
    data.frame(px = rec$px, py = rec$py, value = d[[cc]][rec$id], source = "Published"))
  side$class <- lambda_class(side$value)
  p_side <- ggplot(side[!is.na(side$class), ], aes(px, py, colour = class)) +
    geom_path(data = outline, aes(px, py), inherit.aes = FALSE,
              colour = "#8E44AD", linewidth = 0.3) +
    geom_point(size = 0.3) +
    facet_wrap(~ source) +
    scale_colour_manual(values = stats::setNames(PALETTE_LAMBDA,
                          levels(side$class)), name = expression(lambda)) +
    coord_fixed() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    labs(x = NULL, y = NULL,
         title = sprintf("%s: %s", best, PATHS$label[jj]),
         subtitle = "the pathway the refit recovers most closely") +
    theme_minimal(base_size = 8) +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          legend.position = "bottom", legend.key.size = unit(0.32, "cm"))
  save_figure(p_side, "fig10-refit-side-by-side", width = FIG_WIDTH_DOUBLE,
              height = 9)
}

}
