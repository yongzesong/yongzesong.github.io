# =============================================================================
# p02-lambda-maps.R — Fig. 5: seven local path coefficients across the plateau
# =============================================================================
# The published surfaces, drawn from the authors' own output with the class
# breaks printed in the legend of Fig. 5. Each path gets a coefficient map and
# a significance map, which is exactly the pair the paper shows beside every
# arrow of the diagram.
# =============================================================================

log_head("Figure — local path coefficients and their significance")
need_pkg("ggplot2", "figures")
library(ggplot2)

d <- readRDS(F_POINTS)
outline <- hull_outline(d)

long <- do.call(rbind, lapply(seq_len(nrow(PATHS)), function(j) {
  cc <- PATHS$published[j]
  data.frame(px = d$px, py = d$py,
             lambda = d[[cc]], p = d[[paste0("sig.", cc)]],
             panel = sprintf("%s. %s", PATHS$lambda[j], PATHS$label[j]),
             kind = PATHS$kind[j], stringsAsFactors = FALSE)
}))
long$panel <- factor(long$panel, levels = unique(long$panel))
long$class <- lambda_class(long$lambda)
long$sig   <- sig_class(long$p)

base_map <- function(df, mapping, palette, name, title, subtitle) {
  ggplot(df[!is.na(df$class) | !is.na(df$sig), ], mapping) +
    geom_path(data = outline, aes(px, py), inherit.aes = FALSE,
              colour = "#8E44AD", linewidth = 0.3) +
    geom_point(size = 0.28, na.rm = TRUE) +
    facet_wrap(~ panel, ncol = 2) +
    scale_colour_manual(values = palette, name = name, na.translate = FALSE) +
    coord_fixed() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_minimal(base_size = 8) +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          legend.position = "bottom", legend.key.size = unit(0.32, "cm"),
          strip.text = element_text(face = "bold", size = 7.5))
}

lam_pal <- stats::setNames(PALETTE_LAMBDA, levels(long$class))
p_lambda <- base_map(long[!is.na(long$class), ], aes(px, py, colour = class),
  lam_pal, expression(lambda),
  "Local path coefficients across the Tibetan Plateau",
  "published output, class breaks as printed in Fig. 5; locations outside [-1, 1] are not drawn")
save_figure(p_lambda, "fig03-lambda-maps", width = FIG_WIDTH_DOUBLE, height = 24)

p_sig <- base_map(long[!is.na(long$sig), ], aes(px, py, colour = sig),
  PALETTE_SIG, "Sig. of lambda",
  "Where the local pathways are statistically significant",
  "green: p < 0.01, pale green: p < 0.05, grey: not significant")
save_figure(p_sig, "fig04-significance-maps", width = FIG_WIDTH_DOUBLE, height = 24)

# How much of the study area each pathway actually covers, once the locations
# with no estimate and the estimates outside the plotted range are removed.
cover <- do.call(rbind, lapply(levels(long$panel), function(pn) {
  s <- long[long$panel == pn, ]
  data.frame(panel = pn, n_total = nrow(s),
             n_mapped = sum(!is.na(s$class)),
             pct_mapped = 100 * mean(!is.na(s$class)),
             pct_significant = 100 * mean(s$sig %in% c("<0.01", "<0.05"), na.rm = TRUE),
             stringsAsFactors = FALSE)
}))
write_result(cover, file.path(RES_DIR, "map-coverage.csv"))
print(cover, row.names = FALSE, digits = 3)
