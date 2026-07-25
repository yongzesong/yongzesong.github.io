# =============================================================================
# p02-error-explanation.R — the headline: how much of each model's error
# geocomplexity accounts for, and how the errors relate to geocomplexity.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping fig02/fig03")
} else {
library(ggplot2)

## -- share of error explained, by model ---------------------------------------
if (file.exists(F_EXPLAIN)) {
  e <- read_result(F_EXPLAIN)
  e$model <- factor(e$model, levels = e$model[order(-e$R2)])
  p <- ggplot(e, aes(model, R2, fill = model)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.3f", R2)), vjust = -0.4, size = 2.8) +
    scale_fill_manual(values = PALETTE_MODEL, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(x = NULL, y = expression("Share of the model's error explained ("*R^2*")")) +
    theme_bw(base_size = 8)
  save_figure(p, "fig02-error-explained", width = FIG_WIDTH_SINGLE + 1, height = 7)
}

## -- error against geocomplexity ----------------------------------------------
if (file.exists(F_ERRORS) && file.exists(F_GC)) {
  err <- read_result(F_ERRORS); gc <- read_result(F_GC)
  vars <- if (is.null(ERROR_EXPLAIN_VARS)) names(gc)[1:2] else paste0("GC_", ERROR_EXPLAIN_VARS)
  vars <- vars[vars %in% names(gc)]
  long <- do.call(rbind, lapply(names(err), function(nm)
    do.call(rbind, lapply(vars, function(v)
      data.frame(model = sub("^error_", "", nm), variable = sub("^GC_", "", v),
                 gc = gc[[v]], abs_error = abs(err[[nm]]))))))
  p2 <- ggplot(long, aes(gc, abs_error, colour = model)) +
    geom_point(size = 0.5, alpha = 0.45) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, formula = y ~ x) +
    facet_grid(model ~ variable, scales = "free") +
    scale_colour_manual(values = PALETTE_MODEL, guide = "none") +
    labs(x = "Geocomplexity", y = "Absolute error") +
    theme_bw(base_size = 8)
  save_figure(p2, "fig03-error-vs-geocomplexity", width = FIG_WIDTH_DOUBLE, height = 11)
}

## -- where geocomplexity raises or lowers the error ---------------------------
# The global scatter above is nearly flat, which is exactly the paper's point:
# the association is local. These are the GWR coefficients, mapped.
if (file.exists(F_EXPCOEF)) {
  cf <- read_result(F_EXPCOEF)
  x  <- sf::read_sf(file.path(DERIVED, "analysis-layer.gpkg"))
  ccols <- grep("^coef_GC_", names(cf), value = TRUE)
  long <- do.call(rbind, lapply(unique(cf$model), function(m) {
    sub <- cf[cf$model == m, , drop = FALSE]
    do.call(rbind, lapply(ccols, function(cn) {
      g <- x[, 0]; g$coef <- sub[[cn]]
      g$model <- m; g$variable <- sub("^coef_GC_", "", cn); g
    }))
  }))
  # A few extreme local coefficients would otherwise wash the whole panel out,
  # so the colour scale is clamped at the 98th percentile and the tails squished.
  lim <- stats::quantile(abs(long$coef), 0.98, na.rm = TRUE)
  p3 <- ggplot(long) +
    geom_sf(aes(fill = coef), colour = NA) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                         midpoint = 0, limits = c(-lim, lim),
                         oob = scales::squish,
                         name = "Local\ncoefficient") +
    facet_grid(model ~ variable) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 7) +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
  save_figure(p3, "fig06-local-coefficients", width = FIG_WIDTH_DOUBLE, height = 13)
}
}
