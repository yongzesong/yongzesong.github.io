# =============================================================================
# p01-gc-maps.R — where each variable is geographically complex
# The paper's Fig. 5 equivalent: geocomplexity mapped for the selected variables.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
if (!has_pkg("ggplot2") || !file.exists(F_GC)) {
  log_warn("ggplot2 or geocomplexity results missing; skipping fig01")
} else {
library(ggplot2)
x  <- sf::read_sf(file.path(DERIVED, "analysis-layer.gpkg"))
gc <- read_result(F_GC)

show <- if (is.null(ERROR_EXPLAIN_VARS)) predictor_names(x)[1:min(4, length(predictor_names(x)))]
        else ERROR_EXPLAIN_VARS
cols <- paste0("GC_", show)
cols <- cols[cols %in% names(gc)]

# Geocomplexity is strongly skewed towards its upper end, so a linear colour
# ramp paints nearly every unit the same shade. Quantile classes, computed per
# variable, are what make the spatial pattern legible.
qbin <- function(v, k = 6) {
  br <- unique(stats::quantile(v, seq(0, 1, length.out = k + 1), na.rm = TRUE))
  cut(v, br, include.lowest = TRUE,
      labels = sprintf("%.3f-%.3f", br[-length(br)], br[-1]))
}

long <- do.call(rbind, lapply(cols, function(cn) {
  g <- x[, 0]
  g$class <- qbin(gc[[cn]])
  g$variable <- sub("^GC_", "", cn)
  g
}))

p <- ggplot(long) +
  geom_sf(aes(fill = as.integer(class)), colour = NA) +
  scale_fill_gradientn(colours = PALETTE_SEQ,
                       breaks = c(1, 6), labels = c("lowest", "highest"),
                       name = "Geocomplexity\n(quantile class)") +
  facet_wrap(~ variable) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8) +
  theme(axis.text = element_text(size = 5))
save_figure(p, "fig01-geocomplexity-maps", width = FIG_WIDTH_DOUBLE, height = 10)
}
