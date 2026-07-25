# =============================================================================
# p01-study-area.R — Figure: sample distribution and response field
# =============================================================================
# Manuscript role: Section 3.1, establishes that the case carries the spatial
# structure the indicator is meant to measure.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!has_pkg("ggplot2")) {
  log_warn("ggplot2 not installed; skipping figures")
} else {
  library(ggplot2)

  df <- read_result(F_ANALYSIS_DATA)
  spl <- if (file.exists(F_SPLIT)) read_result(F_SPLIT) else NULL

  p_map <- ggplot(df, aes(lon, lat, colour = .data[[RESPONSE]])) +
    geom_point(size = 1.3) +
    scale_colour_gradientn(colours = PALETTE_SEQ, name = RESPONSE) +
    coord_quickmap() +
    labs(x = "Longitude", y = "Latitude",
         subtitle = sprintf("(a) %s at %d sample locations", RESPONSE, nrow(df))) +
    theme_bw(base_size = 8) +
    theme(legend.key.width = unit(0.3, "cm"))

  p_hist <- ggplot(df, aes(.data[[RESPONSE]])) +
    geom_histogram(bins = 30, fill = PALETTE_SEQ[3], colour = "white", linewidth = 0.2) +
    labs(x = RESPONSE, y = "Count",
         subtitle = sprintf("(b) Distribution (mean = %.2f)", mean(df[[RESPONSE]]))) +
    theme_bw(base_size = 8)

  save_figure(p_map,  "fig01a-study-area", width = FIG_WIDTH_SINGLE, height = 8)
  save_figure(p_hist, "fig01b-response-distribution", width = FIG_WIDTH_SINGLE, height = 6)

  if (!is.null(spl)) {
    p_split <- ggplot(spl, aes(lon, lat, colour = split)) +
      geom_point(size = 1.1) +
      scale_colour_manual(values = c(train = "#B6992D", test = "#4E79A7"), name = NULL) +
      coord_quickmap() +
      labs(x = "Longitude", y = "Latitude",
           subtitle = "(c) Training and evaluation locations") +
      theme_bw(base_size = 8)
    save_figure(p_split, "fig01c-train-test-split", width = FIG_WIDTH_SINGLE, height = 8)
  }
}
