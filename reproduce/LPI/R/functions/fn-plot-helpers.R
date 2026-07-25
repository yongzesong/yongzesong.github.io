# =============================================================================
# fn-plot-helpers.R — shared plotting components
# Source: cc005 LPI pipeline (plot results.R), generalized.
# Colour-method mapping is bound once from config and reused in every figure
# (writing style guide Rule 26: fixed colour mapping across all figures).
# =============================================================================

map_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text    = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 9),
      legend.text  = ggplot2::element_text(size = 8),
      panel.grid   = ggplot2::element_blank()
    )
}

facet_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(size = 7.5),
      axis.text        = ggplot2::element_text(size = 7),
      axis.title       = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# Optional study-area boundary layer ("" in config -> empty layer list).
boundary_layer <- function(cfg) {
  if (is.null(cfg$data$boundary) || cfg$data$boundary == "") return(list())
  boundary <- sf::st_read(cfg$data$boundary, quiet = TRUE)
  list(ggplot2::geom_sf(data = boundary, fill = "grey95",
                        colour = "grey60", linewidth = 0.3))
}

# One map panel with its own 6-quantile legend (equal-frequency bins).
make_value_map <- function(values, x_coords, y_coords, cfg,
                           title = NULL, legend_title = "Value") {
  palette  <- unlist(cfg$figures$palette)
  q_breaks <- unique(quantile(values, probs = seq(0, 1, length.out = 7),
                              na.rm = TRUE))
  n_bins <- length(q_breaks) - 1
  bin_labels <- paste0(round(q_breaks[-(n_bins + 1)], 3), "-",
                       round(q_breaks[-1], 3))

  df_map <- data.frame(
    x_plot  = x_coords,
    y_plot  = y_coords,
    value_q = cut(values, breaks = q_breaks, labels = bin_labels,
                  include.lowest = TRUE)
  )

  ggplot2::ggplot() +
    boundary_layer(cfg) +
    ggplot2::geom_point(
      data = df_map,
      ggplot2::aes(x = x_plot, y = y_plot, colour = value_q),
      size = cfg$figures$point_size, alpha = 0.8
    ) +
    ggplot2::scale_colour_manual(
      values = setNames(palette[1:n_bins], bin_labels),
      name   = legend_title,
      guide  = ggplot2::guide_legend(override.aes = list(size = 2.5))
    ) +
    ggplot2::labs(title = title, x = "Longitude", y = "Latitude") +
    map_theme() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 9, hjust = 0.5),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.text     = ggplot2::element_text(size = 7)
    )
}

# Faceted histogram + density + mean line of local q values.
plot_q_histograms <- function(q_df, var_list, label_map, ncol_val, fname,
                              width = 8, height = 6) {
  q_long <- reshape2::melt(q_df[, c("LocationID", var_list)],
                           id.vars = "LocationID")
  q_long$variable <- factor(label_map[as.character(q_long$variable)],
                            levels = label_map[var_list])
  mean_vals <- aggregate(value ~ variable, data = q_long,
                         FUN = mean, na.rm = TRUE)
  mean_vals$value <- round(mean_vals$value, 3)

  p <- ggplot2::ggplot(q_long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)), bins = 40,
      fill = "lightblue", colour = "black", alpha = 0.7) +
    ggplot2::geom_density(colour = "red", linewidth = 0.8) +
    ggplot2::geom_vline(
      data = mean_vals, ggplot2::aes(xintercept = value),
      colour = "blue", linetype = "dashed", linewidth = 0.8) +
    ggplot2::geom_text(
      data = mean_vals,
      ggplot2::aes(x = Inf, y = Inf, label = paste("Mean:", value)),
      hjust = 1.15, vjust = 1.4, size = 3, colour = "black",
      inherit.aes = FALSE) +
    ggplot2::facet_wrap(~ variable, scales = "free", ncol = ncol_val) +
    ggplot2::labs(x = "Local q-value", y = "Density") +
    facet_theme()

  ggplot2::ggsave(fname, p, width = width, height = height, dpi = 300)
  cat("Saved:", fname, "\n")
  invisible(p)
}

# Interaction pair category: "X x X", "X x GC", "GC x GC".
vartype_of <- function(var1, var2, x_vars, gc_names) {
  ifelse(var1 %in% x_vars & var2 %in% x_vars, "X × X",
         ifelse(var1 %in% gc_names & var2 %in% gc_names,
                "GC × GC", "X × GC"))
}

interaction_palette <- function(cfg) {
  setNames(
    c(cfg$figures$interaction_colors$xx,
      cfg$figures$interaction_colors$xgc,
      cfg$figures$interaction_colors$gcgc),
    c("X × X", "X × GC", "GC × GC")
  )
}
