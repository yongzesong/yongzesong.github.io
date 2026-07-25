# =============================================================================
# fn_plots.R — figure builders, one per reproduction target
# =============================================================================
# Each builder returns a ggplot object and is named for the figure it produces
# in 07_figures.R. Figure numbers follow the reference study so a reader can
# compare panel by panel; see 00_docs/04-figure-table-plan.md for the mapping.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

soh_theme <- function(base_size = 9) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.2, colour = "grey88"),
      strip.background = element_rect(fill = "grey95", colour = NA),
      strip.text = element_text(face = "bold", size = base_size - 0.5),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(4, "mm"),
      plot.title = element_text(face = "bold", size = base_size + 1)
    )
}

# Diverging palette for outlier patterns, sequential for magnitudes.
soh_pal_div <- c("#2166AC", "#F7F7F7", "#B2182B")
soh_pal_seq <- c("#F7FBFF", "#6BAED6", "#08306B")

# --- F01 response map --------------------------------------------------------

soh_fig_response_map <- function(points, cfg) {
  ggplot(points, aes(x, y, colour = response)) +
    geom_point(size = 1.2) +
    scale_colour_gradientn(colours = soh_pal_seq, name = cfg$study$response_label) +
    coord_equal() +
    labs(x = paste0("Easting (", cfg$data$dist_unit, ")"),
         y = paste0("Northing (", cfg$data$dist_unit, ")")) +
    soh_theme()
}

# --- F02 covariate maps ------------------------------------------------------

soh_fig_covariate_maps <- function(grids, cfg, ncol = 3) {
  codes <- cfg$variables$codes
  long <- do.call(rbind, lapply(codes, function(v) {
    data.frame(x = grids$x, y = grids$y, value = grids[[v]],
               variable = factor(unname(cfg$variables$labels[v]),
                                 levels = unname(cfg$variables$labels[codes])))
  }))
  ggplot(long, aes(x, y, fill = value)) +
    geom_raster() +
    facet_wrap(~variable, ncol = ncol) +
    scale_fill_gradientn(colours = soh_pal_seq, name = "Value") +
    coord_equal() +
    labs(x = NULL, y = NULL) +
    soh_theme() +
    theme(axis.text = element_text(size = 6))
}

# --- F03 Moran's I -----------------------------------------------------------

soh_fig_moran <- function(moran, cfg) {
  if (is.null(moran)) return(NULL)
  p1 <- ggplot(moran$scatter, aes(z, lag)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_point(size = 0.9, alpha = 0.6, colour = "#2166AC") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = "#B2182B", linewidth = 0.6) +
    labs(x = "Standardised response", y = "Spatial lag",
         title = sprintf("(a) Moran scatter, I = %.3f", moran$statistic)) +
    soh_theme()

  p2 <- ggplot(data.frame(sim = moran$sim), aes(sim)) +
    geom_histogram(bins = 40, fill = "grey80", colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = moran$statistic, colour = "#B2182B", linewidth = 0.7) +
    labs(x = "Permuted Moran's I", y = "Frequency",
         title = sprintf("(b) %d permutations, p = %.3f",
                         moran$n_sim, moran$p_value)) +
    soh_theme()

  soh_combine(p1, p2)
}

soh_combine <- function(...) {
  ps <- list(...)
  if (requireNamespace("patchwork", quietly = TRUE)) {
    Reduce(`+`, ps) + patchwork::plot_layout(nrow = 1)
  } else {
    ps[[1]]
  }
}

# --- F04 SOP maps across buffers --------------------------------------------

soh_fig_sop_maps <- function(points, sopvars, cfg, variable, buffers = NULL) {
  buffers <- buffers %||% cfg$sop$buffers
  show <- buffers[unique(round(seq(1, length(buffers), length.out = 4)))]
  blk <- sopvars[[variable]]
  long <- do.call(rbind, lapply(show, function(b) {
    rbind(
      data.frame(x = points$x, y = points$y,
                 value = blk[[paste0("p_b", b)]],
                 sign = "Positive SOP", radius = b),
      data.frame(x = points$x, y = points$y,
                 value = blk[[paste0("n_b", b)]],
                 sign = "Negative SOP", radius = b)
    )
  }))
  long$radius <- factor(paste0(long$radius, " ", cfg$data$dist_unit),
                        levels = paste0(show, " ", cfg$data$dist_unit))
  long$sign <- factor(long$sign, levels = c("Positive SOP", "Negative SOP"))
  ggplot(long, aes(x, y, colour = value)) +
    geom_point(size = 0.9) +
    facet_grid(sign ~ radius) +
    scale_colour_gradient2(low = soh_pal_div[1], mid = soh_pal_div[2],
                           high = soh_pal_div[3], midpoint = 0, name = "SOP") +
    coord_equal() +
    labs(x = NULL, y = NULL,
         title = paste0("Spatial outlier patterns of ",
                        unname(cfg$variables$labels[variable]))) +
    soh_theme() +
    theme(axis.text = element_text(size = 6))
}

# --- F05 individual PD -------------------------------------------------------

soh_fig_individual_pd <- function(individual, cfg) {
  d <- individual
  d$name <- factor(d$label, levels = d$label[order(d$qv)])
  ggplot(d, aes(name, qv, fill = type)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = sprintf("%.3f", qv)), hjust = -0.15, size = 2.3) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = c(variable = "#9ECAE1", SOP = "#B2182B"),
                      name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(x = NULL, y = "Power of determinant (PD)") +
    soh_theme()
}

# --- F06 interaction heatmap -------------------------------------------------

soh_fig_interaction_heatmap <- function(pairs, cfg, title = NULL) {
  d <- pairs
  # mirror SOP-SOP pairs so the matrix reads symmetrically
  if (unique(d$mode)[1] == "sop_sop") {
    d2 <- d; d2$factor1 <- d$factor2; d2$factor2 <- d$factor1
    d <- rbind(d, d2)
  }
  ggplot(d, aes(factor1, factor2, fill = qv)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", qv)), size = 2.1, colour = "grey20") +
    scale_fill_gradientn(colours = soh_pal_seq, name = "PD") +
    labs(x = NULL, y = NULL, title = title) +
    soh_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# --- F07 augmentation gain ---------------------------------------------------

soh_fig_augmentation <- function(aug, cfg, level = "variable") {
  d <- aug[aug$level == level, ]
  long <- rbind(
    data.frame(unit = d$unit, qv = d$qv_base, set = "Variable only"),
    data.frame(unit = d$unit, qv = d$qv_augmented, set = "Variable + SOP")
  )
  long$unit <- factor(long$unit, levels = d$unit[order(d$qv_augmented)])
  long$set <- factor(long$set, levels = c("Variable only", "Variable + SOP"))

  p1 <- ggplot(long, aes(unit, qv, fill = set)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    coord_flip() +
    scale_fill_manual(values = c("#9ECAE1", "#B2182B"), name = NULL) +
    labs(x = NULL, y = "PD", title = "(a) PD with and without SOP") +
    soh_theme()

  d$unit <- factor(d$unit, levels = d$unit[order(d$delta)])
  p2 <- ggplot(d, aes(unit, delta)) +
    geom_col(fill = "#B2182B", width = 0.68) +
    geom_text(aes(label = sprintf("%+.3f", delta)), hjust = -0.12, size = 2.3) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.2))) +
    labs(x = NULL, y = "PD gain", title = "(b) Gain from the second dimension") +
    soh_theme()

  soh_combine(p1, p2)
}

# --- F08 overall scenarios and scale sensitivity -----------------------------

soh_fig_overall <- function(overall, cfg) {
  d <- overall
  d$label <- factor(d$label, levels = d$label)
  ggplot(d, aes(label, qv, fill = scenario)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.3f", qv)), vjust = -0.6, size = 2.6) +
    scale_fill_manual(values = c(A1 = "#F4A582", A2 = "#B2182B", A3 = "#9ECAE1"),
                      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(x = NULL, y = "Overall PD") +
    soh_theme()
}

soh_fig_scale <- function(scale_df, cfg) {
  ggplot(scale_df, aes(radius, qv, colour = scenario, shape = scenario)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = c("#F4A582", "#B2182B", "#4292C6"), name = NULL) +
    scale_shape_manual(values = c(16, 17, 15), name = NULL) +
    labs(x = paste0("Largest neighbourhood radius (", cfg$data$dist_unit, ")"),
         y = "Overall PD") +
    soh_theme() +
    theme(legend.position = "bottom")
}

soh_fig_threshold <- function(thr_df, cfg) {
  ggplot(thr_df, aes(factor(sd_threshold), qv, fill = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("#F4A582", "#B2182B", "#9ECAE1"), name = NULL) +
    labs(x = "Outlier threshold (standard deviations)", y = "Overall PD") +
    soh_theme() +
    theme(legend.position = "bottom")
}
