# =============================================================================
# 01-helpers.R — logging, IO, cross-validation, error metrics, base-R figures
#
# Nothing here is specific to Zn; every function is driven by the constants in
# config/project-config.R. The figure helpers exist because the whole tutorial
# is drawn with base graphics: one call has to emit a PNG and a matching
# vector PDF, and maps have to be square with a legible colour bar.
# =============================================================================

# -- logging ------------------------------------------------------------------
log_info <- function(fmt, ...) cat(sprintf(paste0("   ", fmt, "\n"), ...))
log_warn <- function(fmt, ...) cat(sprintf(paste0("   ! ", fmt, "\n"), ...))
log_head <- function(fmt, ...)
  cat(sprintf(paste0("\n== ", fmt, " ",
                     strrep("=", max(2, 54 - nchar(sprintf(fmt, ...)))), "\n"), ...))

has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

need_pkg <- function(p, what) {
  if (!has_pkg(p)) stop(sprintf("package '%s' is required for %s", p, what))
  invisible(TRUE)
}

# Wall-clock timer used by run-all.R to fill env/runtimes.csv.
timeit <- function(label, expr) {
  t0 <- Sys.time()
  val <- force(expr)
  log_info("%s took %.1f s", label, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  invisible(val)
}


# -- IO -----------------------------------------------------------------------
write_result <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE)
  log_info("wrote %s (%d rows)", sub(paste0(PROJ_ROOT, "/"), "", path), nrow(df))
  invisible(df)
}
read_result <- function(path) utils::read.csv(path, check.names = FALSE)

#' Record the session. The pipeline calls every package through `::` rather
#' than library(), so sessionInfo() would list almost nothing; the versions
#' that actually produced the results are therefore written out explicitly.
PKGS_USED <- c("geosimilarity", "car")

write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID),
               paste("Method :", METHOD_PAPER),
               paste("Seed   :", SEED),
               paste("Run    :", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
               "", "Packages used by the pipeline:"), con)
  writeLines(vapply(PKGS_USED, function(p)
    sprintf("  %-14s %s", p,
            if (has_pkg(p)) as.character(utils::packageVersion(p)) else "not installed"),
    character(1)), con)
  writeLines("", con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}


# -- error metrics (paper Eq. 8 and Eq. 13) -----------------------------------
rmse_score <- function(o, p) sqrt(mean((o - p)^2))
mae_score  <- function(o, p) mean(abs(o - p))


# -- cross-validation ---------------------------------------------------------
#' Repeated random 50/50 splits, generated once and shared by every model.
#'
#' Each repeat gets its own seed (SEED + i) so the split for repeat i is the
#' same no matter which model asks for it, and re-running one model later
#' reproduces exactly the splits the others saw. The size rule
#' floor(prop * n) matches gos_bestkappa()'s internal splitter, so step 30 and
#' step 50 partition the data the same way.
cv_splits <- function(n, nrepeat, prop = 0.5, seed = SEED) {
  if (n < 10) stop("cv_splits: need at least 10 observations, got ", n)
  ntrain <- floor(prop * n)
  if (ntrain < 2 || ntrain > n - 2)
    stop("cv_splits: prop = ", prop, " leaves an unusable split for n = ", n)
  lapply(seq_len(nrepeat), function(i) {
    set.seed(seed + i)
    sort(sample.int(n, ntrain))
  })
}


# -- multicollinearity --------------------------------------------------------
#' Variance inflation factors for a set of covariates.
#'
#' VIF_i = 1 / (1 - R2_i) from regressing covariate i on all the others. For a
#' model with continuous main effects only this is exactly what car::vif()
#' returns, and it keeps the pipeline free of the car dependency chain; when
#' car is installed, vif_check() below confirms the two agree.
vif_lm <- function(data, vars) {
  if (length(vars) < 2) return(stats::setNames(rep(1, length(vars)), vars))
  vapply(vars, function(v) {
    f <- stats::as.formula(paste(v, "~", paste(setdiff(vars, v), collapse = " + ")))
    r2 <- summary(stats::lm(f, data = data))$r.squared
    if (r2 >= 1 - 1e-12) Inf else 1 / (1 - r2)
  }, numeric(1))
}

#' Drop the worst offender until every VIF is below the threshold (paper
#' Sect. 2.2.1). Returns the surviving variables plus the drop history.
vif_prune <- function(data, vars, threshold = VIF_THRESHOLD) {
  keep <- vars; dropped <- character(0); history <- list()
  repeat {
    v <- vif_lm(data, keep)
    history[[length(history) + 1]] <- v
    if (length(keep) <= 2 || max(v) < threshold) break
    worst <- names(which.max(v))
    log_info("VIF %.2f > %.1f — dropping %s", max(v), threshold, worst)
    dropped <- c(dropped, worst); keep <- setdiff(keep, worst)
  }
  list(keep = keep, dropped = dropped, vif = history[[length(history)]],
       history = history)
}


# =============================================================================
# Figures — base R only, PNG + PDF from a single call
# =============================================================================

#' Draw the same figure to every device in FIG_DEVICES.
#'
#' `draw` is a zero-argument function holding the plotting calls. Sizes are in
#' centimetres so the PNG (at FIG_DPI) and the PDF come out the same shape.
#' par() is reset for each device, so `draw` starts from a known state.
#' The PNG back end is left to R's default (`quartz` on this machine): the
#' cairo build here cannot load its X11 dependencies, and forcing
#' `type = "cairo"` silently produces no file at all. The check at the end
#' turns that class of failure into an error instead of a missing figure.
draw_figure <- function(name, draw, width = FIG_W_DOUBLE, height = 9) {
  for (dev in FIG_DEVICES) {
    path <- file.path(FIG_DIR, paste0(name, ".", dev))
    if (file.exists(path)) unlink(path)
    if (identical(dev, "png")) {
      grDevices::png(path, width = width, height = height, units = "cm",
                     res = FIG_DPI, bg = "white")
    } else {
      grDevices::pdf(path, width = width / 2.54, height = height / 2.54,
                     bg = "white", useDingbats = FALSE)
    }
    op <- graphics::par(no.readonly = TRUE)
    tryCatch(draw(), finally = { graphics::par(op); grDevices::dev.off() })
    if (!file.exists(path) || file.size(path) == 0)
      stop("draw_figure: the ", dev, " device produced no output for ", name)
  }
  log_info("wrote figs/%s.{%s}", name, paste(FIG_DEVICES, collapse = ","))
  invisible(name)
}

#' A reversed sequential ramp from base R's hcl.colors().
ramp <- function(palette, n = PAL_N) rev(grDevices::hcl.colors(n, palette))

# Map axis titles. Latitudes in the study area are southern, so the tick
# labels are already negative and the axis needs no hemisphere letter.
LON_LAB <- expression("Longitude (" * degree * "E)")
LAT_LAB <- expression("Latitude (" * degree * ")")

#' Reshape scattered regular-grid values into the matrix image() wants.
#'
#' The prediction grid is a 1 km lattice clipped to the study area, so roughly
#' a quarter of the bounding box has no cell; those stay NA and are simply not
#' painted. Coordinates are rounded before matching because they arrive as
#' repeated floating-point sums, and the axes are then rebuilt as exact
#' arithmetic sequences: image(useRaster = TRUE) rejects an axis whose steps
#' differ even in the sixth decimal, which is all the rounding leaves behind.
grid_matrix <- function(x, y, z, digits = 5) {
  regularise <- function(u) {
    d <- diff(u)
    if (length(d) > 1 && diff(range(d)) < 0.01 * stats::median(d))
      seq(u[1], u[length(u)], length.out = length(u)) else u
  }
  xr <- round(x, digits); yr <- round(y, digits)
  ux <- sort(unique(xr)); uy <- sort(unique(yr))
  m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
  m[cbind(match(xr, ux), match(yr, uy))] <- z
  list(x = regularise(ux), y = regularise(uy), z = m)
}

#' One square tile map of a gridded variable.
#'
#' Values outside `zlim` are clipped rather than dropped, which keeps a handful
#' of extreme uncertainty cells from flattening the whole colour scale. The
#' aspect ratio corrects longitude for latitude so the study area is not
#' stretched, and pty = "s" keeps the panel square as the house style requires.
tile_map <- function(x, y, z, pal, zlim = NULL, main = NULL, xlab = NULL,
                     ylab = NULL, axes = TRUE, n = PAL_N, cex.main = 1) {
  if (is.null(zlim)) zlim <- range(z, na.rm = TRUE)
  zc <- pmin(pmax(z, zlim[1]), zlim[2])
  g  <- grid_matrix(x, y, zc)
  cols <- ramp(pal, n)
  graphics::par(pty = "s")
  graphics::plot(range(g$x), range(g$y), type = "n", asp = 1 / cos(mean(y) * pi / 180),
                 xlab = "", ylab = "", axes = FALSE, xaxs = "i", yaxs = "i")
  graphics::image(g$x, g$y, g$z, col = cols, zlim = zlim, add = TRUE, useRaster = TRUE)
  if (axes) {
    graphics::axis(1, cex.axis = 0.75, tcl = -0.25, mgp = c(2, 0.4, 0))
    graphics::axis(2, las = 1, cex.axis = 0.75, tcl = -0.25, mgp = c(2, 0.5, 0))
  }
  graphics::box()
  if (!is.null(xlab)) graphics::mtext(xlab, side = 1, line = 1.6, cex = 0.75)
  if (!is.null(ylab)) graphics::mtext(ylab, side = 2, line = 2.0, cex = 0.75)
  if (!is.null(main)) graphics::mtext(main, side = 3, line = 0.4, cex = cex.main, font = 2)
  invisible(zlim)
}

#' A horizontal colour bar, drawn in its own (thin) panel of the layout.
#'
#' `clipped` adds the >= sign that tells the reader the top class is open.
color_bar <- function(zlim, pal, label = NULL, n = PAL_N, clipped = FALSE,
                      digits = 2, cex = 0.7) {
  graphics::par(pty = "m", mar = c(2.4, 2.0, 0.6, 2.0))
  graphics::plot(zlim, c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
                 xaxs = "i", yaxs = "i")
  xs <- seq(zlim[1], zlim[2], length.out = n + 1)
  graphics::rect(xs[-(n + 1)], 0, xs[-1], 1, col = ramp(pal, n), border = NA)
  graphics::box()
  at <- pretty(zlim, 4); at <- at[at >= zlim[1] & at <= zlim[2]]
  lab <- as.list(formatC(at, format = "f", digits = digits))
  # plotmath rather than a literal "≥": the PDF device would otherwise
  # transliterate U+2265 and warn on every figure.
  if (clipped)
    lab[[length(lab)]] <- bquote(phantom() >= .(lab[[length(lab)]]))
  graphics::axis(1, at = at, labels = as.expression(lab), cex.axis = cex,
                 tcl = -0.2, mgp = c(1, 0.25, 0))
  if (!is.null(label))
    graphics::mtext(label, side = 1, line = 1.35, cex = cex)
  invisible(NULL)
}

#' Panel tag "(a)", "(b)", ... in the top-left of the current plot.
panel_tag <- function(tag, adj = -0.02, line = 0.4, cex = 0.85)
  graphics::mtext(tag, side = 3, line = line, adj = adj, font = 2, cex = cex)
