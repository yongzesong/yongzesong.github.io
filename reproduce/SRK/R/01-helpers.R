# =============================================================================
# 01-helpers.R — logging, IO and figure helpers shared by every step
# =============================================================================

log_info <- function(fmt, ...) cat(sprintf(paste0("   ", fmt, "\n"), ...))
log_warn <- function(fmt, ...) cat(sprintf(paste0("   ! ", fmt, "\n"), ...))
log_head <- function(fmt, ...)
  cat(sprintf(paste0("\n== ", fmt, " ",
                     strrep("=", max(2, 54 - nchar(sprintf(fmt, ...)))), "\n"), ...))

has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

need_pkg <- function(...) {
  miss <- Filter(function(p) !has_pkg(p), c(...))
  if (length(miss))
    stop("missing package(s): ", paste(miss, collapse = ", "),
         "\ninstall with: install.packages(c(",
         paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
  invisible(TRUE)
}

write_result <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE)
  log_info("wrote %s (%d rows)", sub(paste0(PROJ_ROOT, "/"), "", path), nrow(df))
  invisible(df)
}
read_result <- function(path) utils::read.csv(path, check.names = FALSE)

# Steps record their own wall-clock time so env/runtimes.csv stays honest.
record_runtime <- function(step, seconds) {
  row <- data.frame(step = step, seconds = round(seconds, 1))
  old <- if (file.exists(F_RUNTIMES)) read_result(F_RUNTIMES) else NULL
  if (!is.null(old)) old <- old[old$step != step, , drop = FALSE]
  utils::write.csv(rbind(old, row), F_RUNTIMES, row.names = FALSE)
  log_info("%s took %.1f s", step, seconds)
  invisible(row)
}

# PNGs need an anti-aliasing back end; quartz on macOS, cairo elsewhere.
png_type <- function() {
  if (capabilities("aqua")) "quartz"
  else if (capabilities("cairo")) "cairo"
  else getOption("bitmapType", "Xlib")
}

save_figure <- function(expr, name, width = FIG_WIDTH_DOUBLE, height = 12,
                        devices = FIG_DEVICES) {
  for (dev in devices) {
    path <- file.path(FIG_DIR, paste0(name, ".", dev))
    if (dev == "png")
      grDevices::png(path, width = width, height = height, units = "cm",
                     res = FIG_DPI, type = png_type())
    else
      grDevices::pdf(path, width = width / 2.54, height = height / 2.54)
    on.exit(grDevices::dev.off(), add = FALSE)
    force(expr())
    grDevices::dev.off()
    on.exit()
  }
  log_info("wrote figs/%s.{%s}", name, paste(devices, collapse = ","))
  invisible(name)
}

# Point maps are drawn everywhere in this pipeline: colour by value, square
# aspect, a compact colour bar on the right.
plot_points <- function(x, y, z, main = "", pal = PAL_SEQ, pch = 15, cex = 0.5,
                        zlim = range(z, na.rm = TRUE), bar_lab = "", asp = 1,
                        mar = c(3.2, 3.4, 2.2, 5.2)) {
  cols <- grDevices::hcl.colors(100, pal, rev = TRUE)
  idx  <- pmin(100, pmax(1, 1 + floor(99 * (z - zlim[1]) / diff(zlim))))
  graphics::par(mar = mar)
  plot(x, y, col = cols[idx], pch = pch, cex = cex, asp = asp,
       xlab = "", ylab = "", main = main, cex.main = 0.95, cex.axis = 0.7)
  usr <- graphics::par("usr")
  bx  <- usr[2] + diff(usr[1:2]) * c(0.04, 0.09)
  by  <- seq(usr[3], usr[4], length.out = 101)
  graphics::rect(bx[1], by[-101], bx[2], by[-1], col = cols, border = NA, xpd = NA)
  graphics::rect(bx[1], usr[3], bx[2], usr[4], border = "grey40", xpd = NA)
  at <- seq(usr[3], usr[4], length.out = 5)
  graphics::text(bx[2], at, sprintf("%.2f", seq(zlim[1], zlim[2], length.out = 5)),
                 pos = 4, cex = 0.6, xpd = NA)
  if (nzchar(bar_lab))
    graphics::mtext(bar_lab, side = 4, line = 3.6, cex = 0.65)
  invisible(NULL)
}

# Gridded fields (the simulation) draw better as a raster than as points.
plot_grid_field <- function(x, y, z, main = "", pal = PAL_SEQ,
                            zlim = range(z, na.rm = TRUE), bar_lab = "",
                            mar = c(2.6, 2.8, 1.8, 4.4)) {
  ux <- sort(unique(x)); uy <- sort(unique(y))
  m  <- matrix(NA_real_, length(ux), length(uy))
  m[cbind(match(x, ux), match(y, uy))] <- z
  cols <- grDevices::hcl.colors(100, pal, rev = TRUE)
  graphics::par(mar = mar)
  graphics::image(ux, uy, m, col = cols, zlim = zlim, useRaster = TRUE,
                  xlab = "", ylab = "", main = main, cex.main = 0.95,
                  cex.axis = 0.7)
  graphics::box()
  usr <- graphics::par("usr")
  bx  <- usr[2] + diff(usr[1:2]) * c(0.04, 0.09)
  by  <- seq(usr[3], usr[4], length.out = 101)
  graphics::rect(bx[1], by[-101], bx[2], by[-1], col = cols, border = NA, xpd = NA)
  graphics::rect(bx[1], usr[3], bx[2], usr[4], border = "grey40", xpd = NA)
  at <- seq(usr[3], usr[4], length.out = 5)
  graphics::text(bx[2], at, sprintf("%.2f", seq(zlim[1], zlim[2], length.out = 5)),
                 pos = 4, cex = 0.6, xpd = NA)
  if (nzchar(bar_lab)) graphics::mtext(bar_lab, side = 4, line = 3.0, cex = 0.65)
  invisible(NULL)
}

write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID),
               paste("Seed:", SEED), ""), con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}
