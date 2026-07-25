# =============================================================================
# 01-helpers.R — logging, IO and figure helpers shared by every step
# =============================================================================

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

write_result <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE)
  log_info("wrote %s (%d rows)", sub(paste0(PROJ_ROOT, "/"), "", path), nrow(df))
  invisible(df)
}
read_result <- function(path) utils::read.csv(path, check.names = FALSE)

save_figure <- function(plot, name, width = FIG_WIDTH_DOUBLE, height = 12,
                        devices = FIG_DEVICES) {
  for (dev in devices) {
    ggplot2::ggsave(file.path(FIG_DIR, paste0(name, ".", dev)), plot,
                    width = width, height = height, units = "cm",
                    dpi = FIG_DPI, device = dev)
  }
  log_info("wrote figs/%s.{%s}", name, paste(devices, collapse = ","))
  invisible(name)
}

#' The analysis table: the authors' access, accessibility and delta values.
#' The block identifier is an 11-digit code and must be read as text: parsed as
#' a number it loses precision and stops matching the geometry.
read_analysis <- function() {
  cc <- c("character"); names(cc) <- ID_COL
  utils::read.csv(file.path(PROJ_ROOT, ANALYSIS_FILE),
                  colClasses = cc, check.names = FALSE)
}

#' Column names for a given walking time.
col_access        <- function(t) paste0(ACCESS_PREFIX, t)
col_accessibility <- function(t) paste0(ACCESSIBILITY_PREFIX, t)
col_delta         <- function(t) paste0(DELTA_PREFIX, t)

#' The explanatory variables, raw and contextualised, with readable names.
explanatory_frame <- function(d) {
  x <- d[, c(REGION_COL, VARS_RAW, VARS_CTX)]
  names(x) <- c("Region", VARS_LABEL, paste("Contextualised", tolower(VARS_LABEL)))
  x
}

write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID), paste("Seed:", SEED), ""), con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}
