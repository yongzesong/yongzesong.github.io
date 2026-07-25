# =============================================================================
# 01-helpers.R — small shared utilities: logging, package guard, IO, figures
# =============================================================================

log_info <- function(fmt, ...) cat(sprintf(paste0("   ", fmt, "\n"), ...))
log_warn <- function(fmt, ...) cat(sprintf(paste0("   ! ", fmt, "\n"), ...))
log_head <- function(fmt, ...)
  cat(sprintf(paste0("\n== ", fmt, " ",
                     strrep("=", max(2, 56 - nchar(sprintf(fmt, ...)))), "\n"), ...))

has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# Require the GD package (the OPGD reference implementation).
need_GD <- function() {
  if (!has_pkg("GD"))
    stop("The 'GD' package is required. Install it with install.packages(\"GD\").")
  suppressMessages(library(GD))
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
    path <- file.path(FIG_DIR, paste0(name, ".", dev))
    ggplot2::ggsave(path, plot, width = width, height = height, units = "cm",
                    dpi = FIG_DPI, device = dev)
  }
  log_info("wrote figs/%s.{%s}", name, paste(devices, collapse = ","))
  invisible(name)
}

# Load the analysis table: a built-in GD dataset, or the user's CSV.
load_dataset <- function(name = DATASET, input = INPUT_FILE) {
  if (!is.null(input)) {
    if (!file.exists(input)) stop("INPUT_FILE not found: ", input)
    return(utils::read.csv(input))
  }
  need_GD()
  e <- new.env()
  utils::data(list = name, package = "GD", envir = e)
  get(name, envir = e)
}

write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID),
               paste("Seed:", SEED), ""), con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}
