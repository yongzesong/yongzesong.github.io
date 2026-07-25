# =============================================================================
# 01-helpers.R — logging, IO, spatial weights and figure helpers
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
    path <- file.path(FIG_DIR, paste0(name, ".", dev))
    ggplot2::ggsave(path, plot, width = width, height = height, units = "cm",
                    dpi = FIG_DPI, device = dev)
  }
  log_info("wrote figs/%s.{%s}", name, paste(devices, collapse = ","))
  invisible(name)
}

r2_score   <- function(o, p) 1 - sum((o - p)^2) / sum((o - mean(o))^2)
rmse_score <- function(o, p) sqrt(mean((o - p)^2))

#' Read the study layer and return an sf object.
read_layer <- function(path = INPUT_FILE) {
  need_pkg("sf", "reading the study layer")
  sf::read_sf(file.path(PROJ_ROOT, path))
}

#' The explanatory variables: everything except the response and the geometry.
predictor_names <- function(x) {
  if (!is.null(PREDICTORS)) return(PREDICTORS)
  setdiff(names(sf::st_drop_geometry(x)), RESPONSE)
}

#' Build the spatial weight matrix the whole analysis rests on.
build_weights <- function(x) {
  need_pkg("sdsfun", "spatial weights")
  if (identical(WEIGHTS_TYPE, "knn")) {
    w <- sdsfun::spdep_contiguity_swm(x, k = WEIGHTS_K, style = WEIGHTS_STYLE)
    log_info("weights: %d-nearest-neighbour, style '%s'", WEIGHTS_K, WEIGHTS_STYLE)
  } else {
    w <- sdsfun::spdep_contiguity_swm(x, queen = WEIGHTS_QUEEN, style = WEIGHTS_STYLE)
    log_info("weights: %s contiguity, style '%s'",
             ifelse(WEIGHTS_QUEEN, "queen", "rook"), WEIGHTS_STYLE)
  }
  w
}

write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID), paste("Seed:", SEED), ""), con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}
