# =============================================================================
# 01-helpers.R — input validation, coordinates, accuracy metrics, I/O
# =============================================================================

# -- Input validation ---------------------------------------------------------

#' Check that a data frame carries what the pipeline needs.
#'
#' Fails early and says which column is missing, because a silent NA in the
#' coordinate columns produces a Moran's I that looks plausible and is wrong.
validate_input <- function(df, response = RESPONSE,
                           lon = "lon", lat = "lat", verbose = TRUE) {
  needed <- c(lon, lat, response)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols)) {
    stop("Input is missing required column(s): ", paste(missing_cols, collapse = ", "),
         "\nPresent columns: ", paste(names(df), collapse = ", "))
  }
  if (!is.numeric(df[[response]])) {
    stop("Response column '", response, "' is not numeric.")
  }
  n_before <- nrow(df)
  df <- df[stats::complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  if (verbose && nrow(df) < n_before) {
    log_warn("dropped %d row(s) with missing coordinates or response",
             n_before - nrow(df))
  }
  dup <- duplicated(df[, c(lon, lat)])
  if (any(dup)) {
    log_warn("%d duplicate coordinate(s) found; keeping the first of each",
             sum(dup))
    df <- df[!dup, , drop = FALSE]
  }
  if (nrow(df) < 30) {
    stop("Only ", nrow(df), " usable rows. Spatial metrics on fewer than ~30 ",
         "points are not interpretable.")
  }
  df
}

#' Resolve which columns act as predictors.
resolve_predictors <- function(df, response = RESPONSE,
                               predictors = PREDICTORS,
                               drop_cols = DROP_COLS) {
  reserved <- c("lon", "lat", response, "residual", "prediction", "split", drop_cols)
  if (is.null(predictors)) {
    predictors <- setdiff(names(df), reserved)
  }
  predictors <- setdiff(predictors, reserved)
  numeric_ok <- vapply(df[predictors], is.numeric, logical(1))
  if (any(!numeric_ok)) {
    log_warn("dropping non-numeric predictor(s): %s",
             paste(predictors[!numeric_ok], collapse = ", "))
    predictors <- predictors[numeric_ok]
  }
  if (!length(predictors)) stop("No usable predictors remain.")
  predictors
}

# -- Coordinates --------------------------------------------------------------

#' Project lon/lat to a metric CRS and return the coordinate matrix.
#'
#' Distance-based neighbours built on degrees are distorted by latitude, which
#' is why PROJECTED_CRS must be in metres.
projected_coords <- function(df, data_crs = DATA_CRS, projected_crs = PROJECTED_CRS,
                             lon = "lon", lat = "lat") {
  pts <- sf::st_as_sf(df, coords = c(lon, lat), crs = data_crs, remove = FALSE)
  pts <- sf::st_transform(pts, projected_crs)
  units_crs <- sf::st_crs(projected_crs)$units
  if (!is.null(units_crs) && !units_crs %in% c("m", "metre", "meter")) {
    log_warn("PROJECTED_CRS units are '%s', not metres; neighbour distances may be wrong.",
             units_crs)
  }
  sf::st_coordinates(pts)
}

# -- Accuracy metrics ---------------------------------------------------------

#' R2, RMSE and MAE for one model.
#'
#' R2 is the squared Pearson correlation between observed and predicted, which
#' is what caret reports and therefore what both source papers report.
accuracy_metrics <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  observed <- observed[keep]; predicted <- predicted[keep]
  resid <- observed - predicted
  data.frame(
    n     = length(observed),
    R2    = stats::cor(observed, predicted)^2,
    RMSE  = sqrt(mean(resid^2)),
    MAE   = mean(abs(resid))
  )
}

# -- I/O ----------------------------------------------------------------------

write_result <- function(x, path, digits = 6) {
  num <- vapply(x, is.numeric, logical(1))
  x[num] <- lapply(x[num], function(v) round(v, digits))
  utils::write.csv(x, path, row.names = FALSE)
  log_info("wrote %s (%d rows)", sub(paste0(PROJ_ROOT, "/"), "", path, fixed = TRUE), nrow(x))
  invisible(path)
}

read_result <- function(path) {
  if (!file.exists(path)) {
    stop("Expected file not found: ", path,
         "\nRun the earlier pipeline step first, or run: Rscript run-all.R")
  }
  utils::read.csv(path, check.names = FALSE)
}

#' Save a figure to every device listed in FIG_DEVICES.
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

#' Record the exact environment a set of results came from.
write_session_info <- function(path = file.path(ENV_DIR, "session-info.txt")) {
  con <- file(path, open = "wt")
  on.exit(close(con))
  writeLines(c(paste("Project:", PROJECT_ID),
               paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
               paste("Seed:", SEED), ""), con)
  utils::capture.output(utils::sessionInfo(), file = con, append = TRUE)
  invisible(path)
}
