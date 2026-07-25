# =============================================================================
# 01_prepare_data.R — assemble the two tables the SOH model needs
# =============================================================================
# The SOH model requires exactly two tables on a common projected CRS.
#
#   points  one row per ANALYSIS UNIT, the support of the response variable.
#           Columns: id, x, y, response, and one column per covariate.
#           These are the units whose heterogeneity is being explained.
#
#   grids   one row per FINE GRID CELL covering the study area and a margin
#           beyond it. Columns: id, x, y, and one column per covariate.
#           These cells are the second dimension — the unsampled locations
#           whose local anomalies become SOP variables.
#
# The grid must be finer than the analysis units, and must extend past the
# study boundary by at least the largest buffer radius. Otherwise the
# neighbourhoods of edge units are truncated and their SOPs are biased toward
# zero. Song (2022) makes the same point about second-dimension data.
#
# In "user" mode this script is where you write your own ingestion code. The
# blocks marked ADAPT are the only places that change.
# =============================================================================

# --- bootstrap (identical in every pipeline script) --------------------------
if (!exists("cfg")) {
  .a <- commandArgs(trailingOnly = FALSE)
  .p <- sub("^--file=", "", .a[grep("^--file=", .a)])
  .c <- c(if (length(.p)) file.path(dirname(.p), "00_setup.R"),
          "00_setup.R", "code/00_setup.R", "../code/00_setup.R")
  source(.c[file.exists(.c)][1])
}

soh_step("01 PREPARE DATA")

if (identical(cfg$data$mode, "demo")) {

  soh_log("mode: demo — generating a synthetic dataset with known anomalies")
  demo <- soh_make_demo_data(cfg)
  points <- demo$points
  grids  <- demo$grids
  saveRDS(demo$truth, soh_cache("demo_truth"))
  utils::write.csv(points, soh_path("data", "demo", "points.csv"), row.names = FALSE)
  utils::write.csv(grids,  soh_path("data", "demo", "grids.csv"),  row.names = FALSE)
  soh_log("demo tables written to data/demo/")

} else {

  # --- ADAPT 1: read your own tables -----------------------------------------
  soh_log("mode: user — reading data/processed/")
  points <- utils::read.csv(soh_path("data", "processed", cfg$data$points_file))
  grids  <- utils::read.csv(soh_path("data", "processed", cfg$data$grids_file))

  # --- ADAPT 2: rename to the internal schema --------------------------------
  ren <- function(d, has_response) {
    names(d)[match(cfg$data$id_col, names(d))] <- "id"
    names(d)[match(cfg$data$x_col,  names(d))] <- "x"
    names(d)[match(cfg$data$y_col,  names(d))] <- "y"
    if (has_response) {
      names(d)[match(cfg$data$response_col, names(d))] <- "response"
    }
    d
  }
  points <- ren(points, TRUE)
  grids  <- ren(grids, FALSE)
}

# --- validation --------------------------------------------------------------
# These checks catch the failures that silently produce meaningless PD values.

codes <- cfg$variables$codes

soh_validate <- function(points, grids, cfg) {
  problems <- character()

  need_p <- c("id", "x", "y", "response", codes)
  need_g <- c("id", "x", "y", codes)
  miss_p <- setdiff(need_p, names(points))
  miss_g <- setdiff(need_g, names(grids))
  if (length(miss_p)) problems <- c(problems,
    paste("points is missing columns:", paste(miss_p, collapse = ", ")))
  if (length(miss_g)) problems <- c(problems,
    paste("grids is missing columns:", paste(miss_g, collapse = ", ")))
  if (length(problems)) stop(paste(problems, collapse = "\n"))

  if (any(!is.finite(points$response))) problems <- c(problems,
    "response contains non-finite values")

  # grid must be finer than the analysis units
  if (nrow(grids) < nrow(points)) problems <- c(problems,
    "grids has fewer rows than points; the second dimension needs a finer support")

  # grid must cover the units plus a margin of the largest buffer
  rmax <- max(cfg$sop$buffers)
  margin <- c(min(points$x) - min(grids$x), max(grids$x) - max(points$x),
              min(points$y) - min(grids$y), max(grids$y) - max(points$y))
  if (any(margin < rmax * 0.5)) problems <- c(problems, sprintf(
    "grid margin around the analysis units is %.1f %s, less than half the largest buffer (%.1f %s); edge units will have truncated neighbourhoods",
    min(margin), cfg$data$dist_unit, rmax, cfg$data$dist_unit))

  # coordinates should be projected, not degrees
  if (max(abs(points$x)) <= 180 && max(abs(points$y)) <= 90 &&
      cfg$data$dist_unit != "degree") problems <- c(problems,
    "coordinates look like longitude and latitude; reproject to a metric CRS before running SOH")

  const <- codes[vapply(codes, function(v) stats::sd(grids[[v]], na.rm = TRUE) == 0,
                        logical(1))]
  if (length(const)) problems <- c(problems,
    paste("constant covariates on the grid produce empty SOPs:",
          paste(const, collapse = ", ")))

  problems
}

problems <- soh_validate(points, grids, cfg)
if (length(problems)) {
  for (p in problems) soh_log(p, level = "WARN ")
  soh_log("Review the warnings above before trusting any PD value.", level = "WARN ")
} else {
  soh_log("validation passed")
}

# --- descriptive summary -----------------------------------------------------

desc <- data.frame(
  variable = c("response", codes),
  label = c(cfg$study$response_label, unname(cfg$variables$labels[codes])),
  category = c(NA, unname(cfg$variables$category[codes])),
  n_units = nrow(points),
  mean = vapply(c("response", codes), function(v) mean(points[[v]], na.rm = TRUE), numeric(1)),
  sd = vapply(c("response", codes), function(v) stats::sd(points[[v]], na.rm = TRUE), numeric(1)),
  min = vapply(c("response", codes), function(v) min(points[[v]], na.rm = TRUE), numeric(1)),
  max = vapply(c("response", codes), function(v) max(points[[v]], na.rm = TRUE), numeric(1)),
  stringsAsFactors = FALSE
)
soh_write_table(desc, "T01_data_summary")

soh_log("analysis units: ", nrow(points), "   grid cells: ", nrow(grids),
        "   covariates: ", length(codes))

print(soh_buffer_diagnostic(points, cfg))

saveRDS(list(points = points, grids = grids), soh_cache("data"))
soh_log("cached: data/interim/data.rds")
