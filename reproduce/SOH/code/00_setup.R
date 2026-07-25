# =============================================================================
# 00_setup.R — sourced first by every pipeline script
# =============================================================================
# Loads the configuration, the function library, and the packages the pipeline
# needs. Nothing here is study specific; edit config/config.R instead.
# =============================================================================

soh_bootstrap <- function(script_dir = NULL) {
  # locate the function library relative to this file
  here <- script_dir
  if (is.null(here)) {
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", args[grep("^--file=", args)])
    here <- if (length(f)) dirname(normalizePath(f)) else getwd()
  }
  if (!file.exists(file.path(here, "R", "fn_utils.R"))) {
    # allow running from the project root or from code
    cand <- c(file.path(here, "code"), dirname(here))
    hit <- cand[file.exists(file.path(cand, "R", "fn_utils.R"))]
    if (!length(hit)) stop("Cannot locate code/R. Set the working directory ",
                           "to the project folder.")
    here <- hit[1]
  }
  for (f in c("fn_utils.R", "fn_sop.R", "fn_pd.R", "fn_experiments.R", "fn_plots.R")) {
    source(file.path(here, "R", f))
  }
  invisible(here)
}

SOH_CODE_DIR <- soh_bootstrap()

# --- packages ----------------------------------------------------------------

soh_required <- c("rpart", "ggplot2")
soh_optional <- c("patchwork", "spdep", "sf", "GD")

soh_check_packages <- function() {
  missing <- soh_required[!vapply(soh_required, requireNamespace, logical(1),
                                  quietly = TRUE)]
  if (length(missing)) {
    stop("Install the required packages first:\n  install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))")
  }
  absent <- soh_optional[!vapply(soh_optional, requireNamespace, logical(1),
                                 quietly = TRUE)]
  if (length(absent)) {
    message("Optional packages not installed (features degrade gracefully): ",
            paste(absent, collapse = ", "))
    message("  patchwork  multi-panel figures      spdep  Moran's I")
    message("  sf         shapefile study areas    GD     cross-check of the q statistic")
  }
  invisible(TRUE)
}

suppressPackageStartupMessages({
  library(rpart)
  library(ggplot2)
})

cfg <- soh_config()
set.seed(cfg$run$seed)
soh_check_packages()

dir.create(soh_path("03_results", "logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(soh_path("03_results", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(soh_path("03_results", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(soh_path("data", "interim"), showWarnings = FALSE, recursive = TRUE)
