# =============================================================================
# fn-config.R — configuration loader and small shared utilities
# All pipeline scripts source this file first and run from the project root.
# =============================================================================

load_config <- function(path = "config/project-config.yaml") {
  if (!file.exists(path)) {
    stop("Config not found at '", path,
         "'. Run scripts from the project root, e.g. Rscript R/03-geocomplexity.R")
  }
  yaml::read_yaml(path)
}

get_cores <- function(cfg) {
  if (identical(cfg$params$cores, "auto")) {
    max(1L, parallel::detectCores() - 1L)
  } else {
    as.integer(cfg$params$cores)
  }
}

ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}

read_analysis_table <- function(cfg) {
  path <- cfg$data$analysis_table
  if (!file.exists(path)) {
    stop("Analysis table not found at '", path,
         "'. Run R/01-prepare-data.R first.")
  }
  read.csv(path)
}

# Column-name helpers -----------------------------------------------------

gc_names_of <- function(cfg) paste0("gc_", cfg$predictors$x_vars)

all_vars_of <- function(cfg) c(cfg$predictors$x_vars, gc_names_of(cfg))

# Named label vectors for figures: x labels from config, GC labels derived.
get_labels <- function(cfg) {
  x_vars   <- cfg$predictors$x_vars
  x_labels <- unlist(cfg$predictors$labels)[x_vars]
  gc_labels <- setNames(paste0("GC(", x_labels, ")"), gc_names_of(cfg))
  c(x_labels, gc_labels)
}

# Read the response (with coordinates) produced by 02-response-intake.R
read_response <- function(cfg) {
  path <- "results/step02-response.csv"
  if (!file.exists(path)) {
    stop("Response file not found at '", path,
         "'. Run R/02-response-intake.R first.")
  }
  read.csv(path)
}

# Read the local extent estimated by 04-local-range.R
read_local_range <- function(cfg) {
  path <- "results/step04-local-range.csv"
  if (!file.exists(path)) {
    stop("Local range file not found at '", path,
         "'. Run R/04-local-range.R first.")
  }
  read.csv(path)
}

# Assemble the modelling frame: analysis table + response + geocomplexity.
# Every script from Step 05 onward starts from this call, so any script can
# be re-run in isolation without recomputing upstream steps.
build_model_frame <- function(cfg) {
  d  <- read_analysis_table(cfg)
  rz <- read_response(cfg)
  d[[cfg$response$response_name]] <- rz[[cfg$response$response_name]]

  gc_path <- "results/step03-geocomplexity.csv"
  if (file.exists(gc_path)) {
    gc <- read.csv(gc_path)
    for (v in gc_names_of(cfg)) d[[v]] <- gc[[v]]
  }
  d
}
