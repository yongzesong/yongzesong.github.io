# =============================================================================
# 00-config.R — paths, seed, package loading
# =============================================================================
# Source this first in every script. It reads config/project-config.R and
# defines the globals the rest of the pipeline assumes exist.
# Do not put project settings here; put them in config/project-config.R.
# =============================================================================

# -- Project root -------------------------------------------------------------
# Found by walking up from the working directory and from the running script
# until config/project-config.R appears. The project therefore runs from any
# working directory, and keeps running after the folder is copied or renamed.

if (!exists("PROJ_ROOT")) {
  .find_root <- function(start) {
    d <- normalizePath(start, mustWork = FALSE)
    for (i in 1:5) {
      if (file.exists(file.path(d, "config", "project-config.R"))) return(d)
      parent <- dirname(d)
      if (identical(parent, d)) break
      d <- parent
    }
    NULL
  }

  .script <- sub("^--file=", "",
                 commandArgs(trailingOnly = FALSE)[
                   grep("^--file=", commandArgs(trailingOnly = FALSE))])
  .candidates <- c(getwd(), if (length(.script)) dirname(normalizePath(.script)))

  PROJ_ROOT <- NULL
  for (cand in .candidates) {
    PROJ_ROOT <- .find_root(cand)
    if (!is.null(PROJ_ROOT)) break
  }
  if (is.null(PROJ_ROOT)) {
    stop("Could not locate the project root (the folder containing ",
         "config/project-config.R). Run from the project folder, or set ",
         "PROJ_ROOT before sourcing R/00-config.R.")
  }
}

CONFIG_FILE <- file.path(PROJ_ROOT, "config", "project-config.R")
if (!file.exists(CONFIG_FILE)) {
  stop("config/project-config.R not found under ", PROJ_ROOT,
       ". Run scripts from the project root, or set PROJ_ROOT before sourcing.")
}
source(CONFIG_FILE)

# -- Directories --------------------------------------------------------------

R_DIR      <- file.path(PROJ_ROOT, "R")
RAW_DIR    <- file.path(PROJ_ROOT, "data", "raw")
DERIV_DIR  <- file.path(PROJ_ROOT, "data", "derived")
REF_DIR    <- file.path(PROJ_ROOT, "data", "reference")
RES_DIR    <- file.path(PROJ_ROOT, "results")
TAB_DIR    <- file.path(PROJ_ROOT, "tables")
FIG_DIR    <- file.path(PROJ_ROOT, "figs")
ENV_DIR    <- file.path(PROJ_ROOT, "env")

for (d in c(DERIV_DIR, RES_DIR, TAB_DIR, FIG_DIR, ENV_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# -- Reproducibility ----------------------------------------------------------

set.seed(SEED)
options(stringsAsFactors = FALSE)

# -- Package loading ----------------------------------------------------------
# Required packages stop the run. Optional packages only disable the module or
# the model that needs them, and the pipeline reports what was skipped.

PKG_REQUIRED <- c("sf", "spdep", "rpart")
PKG_OPTIONAL <- c("geocomplexity", "ggplot2", "ranger", "xgboost", "mgcv",
                  "Cubist", "FNN", "pls", "earth", "kernlab", "gbm")

.missing_required <- PKG_REQUIRED[!vapply(PKG_REQUIRED, requireNamespace,
                                          logical(1), quietly = TRUE)]
if (length(.missing_required)) {
  stop("Missing required packages: ", paste(.missing_required, collapse = ", "),
       "\nInstall with: install.packages(c(",
       paste0('"', .missing_required, '"', collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(rpart)
})

has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

PKG_AVAILABLE <- vapply(PKG_OPTIONAL, has_pkg, logical(1))

# -- Small logging helpers ----------------------------------------------------

log_step <- function(...) {
  cat("\n== ", sprintf(...), " ", strrep("=", max(0, 60 - nchar(sprintf(...)))),
      "\n", sep = "")
}
log_info <- function(...) cat("   ", sprintf(...), "\n", sep = "")
log_warn <- function(...) cat("   ! ", sprintf(...), "\n", sep = "")

# -- Canonical file names -----------------------------------------------------
# Every step reads and writes through these so the pipeline stays wired
# together when a step is re-run on its own.

F_ANALYSIS_DATA <- file.path(DERIV_DIR, "analysis-data.csv")
F_SPLIT         <- file.path(DERIV_DIR, "train-test-split.csv")
F_PREDICTIONS   <- file.path(DERIV_DIR, "test-predictions.csv")
F_RESIDUALS     <- file.path(DERIV_DIR, "test-residuals.csv")
F_ACCURACY      <- file.path(RES_DIR,   "accuracy-metrics.csv")
F_DSI           <- file.path(RES_DIR,   "dsi-metrics.csv")
F_DG_POINT      <- file.path(RES_DIR,   "dg-pointwise.csv")
F_DG_SUMMARY    <- file.path(RES_DIR,   "dg-summary.csv")
F_DG_OPTIMAL    <- file.path(RES_DIR,   "dg-optimal-model.csv")
F_SENSITIVITY   <- file.path(RES_DIR,   "sensitivity-k.csv")

invisible(TRUE)
