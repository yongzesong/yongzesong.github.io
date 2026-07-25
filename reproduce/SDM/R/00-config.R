# =============================================================================
# 00-config.R — resolve paths, load the user config, set shared constants
# Sourced first by every step. Edit config/project-config.R, not this file.
# =============================================================================

PROJ_ROOT <- getwd()
R_DIR     <- file.path(PROJ_ROOT, "R")
CFG_FILE  <- file.path(PROJ_ROOT, "config", "project-config.R")
DERIVED   <- file.path(PROJ_ROOT, "data", "derived")
RES_DIR   <- file.path(PROJ_ROOT, "results")
TAB_DIR   <- file.path(PROJ_ROOT, "tables")
FIG_DIR   <- file.path(PROJ_ROOT, "figs")
ENV_DIR   <- file.path(PROJ_ROOT, "env")

for (d in c(DERIVED, RES_DIR, TAB_DIR, FIG_DIR, ENV_DIR))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(CFG_FILE))
  stop("Missing config/project-config.R — run from the project root.")
source(CFG_FILE)

F_SUMMARY   <- file.path(RES_DIR, "variable-summary.csv")
F_SCALES    <- file.path(RES_DIR, "walking-scales.csv")
F_DEMO      <- file.path(RES_DIR, "accessibility-demo.csv")
F_DELTA     <- file.path(RES_DIR, "delta-summary.csv")
F_CLASS     <- file.path(RES_DIR, "green-city-classification.csv")
F_REGION    <- file.path(RES_DIR, "delta-by-region.csv")
F_DRIVERS   <- file.path(RES_DIR, "drivers-pd.csv")
F_TOTAL     <- file.path(RES_DIR, "drivers-total-pd.csv")
F_INTERACT  <- file.path(RES_DIR, "interaction-pid.csv")

set.seed(SEED)
