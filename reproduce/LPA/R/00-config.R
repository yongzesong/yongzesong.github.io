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

# Result files, referenced by name from every step and figure script.
F_POINTS    <- file.path(DERIVED, "points.rds")
F_VARS      <- file.path(RES_DIR, "variable-summary.csv")
F_KCURVE    <- file.path(RES_DIR, "ripley-k-curve.csv")
F_RANGE     <- file.path(RES_DIR, "optimal-range.csv")
F_LAMBDA    <- file.path(RES_DIR, "local-lambda-recomputed.csv")
F_FITLOG    <- file.path(RES_DIR, "local-fit-diagnostics.csv")
F_TABLE2    <- file.path(RES_DIR, "table2-published-reproduction.csv")
F_AGREE     <- file.path(RES_DIR, "recomputed-vs-published.csv")
F_SENS      <- file.path(RES_DIR, "radius-sensitivity.csv")

set.seed(SEED)
