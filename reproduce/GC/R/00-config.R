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
F_VARS      <- file.path(RES_DIR, "variable-summary.csv")
F_GC        <- file.path(RES_DIR, "geocomplexity.csv")
F_GCMETHOD  <- file.path(RES_DIR, "gc-method-comparison.csv")
F_GCCORR    <- file.path(RES_DIR, "gc-method-correlation.csv")
F_BASE      <- file.path(RES_DIR, "baseline-models.csv")
F_ERRORS    <- file.path(RES_DIR, "model-errors.csv")
F_EXPLAIN   <- file.path(RES_DIR, "error-explanation.csv")
F_EXPCOEF   <- file.path(RES_DIR, "error-explanation-coefficients.csv")
F_IMPROVE   <- file.path(RES_DIR, "model-improvement.csv")

set.seed(SEED)
