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

# The method itself. One file, no package dependencies.
source(file.path(R_DIR, "02-sda-core.R"))

# Result files, referenced by name from every step and figure script.
F_POINTS     <- file.path(DERIVED, "points.csv")
F_SDVARS     <- file.path(DERIVED, "sdvars-generated.csv")
F_GENCHECK   <- file.path(RES_DIR, "generation-check.csv")
F_SELECTED   <- file.path(RES_DIR, "selected-variables.csv")
F_SELPARAM   <- file.path(RES_DIR, "selected-parameters.csv")
F_CV         <- file.path(RES_DIR, "cross-validation.csv")
F_PRED       <- file.path(RES_DIR, "cv-predictions.csv")
F_BUFFER     <- file.path(RES_DIR, "buffer-sensitivity.csv")
F_TIMING     <- file.path(RES_DIR, "timing.csv")

set.seed(SEED)
