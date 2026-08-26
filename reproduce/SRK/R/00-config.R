# =============================================================================
# 00-config.R — resolve paths, load the user config, set shared constants
# Sourced first by every step. Edit config/project-config.R, not this file.
# =============================================================================

PROJ_ROOT <- getwd()
R_DIR     <- file.path(PROJ_ROOT, "R")
CFG_FILE  <- file.path(PROJ_ROOT, "config", "project-config.R")
DATA_DIR  <- file.path(PROJ_ROOT, "data")
DERIVED   <- file.path(DATA_DIR, "derived")
RES_DIR   <- file.path(PROJ_ROOT, "results")
TAB_DIR   <- file.path(PROJ_ROOT, "tables")
FIG_DIR   <- file.path(PROJ_ROOT, "figs")
ENV_DIR   <- file.path(PROJ_ROOT, "env")

for (d in c(DATA_DIR, DERIVED, RES_DIR, TAB_DIR, FIG_DIR, ENV_DIR))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(CFG_FILE))
  stop("Missing config/project-config.R — run from the project root.")
source(CFG_FILE)

# The method itself. One file; gstat, automap and ranger do the heavy lifting.
source(file.path(R_DIR, "02-srk-core.R"))

# Result files, referenced by name from every step, figure script and test.
F_SIM_FIELDS  <- file.path(DERIVED,  "simulation-fields.csv")
F_SIM_RESULTS <- file.path(RES_DIR,  "simulation-metrics.csv")
F_SIM_DETAIL  <- file.path(RES_DIR,  "simulation-predictions.csv")
F_SIM_IMP     <- file.path(RES_DIR,  "simulation-importance.csv")
F_SAMPLES     <- function(e) file.path(DERIVED, sprintf("samples-%s.csv", e))
F_SCREEN      <- file.path(RES_DIR,  "covariate-screening.csv")
F_RESPONSE    <- file.path(RES_DIR,  "response-summary.csv")
F_SV          <- function(e) file.path(DERIVED, sprintf("singularity-%s.csv", e))
F_SV_DIAG     <- file.path(RES_DIR,  "singularity-diagnostics.csv")
F_CV_SUMMARY  <- file.path(RES_DIR,  "cv-summary.csv")
F_CV_FOLDS    <- file.path(RES_DIR,  "cv-folds.csv")
F_CV_PRED     <- file.path(RES_DIR,  "cv-predictions.csv")
F_IMPORTANCE  <- file.path(RES_DIR,  "rf-importance.csv")
F_VARIOGRAM   <- file.path(RES_DIR,  "residual-variogram.csv")
F_SEEDS       <- file.path(RES_DIR,  "seed-stability.csv")
F_SEEDS_PAIR  <- file.path(RES_DIR,  "seed-stability-pairs.csv")
F_SENS        <- file.path(RES_DIR,  "sensitivity.csv")
F_RUNTIMES    <- file.path(ENV_DIR,  "runtimes.csv")

set.seed(SEED)
