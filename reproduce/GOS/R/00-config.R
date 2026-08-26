# =============================================================================
# 00-config.R — resolve paths, load the user config, name every output file
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

# -- Result files, referenced by name from every step -------------------------
F_RESPONSE <- file.path(RES_DIR, "response-summary.csv")
F_VARSEL   <- file.path(RES_DIR, "variable-selection.csv")
F_KAPPA    <- file.path(RES_DIR, "kappa-rmse.csv")
F_GRID     <- file.path(RES_DIR, "grid-prediction.csv")
F_GRIDBCS  <- file.path(RES_DIR, "grid-prediction-bcs.csv")
F_MODELS   <- file.path(RES_DIR, "model-comparison.csv")
F_MODREP   <- file.path(RES_DIR, "model-comparison-repeats.csv")
F_MODELS_NOSCREEN <- file.path(RES_DIR, "model-comparison-no-outlier-screening.csv")

# -- Intermediates handed from one step to the next ---------------------------
D_SAMPLES  <- file.path(DERIVED, "samples.rds")     # screened, log-transformed
D_SELECTED <- file.path(DERIVED, "selected-vars.rds")
D_LAMBDA   <- file.path(DERIVED, "best-kappa.rds")

# -- Verbatim copies of the packaged inputs, written by step 10 ---------------
F_ZN_CSV   <- file.path(DATA_DIR, "zn-samples.csv")
F_GRID_CSV <- file.path(DATA_DIR, "grid-covariates.csv")

set.seed(SEED)
