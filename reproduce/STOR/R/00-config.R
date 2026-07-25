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

# The method itself. One file, base R only.
source(file.path(R_DIR, "02-stor-core.R"))

# Result files, referenced by name from every step and figure script.
F_BLOCKS     <- file.path(PROJ_ROOT, BLOCK_FILE)
F_TRUTH      <- file.path(DERIVED, "simulation-truth.csv")
F_WEIGHTS    <- file.path(RES_DIR, "entropy-weights.csv")
F_SRII       <- file.path(RES_DIR, "srii-blocks.csv")
F_UTILITY    <- file.path(RES_DIR, "utility-curve.csv")
F_BINS       <- file.path(RES_DIR, "utility-bins.csv")
F_BOUNDS     <- file.path(RES_DIR, "dmu-boundaries.csv")
F_STAGES     <- file.path(RES_DIR, "dmu-stages.csv")
F_LISA       <- file.path(RES_DIR, "lisa-clusters.csv")
F_TRADEOFF   <- file.path(RES_DIR, "tradeoff-groups.csv")
F_CONTRIB    <- file.path(RES_DIR, "income-contribution.csv")
F_GWRCOEF    <- file.path(RES_DIR, "gwr-coefficients.csv")

set.seed(SEED)
