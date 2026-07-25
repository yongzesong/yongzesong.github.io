# =============================================================================
# 00-config.R — resolve paths, load the user config, set up shared constants
# Sourced first by every step. Never edit project settings here; edit
# config/project-config.R instead.
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

for (d in c(DERIVED, RES_DIR, TAB_DIR, FIG_DIR, ENV_DIR))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(CFG_FILE))
  stop("Missing config/project-config.R — run from the project root.")
source(CFG_FILE)

# Result-file names, referenced by name from every step and figure script.
F_VARS        <- file.path(RES_DIR, "variable-summary.csv")
F_ANALYSIS    <- file.path(DERIVED, "analysis-data.csv")
F_DISC        <- file.path(RES_DIR, "optimal-discretization.csv")
F_DISC_CURVE  <- file.path(RES_DIR, "discretization-curves.csv")
F_FACTOR      <- file.path(RES_DIR, "factor-detector.csv")
F_INTERACT    <- file.path(RES_DIR, "interaction-detector.csv")
F_RISK        <- file.path(RES_DIR, "risk-detector.csv")
F_RISKMEAN    <- file.path(RES_DIR, "risk-means.csv")
F_ECO         <- file.path(RES_DIR, "ecological-detector.csv")
F_SESU        <- file.path(RES_DIR, "sesu-scale.csv")

# Fixed colours for the five discretisation methods (used across figures).
PALETTE_DISC <- c(equal = "#E15759", natural = "#59A14F", quantile = "#4E79A7",
                  geometric = "#59B3C4", sd = "#B07AA1", manual = "#9C755F")
PALETTE_SEQ  <- c("#FFF7EC", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000")

set.seed(SEED)
